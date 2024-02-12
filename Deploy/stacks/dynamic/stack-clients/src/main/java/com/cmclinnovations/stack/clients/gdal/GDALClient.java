package com.cmclinnovations.stack.clients.gdal;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.time.ZonedDateTime;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.StringJoiner;
import java.util.stream.Collectors;

import org.json.JSONArray;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.cmclinnovations.stack.clients.core.EndpointNames;
import com.cmclinnovations.stack.clients.core.StackClient;
import com.cmclinnovations.stack.clients.docker.ContainerClient;
import com.cmclinnovations.stack.clients.geoserver.GeoServerClient;
import com.cmclinnovations.stack.clients.geoserver.MultidimSettings;
import com.cmclinnovations.stack.clients.postgis.PostGISClient;
import com.cmclinnovations.stack.clients.postgis.PostGISEndpointConfig;
import com.cmclinnovations.stack.clients.utils.DateTimeParser;
import com.cmclinnovations.stack.clients.utils.FileUtils;
import com.cmclinnovations.stack.clients.utils.TempDir;
import com.google.common.collect.ArrayListMultimap;
import com.google.common.collect.Multimap;

/**
 * Contains methods to run gdal commands for transforming and uploading raster
 * and vector data
 */
public class GDALClient extends ContainerClient {

    private static final String GDAL = "gdal";

    private static final String GEOSERVER = "geoserver";

    private static final String POSTGIS = "postgis";

    private static final String GDALSRSINFO = "gdalsrsinfo";

    private static final Logger logger = LoggerFactory.getLogger(GDALClient.class);

    private final PostGISEndpointConfig postgreSQLEndpoint;

    private static GDALClient instance = null;

    public static GDALClient getInstance() {
        if (null == instance) {
            instance = new GDALClient();
        }
        return instance;
    }

    private GDALClient() {
        postgreSQLEndpoint = readEndpointConfig(EndpointNames.POSTGIS, PostGISEndpointConfig.class);
    }

    private String computePGSQLSourceString(String database) {
        return "PG:dbname=" + database + " host=" + postgreSQLEndpoint.getHostName()
                + " port=" + postgreSQLEndpoint.getPort() + " user=" + postgreSQLEndpoint.getUsername()
                + " password=" + postgreSQLEndpoint.getPassword();
    }

    public void uploadVectorStringToPostGIS(String database, String layerName, String fileContents,
            Ogr2OgrOptions options, boolean append) {

        try (TempDir tmpDir = makeLocalTempDir()) {
            Path filePath = tmpDir.getPath().resolve(layerName);
            try {
                Files.writeString(filePath, fileContents);
                uploadVectorToPostGIS(database, layerName, filePath.toString(), options, append);
            } catch (IOException ex) {
                throw new RuntimeException("Failed to write string for vector '" + layerName
                        + "' layer to a file in a temporary directory.", ex);
            }
        }
    }

    public void uploadVectorFilesToPostGIS(String database, String layerName, String dirPath, Ogr2OgrOptions options,
            boolean append) {
        try (TempDir tmpDir = makeLocalTempDir()) {
            tmpDir.copyFrom(Path.of(dirPath));
            String gdalContainerId = getContainerId(GDAL);
            Multimap<String, String> foundGeoFiles = findGeoFiles(gdalContainerId, tmpDir.toString());
            for (Collection<String> filesOfType : foundGeoFiles.asMap().values()) {
                for (String filePath : filesOfType) {
                    uploadVectorToPostGIS(database, layerName, filePath, options, append);
                    // If inserting multiple sources into a single layer then ensure subsequent
                    // files are appended.
                    if (null != layerName) {
                        append = true;
                    }
                }
            }
        }
    }

    public void uploadVectorFileToPostGIS(String database, String layerName, String filePath, Ogr2OgrOptions options,
            boolean append) {

        try (TempDir tmpDir = makeLocalTempDir()) {
            Path sourcePath = Path.of(filePath);
            tmpDir.copyFrom(sourcePath);
            uploadVectorToPostGIS(database, layerName, tmpDir.getPath().resolve(sourcePath.getFileName()).toString(),
                    options, append);
        }
    }

    public void uploadVectorURLToPostGIS(String database, String layerName, String url, Ogr2OgrOptions options,
            boolean append) {
        uploadVectorToPostGIS(database, layerName, url, options, append);
    }

    private void uploadVectorToPostGIS(String database, String layerName, String filePath, Ogr2OgrOptions options,
            boolean append) {

        String containerId = getContainerId(GDAL);

        ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
        ByteArrayOutputStream errorStream = new ByteArrayOutputStream();

        String execId = createComplexCommand(containerId, options.appendToArgs(layerName, "ogr2ogr",
                "-f", "PostgreSQL",
                computePGSQLSourceString(database),
                filePath,
                "--config", "OGR_TRUNCATE", append ? "NO" : "YES",
                "--config", "PG_USE_COPY", "YES"))
                .withOutputStream(outputStream)
                .withErrorStream(errorStream)
                .withEnvVars(options.getEnv())
                .withEvaluationTimeout(300)
                .exec();
        handleErrors(errorStream, execId, logger);
    }

    public void uploadRasterFilesToPostGIS(String database, String schema, String layerName,
            String dirPath, GDALTranslateOptions gdalOptions, MultidimSettings mdimSettings, boolean append) {

        String gdalContainerId = getContainerId(GDAL);
        String postGISContainerId = getContainerId(POSTGIS);

        try (TempDir tempDir = makeLocalTempDir()) {

            tempDir.copyFrom(Path.of(dirPath));
            List<String> postgresFiles = convertRastersToGeoTiffs(gdalContainerId, database, schema, layerName, tempDir,
                    gdalOptions, mdimSettings);

            ensurePostGISRasterSupportEnabled(postGISContainerId, database);
            uploadRasters(postGISContainerId, database, layerName, postgresFiles, append);
        }
    }

    private Multimap<String, String> findGeoFiles(String containerId, String dirPath) {
        ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
        ByteArrayOutputStream errorStream = new ByteArrayOutputStream();
        String execId = createComplexCommand(containerId, "gdalmanage", "identify", "-r", dirPath)
                .withOutputStream(outputStream)
                .withErrorStream(errorStream)
                .exec();
        handleErrors(errorStream, execId, logger);

        return outputStream.toString().lines()
                .map(entry -> entry.split(": "))
                .collect(ArrayListMultimap::create,
                        (m, pair) -> m.put(pair[1], pair[0]),
                        Multimap::putAll);
    }

    private void addCustomCRStoPostGis(String geoserverContainerID, String postGISContainerId, String gdalContainerId,
            String filePath, String databaseName, String newSrid) {

        String detectedSrid = getDetectedSrid(gdalContainerId, filePath);

        if (detectedSrid.equals("EPSG:-1")) {
            logger.info("Unknown CRS detected, adding custom projection to postGIS and GeoServer");

            String proj4String = getProj4String(gdalContainerId, filePath);
            String wktString = getWktString(gdalContainerId, filePath);

            String[] sridAuthNameArray;
            try {
                sridAuthNameArray = newSrid.split(":");
                String authName = sridAuthNameArray[0];
                String srid = sridAuthNameArray[1];
                PostGISClient.getInstance().addProjectionsToPostgis(postGISContainerId, databaseName, proj4String,
                        wktString,
                        authName, srid);
                GeoServerClient.getInstance().addProjectionsToGeoserver(geoserverContainerID, wktString, srid);
            } catch (NullPointerException ex) {
                throw new RuntimeException(
                        "Custom CRS not specified, add \"sridOut\": \"<AUTH>:<123456>\" to gdalTranslateOptions", ex);
            }
        }
    }

    private String getDetectedSrid(String gdalContainerId, String filePath) {
        ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
        ByteArrayOutputStream errorStream = new ByteArrayOutputStream();
        String execId = createComplexCommand(gdalContainerId, GDALSRSINFO, "-o", "epsg", filePath)
                .withOutputStream(outputStream)
                .withErrorStream(errorStream)
                .exec();
        handleErrors(errorStream, execId, logger);
        return outputStream.toString().replace("\n", "");
    }

    private String getProj4String(String gdalContainerId, String filePath) {
        ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
        ByteArrayOutputStream errorStream = new ByteArrayOutputStream();
        String execId = createComplexCommand(gdalContainerId, GDALSRSINFO, "-o", "proj4", filePath)
                .withOutputStream(outputStream)
                .withErrorStream(errorStream)
                .exec();
        handleErrors(errorStream, execId, logger);
        return outputStream.toString().replace("\n", "");
    }

    private String getWktString(String gdalContainerId, String filePath) {
        ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
        ByteArrayOutputStream errorStream = new ByteArrayOutputStream();
        String execId = createComplexCommand(gdalContainerId, GDALSRSINFO, "-o", "wkt", "--single-line", filePath)
                // This will get either wkt1 or wkt2 whichever exists. Other options exist
                // instead of "wkt": {wkt_all, wkt1, wkt_simple, wkt_noct, wkt_esri, wkt2,
                // wkt2_2015, wkt2_2018}).withOutputStream(outputStream)

                .withOutputStream(outputStream)
                .withErrorStream(errorStream)
                .exec();
        handleErrors(errorStream, execId, logger);
        return outputStream.toString();
    }

    private JSONArray getTimeFromGdalmdiminfo(String timeArrayName, String filePath) {
        String gdalContainerId = getContainerId(GDAL);
        ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
        ByteArrayOutputStream errorStream = new ByteArrayOutputStream();
        String execId = createComplexCommand(gdalContainerId, "gdalmdiminfo", "-detailed", "-array", timeArrayName,
                filePath)
                .withOutputStream(outputStream)
                .withErrorStream(errorStream)
                .exec();
        handleErrors(errorStream, execId, logger);

        String inputString = outputStream.toString().replace(" ", "");
        return new JSONObject(inputString).getJSONArray("values");
    }

    private void multipleRastersFromMultiDim(MultidimSettings mdimSettings, String filePath, Path outputDirectory,
            String database, String layername) {

        String timeArrayName = mdimSettings.getTimeOptions().getArrayName();
        String variableArrayName = mdimSettings.getLayerArrayName();
        String dateTimeFormat = mdimSettings.getTimeOptions().getFormat();
        String timeZone = mdimSettings.getTimeOptions().getTimeZone();
        JSONArray arrayList = getTimeFromGdalmdiminfo(timeArrayName, filePath); // to generate output filenames
        String gdalContainerId = getContainerId(GDAL);
        String postGISContainerId = getContainerId(POSTGIS);
        StringJoiner dateTimes = new StringJoiner("'),('", "('", "')"); // SQL will want ('value1'),...,('valueN')
        ByteArrayOutputStream errorStream = new ByteArrayOutputStream();

        DateTimeParser dateTimeParser = new DateTimeParser(dateTimeFormat, timeZone);

        for (int index = 0; index < arrayList.length(); index++) {
            String outputRasterFilePath = outputDirectory
                    .resolve(variableArrayName + "_" + arrayList.getString(index) + ".tif").toString();

            // Convert the time from "dateTimeFormat" format to a format suitable for
            // PostGIS
            ZonedDateTime zonedDateTime = dateTimeParser.parse(arrayList.getString(index));
            dateTimes.add(zonedDateTime.toInstant().toString());

            String execId = createComplexCommand(gdalContainerId, "gdalwarp", "-srcband", Integer.toString(index + 1),
                    "-t_srs", "EPSG:4326", "-r", "cubicspline", "-wo", "OPTIMIZE_SIZE=YES", "-multi", "-wo",
                    "NUM_THREADS=ALL_CPUS", "NETCDF:" + filePath + ":" + variableArrayName,
                    outputRasterFilePath)
                    .withErrorStream(errorStream)
                    .exec();
            handleErrors(errorStream, execId, logger);
            errorStream.reset();
        }

        String hereDocument = "CREATE TABLE IF NOT EXISTS " + layername
                + "_times (bands SERIAL, time TIMESTAMPTZ PRIMARY KEY); " +
                "INSERT INTO " + layername + "_times (time) VALUES " + dateTimes.toString() + ";";
        String execId = createComplexCommand(postGISContainerId,
                "psql", "-U", postgreSQLEndpoint.getUsername(), "-d", database, "-w")
                .withHereDocument(hereDocument)
                .withErrorStream(errorStream)
                .exec();
        handleErrors(errorStream, execId, logger);
        errorStream.reset();
    }

    private List<String> convertRastersToGeoTiffs(String gdalContainerId, String databaseName, String schemaName,
            String layerName, TempDir tempDir, GDALTranslateOptions options, MultidimSettings mdimSettings) {

        Multimap<String, String> foundRasterFiles = findGeoFiles(gdalContainerId, tempDir.toString());
        Set<Path> createdDirectories = new HashSet<>();
        List<String> postgresFiles = new ArrayList<>();

        String geoserverContainerId = getContainerId(GEOSERVER);
        String postGISContainerId = getContainerId(POSTGIS);

        for (Map.Entry<String, Collection<String>> fileTypeEntry : foundRasterFiles.asMap().entrySet()) {
            String inputFormat = fileTypeEntry.getKey();
            for (String filePath : fileTypeEntry.getValue()) {

                addCustomCRStoPostGis(geoserverContainerId, postGISContainerId, gdalContainerId, filePath, databaseName,
                        options.getSridOut());

                postgresFiles.add(processFile(gdalContainerId, inputFormat, filePath, databaseName, schemaName,
                        layerName, tempDir, options, mdimSettings, createdDirectories));
            }
        }
        createdDirectories.forEach(
                directoryPath -> executeSimpleCommand(gdalContainerId, "chmod", "-R", "777", directoryPath.toString()));
        return postgresFiles;
    }

    private String processFile(String gdalContainerId, String inputFormat, String filePath,
            String databaseName, String schemaName, String layerName, TempDir tempDir,
            GDALTranslateOptions options, MultidimSettings mdimSettings, Set<Path> createdDirectories) {

        String postgresOutputPath;
        String geotiffsOutputPath = generateOutFilePath(tempDir.toString(), databaseName, schemaName, layerName,
                filePath, "geotiffs");
        Path geotiffsOutputDirectory = Paths.get(geotiffsOutputPath).getParent();

        List<Path> directoryPaths = new ArrayList<>();
        directoryPaths.add(geotiffsOutputDirectory);

        if (inputFormat.equals("netCDF")) {
            postgresOutputPath = generateOutFilePath(tempDir.toString(), databaseName, schemaName, layerName,
                    filePath, "multidim_geospatial");
            Path directoryPath = Paths.get(postgresOutputPath).getParent();
            directoryPaths.add(directoryPath);
        } else {
            postgresOutputPath = geotiffsOutputPath;
        }

        for (Path dirPath : directoryPaths) {
            if (!createdDirectories.contains(dirPath)) {
                makeDir(gdalContainerId, dirPath.toString());
                executeSimpleCommand(gdalContainerId, "chmod", "-R", "777", dirPath.toString());
                createdDirectories.add(dirPath);
            }
        }

        ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
        ByteArrayOutputStream errorStream = new ByteArrayOutputStream();

        String execId;
        if (inputFormat.equals("netCDF")) {
            logger.info("netCDF found, uploading witt translate");
            execId = createComplexCommand(gdalContainerId, "cp",
                    filePath,
                    postgresOutputPath)
                    .withOutputStream(outputStream)
                    .withErrorStream(errorStream)
                    .withEvaluationTimeout(300)
                    .exec();
            handleErrors(errorStream, execId, logger);
            multipleRastersFromMultiDim(mdimSettings, filePath, geotiffsOutputDirectory, databaseName, layerName);
        } else {
            execId = createComplexCommand(gdalContainerId, options.appendToArgs("gdal_translate",
                    "-if", inputFormat,
                    // https://gdal.org/drivers/raster/cog.html#raster-cog
                    "-of", "COG",
                    filePath,
                    postgresOutputPath))
                    .withOutputStream(outputStream)
                    .withErrorStream(errorStream)
                    .withEnvVars(options.getEnv())
                    .withEvaluationTimeout(300)
                    .exec();
            handleErrors(errorStream, execId, logger);
        }

        return postgresOutputPath;
    }

    private void ensurePostGISRasterSupportEnabled(String postGISContainerId, String database) {
        ByteArrayOutputStream errorStream = new ByteArrayOutputStream();
        String execId = createComplexCommand(postGISContainerId,
                "psql", "-U", postgreSQLEndpoint.getUsername(), "-d", database, "-w")
                .withHereDocument("CREATE EXTENSION IF NOT EXISTS postgis_raster;" +
                        "ALTER DATABASE \"" + database + "\" SET postgis.enable_outdb_rasters = True;" +
                        "ALTER DATABASE \"" + database + "\" SET postgis.gdal_enabled_drivers = 'GTiff netCDF';")
                .withErrorStream(errorStream)
                .exec();
        handleErrors(errorStream, execId, logger);
    }

    private void uploadRasters(String postGISContainerId, String database, String layerName,
            List<String> geotiffFiles, boolean append) {

        ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
        ByteArrayOutputStream errorStream = new ByteArrayOutputStream();
        String mode = append ? "-a" : "-d";
        String execId = createComplexCommand(postGISContainerId, "bash", "-c",
                "(which raster2pgsql || (apt update && apt install -y postgis && rm -rf /var/lib/apt/lists/*)) && " +
                // https://postgis.net/docs/using_raster_dataman.html#RT_Raster_Loader
                        "raster2pgsql " + mode + " -C -t auto -R -F -I -M -Y"
                        + geotiffFiles.stream().collect(Collectors.joining("' '", " '", "' "))
                        + layerName
                        + " | psql -U " + postgreSQLEndpoint.getUsername() + " -d " + database + " -w")
                .withOutputStream(outputStream)
                .withErrorStream(errorStream)
                .withEvaluationTimeout(300)
                .exec();

        handleErrors(errorStream, execId, logger);
    }

    // add .tif extension on files in geotiffs directory

    // return filePath for any file to either "geotiffs" or "multidim_geospatial"
    private static String generateOutFilePath(String basePathIn, String databaseName, String schemaName,
            String layerName, String filePath, String destinationDirectory) {
        if (destinationDirectory.equals("multidim_geospatial")) {
            // the Path object of multidim_geospatial
            Path multiDimOutDirPath = Path.of(StackClient.MULTIDIM_GEOSPATIAL_DIR, databaseName, schemaName, layerName);
            return multiDimOutDirPath.resolve(Path.of(basePathIn).relativize(Path.of(filePath)))
                    .toString();
        } else {
            // alternative should be destinationDirectory.equals("geotiffs"), and this shall
            // be default
            // returns the Path object of geotiffs
            Path rasterOutDirPath = Path.of(StackClient.GEOTIFFS_DIR, databaseName, schemaName, layerName);
            String rasterOutFilePath = rasterOutDirPath.resolve(Path.of(basePathIn).relativize(Path.of(filePath)))
                    .toString();
            return FileUtils.replaceExtension(rasterOutFilePath, ".tif");

        }
    }

}
