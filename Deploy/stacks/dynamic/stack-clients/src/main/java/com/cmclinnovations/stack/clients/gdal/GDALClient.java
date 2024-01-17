package com.cmclinnovations.stack.clients.gdal;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
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
import com.cmclinnovations.stack.clients.utils.FileUtils;
import com.cmclinnovations.stack.clients.utils.TempDir;
import com.google.common.collect.ArrayListMultimap;
import com.google.common.collect.Multimap;

public class GDALClient extends ContainerClient {

    /**
     *
     */
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
            String gdalContainerId = getContainerId("gdal");
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

        String containerId = getContainerId("gdal");

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

        String gdalContainerId = getContainerId("gdal");
        String postGISContainerId = getContainerId("postgis");

        try (TempDir tempDir = makeLocalTempDir()) {

            tempDir.copyFrom(Path.of(dirPath));

            List<String> geotiffFiles = convertRastersToGeoTiffs(gdalContainerId, database, schema, layerName, tempDir, gdalOptions, mdimSettings);

            ensurePostGISRasterSupportEnabled(postGISContainerId, database);

            uploadRasters(postGISContainerId, database, layerName, geotiffFiles, append);
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
            } catch (NullPointerException e) {
                logger.error("Custom CRS not specified, add \"sridOut\": \"AUTH:123456\" to gdalTranslateOptions");
                e.printStackTrace();
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
        String execId = createComplexCommand(gdalContainerId, GDALSRSINFO, "-o", "wkt", "--single-line", filePath) // This
                                                                                                                   // will
                                                                                                                   // get
                                                                                                                   // either
                                                                                                                   // wkt1
                                                                                                                   // or
                                                                                                                   // wkt2
                                                                                                                   // whichever
                                                                                                                   // exists.Other
                                                                                                                   // options
                                                                                                                   // exist
                                                                                                                   // instead
                                                                                                                   // of"wkt"
                                                                                                                   // :{wkt_all,wkt1,wkt_simple,wkt_noct,wkt_esri,wkt2,
                                                                                                                   // wkt2_2015,
                                                                                                                   // wkt2_2018})
                .withOutputStream(outputStream)
                .withErrorStream(errorStream)
                .exec();
        handleErrors(errorStream, execId, logger);
        return outputStream.toString();
    }

    private JSONArray getTimeFromGdalmdiminfo(String timeArrayName, String filePath) {
        String gdalContainerId = getContainerId("gdal");
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

    private void multipleRastersFromMultiDim(String timeArrayName, String variableArrayName, String filePath,
            Path outputDirectory) {
        JSONArray arrayList = getTimeFromGdalmdiminfo(timeArrayName, filePath); // to generate output filenames
        String gdalContainerId = getContainerId("gdal");

        ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
        ByteArrayOutputStream errorStream = new ByteArrayOutputStream();

        for (int index = 0; index < arrayList.length(); index++) {
            String outputRasterFilePath = outputDirectory.resolve(variableArrayName + "_" + arrayList.getString(index) + ".tif").toString();
            String execId = createComplexCommand(gdalContainerId, "gdalwarp", "-srcband", Integer.toString(index + 1),
                    "-t_srs", "EPSG:4326", "-r", "cubicspline", "-wo", "OPTIMIZE_SIZE=YES", "-multi", "-wo",
                    "NUM_THREADS=ALL_CPUS", "NETCDF:" + filePath + ":" + variableArrayName,
                    outputRasterFilePath)
                    .withOutputStream(outputStream)
                    .withErrorStream(errorStream)
                    .exec();
            handleErrors(errorStream, execId, logger);
            outputStream.reset();
            errorStream.reset();// reset
        }

        // gdalwarp -b 1 -t_srs EPSG:4326 -r cubicspline -wo OPTIMIZE_SIZE=YES -multi
        // -wo NUM_THREADS=ALL_CPUS
        // input: NETCDF:tas_rcp85_land-cpm_uk_2.2km_01_1hr_20400101-20400130.nc:tas
        // tas_rcp85_land-cpm_uk_2.2km_01_1hr_20400101-20400130.tif
    }

    private List<String> convertRastersToGeoTiffs(String gdalContainerId, String databaseName, String schemaName,
            String layerName, TempDir tempDir, GDALTranslateOptions options, MultidimSettings mdimSettings) {

        Multimap<String, String> foundRasterFiles = findGeoFiles(gdalContainerId, tempDir.toString());

        ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
        ByteArrayOutputStream errorStream = new ByteArrayOutputStream();

        Set<Path> createdDirectories = new HashSet<>();

        List<String> geotiffFiles = new ArrayList<>();
        String geoserverContainerId = getContainerId("geoserver");

        for (Map.Entry<String, Collection<String>> fileTypeEntry : foundRasterFiles.asMap().entrySet()) {
            String inputFormat = fileTypeEntry.getKey();
            for (String filePath : fileTypeEntry.getValue()) {

                String outputPath;
                if (inputFormat.equals("netCDF")) {
                    outputPath = generateOutFilePath(tempDir.toString(), databaseName, schemaName, layerName, filePath);
                } else {
                    outputPath = generateRasterOutFilePath(tempDir.toString(), databaseName, schemaName, layerName,
                            filePath);
                }
                geotiffFiles.add(outputPath);
                String postGISContainerId = getContainerId("postgis");
                addCustomCRStoPostGis(geoserverContainerId, postGISContainerId, gdalContainerId, filePath, databaseName,
                        options.getSridOut());
                Path directoryPath = Paths.get(outputPath).getParent();
                if (!createdDirectories.contains(directoryPath)) {
                    makeDir(gdalContainerId, directoryPath.toString());
                    executeSimpleCommand(gdalContainerId, "chmod", "-R", "777", directoryPath.toString());
                    createdDirectories.add(directoryPath);
                }
                String execId;
                if (inputFormat.equals("netCDF")) {
                    logger.info("netCDF found, uploading without translate");
                    execId = createComplexCommand(gdalContainerId, "cp",
                            filePath,
                            outputPath)
                            // TODO make sure that outputpath is set here and doesn't override the location of geotiffs
                            .withOutputStream(outputStream)
                            .withErrorStream(errorStream)
                            .withEvaluationTimeout(300)
                            .exec();
                    handleErrors(errorStream, execId, logger);
                    multipleRastersFromMultiDim(mdimSettings.getTimeOptions().getArrayName(), mdimSettings.getLayerArrayName(), filePath, directoryPath);
                } else {
                    execId = createComplexCommand(gdalContainerId, options.appendToArgs("gdal_translate",
                            "-if", inputFormat,
                            // https://gdal.org/drivers/raster/cog.html#raster-cog
                            "-of", "COG",
                            filePath,
                            outputPath))
                            .withOutputStream(outputStream)
                            .withErrorStream(errorStream)
                            .withEnvVars(options.getEnv())
                            .withEvaluationTimeout(300)
                            .exec();
                    handleErrors(errorStream, execId, logger);
                }
            }
        }

        createdDirectories.forEach(
                directoryPath -> executeSimpleCommand(gdalContainerId, "chmod", "-R", "777", directoryPath.toString()));

        return geotiffFiles;
    }

    private void ensurePostGISRasterSupportEnabled(String postGISContainerId, String database) {
        ByteArrayOutputStream errorStream = new ByteArrayOutputStream();
        String execId = createComplexCommand(postGISContainerId,
                "psql", "-U", postgreSQLEndpoint.getUsername(), "-d", database, "-w")
                .withHereDocument("CREATE EXTENSION IF NOT EXISTS postgis_raster;" +
                        "ALTER DATABASE \"" + database + "\" SET postgis.enable_outdb_rasters = True;" +
                        "ALTER DATABASE \"" + database + "\" SET postgis.gdal_enabled_drivers = 'GTiff';")
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

    public static String generateRasterOutFilePath(String basePathIn, String databaseName, String schemaName,
            String layerName,
            String filePath) {
        return FileUtils.replaceExtension(
                generateOutFilePath(basePathIn, databaseName, schemaName, layerName, filePath),
                ".tif");
    }

    private static String generateOutFilePath(String basePathIn, String databaseName, String schemaName,
            String layerName,
            String filePath) {
        if (filePath.endsWith(".nc")) {
            return generateMultiDimOutDirPath(databaseName, schemaName, layerName)
                    .resolve(Path.of(basePathIn).relativize(Path.of(filePath)))
                    .toString();
        } else {
            return generateRasterOutDirPath(databaseName, schemaName, layerName)
                    .resolve(Path.of(basePathIn).relativize(Path.of(filePath)))
                    .toString();
        }
    }

    public static Path generateRasterOutDirPath(String databaseName, String schemaName, String layerName) {
        return Path.of(StackClient.GEOTIFFS_DIR, databaseName, schemaName, layerName);
    }

    public static Path generateMultiDimOutDirPath(String databaseName, String schemaName, String layerName) {
        return Path.of(StackClient.MULTIDIM_GEOSPATIAL_DIR, databaseName, schemaName, layerName);
    }
}
