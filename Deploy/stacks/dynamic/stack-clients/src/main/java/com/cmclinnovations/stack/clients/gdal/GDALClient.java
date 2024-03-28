package com.cmclinnovations.stack.clients.gdal;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.time.ZonedDateTime;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import java.util.Set;
import java.util.StringJoiner;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.json.JSONArray;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.cmclinnovations.stack.clients.core.EndpointNames;
import com.cmclinnovations.stack.clients.core.StackClient;
import com.cmclinnovations.stack.clients.docker.ContainerClient;
import com.cmclinnovations.stack.clients.geoserver.GeoServerClient;
import com.cmclinnovations.stack.clients.geoserver.MultidimSettings;
import com.cmclinnovations.stack.clients.geoserver.TimeOptions;
import com.cmclinnovations.stack.clients.postgis.PostGISClient;
import com.cmclinnovations.stack.clients.postgis.PostGISEndpointConfig;
import com.cmclinnovations.stack.clients.utils.DateStringFormatter;
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

    private List<String> multipleGeoTiffRastersFromMultiDim(MultidimSettings mdimSettings, String filePath,
            Path outputDirectory, String layerName, JSONArray timeArray) {

        String variableArrayName = mdimSettings.getLayerArrayName();
        String dateTimeFormat = mdimSettings.getTimeOptions().getFormat();

        String gdalContainerId = getContainerId(GDAL);

        ByteArrayOutputStream errorStream = new ByteArrayOutputStream();

        List<String> filenames = new ArrayList<>(timeArray.length());

        String inputRasterFilePath = "NETCDF:" + filePath + ":" + variableArrayName;

        for (int index = 0; index < timeArray.length(); index++) {

            String filename;
            if (null != dateTimeFormat) {
                filename = variableArrayName + "_" + timeArray.getString(index) + ".tif";
            } else {
                filename = variableArrayName + "_" + index + ".tif";
            }
            filenames.add(filename);
            String outputRasterFilePath = outputDirectory.resolve(filename).toString();

            String execId = createComplexCommand(gdalContainerId, "gdalwarp",
                    "-srcband", Integer.toString(index + 1),
                    "-t_srs", "EPSG:4326",
                    "-r", "cubicspline",
                    "-wo", "OPTIMIZE_SIZE=YES",
                    "-multi",
                    "-wo", "NUM_THREADS=ALL_CPUS",
                    inputRasterFilePath,
                    outputRasterFilePath)
                    .withErrorStream(errorStream)
                    .exec();
            handleErrors(errorStream, execId, logger);
            errorStream.reset();
        }

        return filenames;
    }

    private String getRasterTimeSqlType(String dateTimeFormat) {
        return (null != dateTimeFormat) ? "TIMESTAMPTZ" : "TEXT";
    }

    private String getRasterTimeSQLValues(String dateTimeFormat, String timeZone, JSONArray timeArray, String arrayName,
            Map<String, Integer> postgresOutputPathsAndNBands) {
        StringJoiner values = new StringJoiner(","); // SQL needs row1,...,rowN

        Iterator<String> filenames = postgresOutputPathsAndNBands.entrySet().stream().sequential()
                .flatMap(entry -> Collections
                        .nCopies(entry.getValue(), "'" + Paths.get(entry.getKey()).getFileName() + "'").stream())
                .collect(Collectors.toList()).iterator();

        Iterator<String> bands = postgresOutputPathsAndNBands.values().stream().sequential()
                .flatMap(nBands -> Stream.iterate(1, n -> n + 1).limit(nBands).map(band -> "'" + band + "'"))
                .collect(Collectors.toList()).iterator();

        ArrayList<String> dateTimesLists = new ArrayList<>();
        ArrayList<String> labelList = new ArrayList<>();
        if (null != dateTimeFormat) {
            DateTimeParser dateTimeParser = new DateTimeParser(dateTimeFormat, timeZone);
            for (int index = 0; index < timeArray.length(); index++) {
                // Convert the time from "dateTimeFormat" format to a format suitable for
                // PostGIS
                ZonedDateTime zonedDateTime = dateTimeParser.parse(timeArray.getString(index));
                String datetime = "'" + zonedDateTime.toInstant().toString() + "'";
                dateTimesLists.add(datetime);
                labelList.add(datetime);
            }
        } else {
            for (int index = 0; index < timeArray.length(); index++) {
                String timeStringUnFormatted = timeArray.getString(index);
                String timeStringFormatted = "'"
                        + DateStringFormatter.customDateStringFormatter(timeStringUnFormatted, arrayName) + "'";
                dateTimesLists.add("lastval()::text");
                labelList.add(timeStringFormatted);
            }
        }
        ListIterator<String> dateTimes = dateTimesLists.listIterator();
        ListIterator<String> labels = labelList.listIterator();

        while (filenames.hasNext() && bands.hasNext() && dateTimes.hasNext() && labels.hasNext()) {
            StringJoiner row = new StringJoiner(",", "(", ")"); // SQL needs ('col1',...,'colM')
            row.add("DEFAULT");
            row.add(filenames.next());
            row.add(bands.next());
            row.add(dateTimes.next());
            row.add(labels.next());

            values.add(row.toString());
        }
        return values.toString();
    }

    private void createRasterTimesTable(String database, String layerName,
            Map<String, Integer> postgresOutputPathsAndNBands, JSONArray timeArray, TimeOptions timeOptions) {

        String dateTimeFormat = timeOptions.getFormat();
        String timeZone = timeOptions.getTimeZone();
        String arrayName = timeOptions.getArrayName();

        String postGISContainerId = getContainerId(POSTGIS);

        String timeSqlType = getRasterTimeSqlType(dateTimeFormat);

        String dataTimeSQLValues = getRasterTimeSQLValues(dateTimeFormat, timeZone, timeArray, arrayName,
                postgresOutputPathsAndNBands);

        ByteArrayOutputStream errorStream = new ByteArrayOutputStream();

        String hereDocument = "CREATE TABLE IF NOT EXISTS \"" + layerName
                + "_times\" (\"index\" SERIAL, \"filename\" text, \"band\" integer, \"time\" " + timeSqlType
                + " CONSTRAINT time_key PRIMARY KEY, \"label\" text); "
                + "INSERT INTO \"" + layerName + "_times\" VALUES " + dataTimeSQLValues + ";";
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

                if (null == options.getSridIn()) {
                    addCustomCRStoPostGis(geoserverContainerId, postGISContainerId, gdalContainerId, filePath,
                            databaseName, options.getSridOut());
                }

                postgresFiles.addAll(processFile(gdalContainerId, inputFormat, filePath, databaseName, schemaName,
                        layerName, tempDir, options, mdimSettings, createdDirectories));
            }
        }
        createdDirectories.forEach(
                directoryPath -> executeSimpleCommand(gdalContainerId, "chmod", "-R", "777", directoryPath.toString()));
        return postgresFiles;
    }

    private Collection<String> processFile(String gdalContainerId, String inputFormat, String filePath,
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
            if (createdDirectories.add(dirPath)) {
                makeDir(gdalContainerId, dirPath.toString());
                executeSimpleCommand(gdalContainerId, "chmod", "-R", "777", dirPath.toString());
            }
        }

        Collection<String> postgresOutputPaths;
        if (inputFormat.equals("netCDF")) {
            logger.info("netCDF found, uploading without translate and creating gdal virtual format .vrt file");
            copyMultiDimRasters(gdalContainerId, filePath, postgresOutputPath);

            String timeArrayName = mdimSettings.getTimeOptions().getArrayName();
            JSONArray timeArray = getTimeFromGdalmdiminfo(timeArrayName, filePath);

            List<String> geoTiffFilenames = multipleGeoTiffRastersFromMultiDim(mdimSettings, filePath,
                    geotiffsOutputDirectory, layerName, timeArray);

            Map<String, Integer> postgresOutputPathsAndNBands = multipleVrtRastersFromMultiDim(gdalContainerId,
                    mdimSettings, postgresOutputPath,
                    geoTiffFilenames);

            createRasterTimesTable(databaseName, layerName, postgresOutputPathsAndNBands, timeArray,
                    mdimSettings.getTimeOptions());

            postgresOutputPaths = postgresOutputPathsAndNBands.keySet();
        } else {
            postgresOutputPaths = generateGeoTiffRaster(gdalContainerId, inputFormat, filePath, postgresOutputPath,
                    options);
        }

        return postgresOutputPaths;
    }

    private List<String> generateGeoTiffRaster(String gdalContainerId, String inputFormat, String filePath,
            String postgresOutputPath, GDALTranslateOptions options) {

        ByteArrayOutputStream errorStream = new ByteArrayOutputStream();
        String execId = createComplexCommand(gdalContainerId, options.appendToArgs("gdal_translate",
                "-if", inputFormat,
                // https://gdal.org/drivers/raster/cog.html#raster-cog
                "-of", "COG",
                filePath,
                postgresOutputPath))
                .withErrorStream(errorStream)
                .withEnvVars(options.getEnv())
                .withEvaluationTimeout(300)
                .exec();
        handleErrors(errorStream, execId, logger);

        return List.of(postgresOutputPath);
    }

    private void copyMultiDimRasters(String gdalContainerId, String filePath, String postgresOutputPath) {
        ByteArrayOutputStream errorStream = new ByteArrayOutputStream();
        String execId = createComplexCommand(gdalContainerId, "cp",
                filePath,
                postgresOutputPath)
                .withErrorStream(errorStream)
                .withEvaluationTimeout(300)
                .exec();
        handleErrors(errorStream, execId, logger);
    }

    private Map<String, Integer> multipleVrtRastersFromMultiDim(String gdalContainerId, MultidimSettings mdimSettings,
            String postgresOutputPath, List<String> geoTiffFilenames) {
        Map<String, Integer> postgresOutputPathsAndNBands = new LinkedHashMap<>();

        String execId;
        String inputRasterFilePath = "NETCDF:" + postgresOutputPath + ":" + mdimSettings.getLayerArrayName();
        ByteArrayOutputStream errorStream = new ByteArrayOutputStream();

        for (int index = 0; index < geoTiffFilenames.size(); ++index) {
            String geoTiffFilename = geoTiffFilenames.get(index);
            String outputRasterFilePath = Paths.get(postgresOutputPath)
                    .resolveSibling(FileUtils.replaceExtension(geoTiffFilename, "vrt"))
                    .toString();
            execId = createComplexCommand(gdalContainerId, "gdalwarp",
                    "-srcband", Integer.toString(index + 1),
                    "-t_srs", "EPSG:4326",
                    "-wo", "OPTIMIZE_SIZE=YES",
                    inputRasterFilePath,
                    outputRasterFilePath)
                    .withErrorStream(errorStream)
                    .exec();
            handleErrors(errorStream, execId, logger);
            errorStream.reset();

            postgresOutputPathsAndNBands.put(outputRasterFilePath, 1);
        }
        return postgresOutputPathsAndNBands;
    }

    private void ensurePostGISRasterSupportEnabled(String postGISContainerId, String database) {
        ByteArrayOutputStream errorStream = new ByteArrayOutputStream();
        String execId = createComplexCommand(postGISContainerId,
                "psql", "-U", postgreSQLEndpoint.getUsername(), "-d", database, "-w")
                .withHereDocument("CREATE EXTENSION IF NOT EXISTS postgis_raster;" +
                        "ALTER DATABASE \"" + database + "\" SET postgis.enable_outdb_rasters = True;" +
                        "ALTER DATABASE \"" + database + "\" SET postgis.gdal_enabled_drivers = 'GTiff netCDF VRT';")
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
                        "raster2pgsql " + mode + " -C -t auto -R -F -q -I -M -Y"
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
            return FileUtils.replaceExtension(rasterOutFilePath, "tif");

        }
    }

}
