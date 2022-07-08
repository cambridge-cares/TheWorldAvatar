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

import com.cmclinnovations.stack.clients.docker.ContainerClient;
import com.cmclinnovations.stack.clients.postgis.PostGISEndpointConfig;
import com.cmclinnovations.stack.clients.utils.FileUtils;
import com.cmclinnovations.stack.clients.utils.TempDir;
import com.google.common.collect.ArrayListMultimap;
import com.google.common.collect.Multimap;

public class GDALClient extends ContainerClient {

    private final PostGISEndpointConfig postgreSQLEndpoint;

    public GDALClient() {
        postgreSQLEndpoint = readEndpointConfig("postgis", PostGISEndpointConfig.class);
    }

    private String computePGSQLSourceString(String database) {
        return "PG:dbname=" + database + " host=" + postgreSQLEndpoint.getHostName()
                + " port=" + postgreSQLEndpoint.getPort() + " user=" + postgreSQLEndpoint.getUsername()
                + " password=" + postgreSQLEndpoint.getPassword();
    }

    public void uploadVectorStringToPostGIS(String database, String layername, String fileContents,
            Ogr2OgrOptions options) {
        String containerId = getContainerId("gdal");

        try (TempDir tmpDir = makeRemoteTempDir(containerId)) {
            sendFilesContent(containerId, Map.of(layername, fileContents.getBytes()),
                    tmpDir.toString());

            uploadVectorToPostGIS(database, layername, tmpDir + "/" + layername, null, options);
        }
    }

    public void uploadVectorFileToPostGIS(String database, String layername, String filePath, Ogr2OgrOptions options) {
        String fileContents;
        try {
            fileContents = Files.readString(Path.of(filePath));
        } catch (IOException ex) {
            throw new RuntimeException("Failed to read file '" + filePath + "'.", ex);
        }
        uploadVectorStringToPostGIS(database, layername, fileContents, options);
    }

    public void uploadVectorURLToPostGIS(String database, String layername, String url, Ogr2OgrOptions options) {
        uploadVectorToPostGIS(database, layername, url, null, options);
    }

    private void uploadVectorToPostGIS(String database, String layername, String filePath, String fileContents,
            Ogr2OgrOptions options) {

        String containerId = getContainerId("gdal");

        ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
        ByteArrayOutputStream errorStream = new ByteArrayOutputStream();

        createComplexCommand(containerId, options.appendToArgs("ogr2ogr", "-overwrite",
                "-f", "PostgreSQL",
                computePGSQLSourceString(database),
                filePath,
                "-nln", layername))
                .withHereDocument(fileContents)
                .withOutputStream(outputStream)
                .withErrorStream(errorStream)
                .withEnvVar("PG_USE_COPY", "YES")
                .withEnvVars(options.getEnv())
                .exec();

        if (0 != errorStream.size()) {
            throw new RuntimeException("Docker exec command wrote the following to stderr:\n" + errorStream.toString());
        }
    }

    public void uploadRasterFilesToPostGIS(String database, String layername,
            String dirPath, GDALTranslateOptions options) {

        String gdalContainerId = getContainerId("gdal");
        String postGISContainerId = getContainerId("postgis");

        try (TempDir tempDir = makeRemoteTempDir(gdalContainerId)) {
            sendFolder(gdalContainerId, dirPath, tempDir.toString());

            List<String> geotiffFiles = convertRastersToGeoTiffs(gdalContainerId, layername, tempDir, options);

            ensurePostGISRasterSupportEnabled(postGISContainerId, database);

            uploadRasters(postGISContainerId, database, layername, geotiffFiles);
        }
    }

    private Multimap<String, String> findRasterFiles(String containerId, String dirPath) {
        ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
        ByteArrayOutputStream errorStream = new ByteArrayOutputStream();
        createComplexCommand(containerId, "gdalmanage", "identify", "-r", dirPath)
                .withOutputStream(outputStream)
                .withErrorStream(errorStream)
                .exec();

        String errors = errorStream.toString();
        return outputStream.toString().lines()
                .map(entry -> entry.split(": "))
                .collect(ArrayListMultimap::create,
                        (m, pair) -> m.put(pair[1], pair[0]),
                        Multimap::putAll);
    }

    private List<String> convertRastersToGeoTiffs(String gdalContainerId, String layername, TempDir tempDir,
            GDALTranslateOptions options) {

        Multimap<String, String> foundRasterFiles = findRasterFiles(gdalContainerId, tempDir.toString());

        ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
        ByteArrayOutputStream errorStream = new ByteArrayOutputStream();

        Set<Path> createdDirectories = new HashSet<>();

        List<String> geotiffFiles = new ArrayList<>();

        for (Map.Entry<String, Collection<String>> fileTypeEntry : foundRasterFiles.asMap().entrySet()) {
            String inputFormat = fileTypeEntry.getKey();
            for (String filePath : fileTypeEntry.getValue()) {

                String outputPath = generateRasterOutPath(tempDir.toString(), filePath, layername);
                geotiffFiles.add(outputPath);

                Path directoryPath = Paths.get(outputPath).getParent();
                if (!createdDirectories.contains(directoryPath)) {
                    makeDir(gdalContainerId, directoryPath.toString());
                    createdDirectories.add(directoryPath);
                }

                createComplexCommand(gdalContainerId, options.appendToArgs("gdal_translate",
                        "-if", inputFormat,
                        // https://gdal.org/drivers/raster/cog.html#raster-cog
                        "-of", "COG",
                        filePath,
                        outputPath))
                        .withOutputStream(outputStream)
                        .withErrorStream(errorStream)
                        .withEnvVars(options.getEnv())
                        .exec();

                if (0 != errorStream.size()) {
                    throw new RuntimeException(
                            "Docker exec command wrote the following to stderr:\n" + errorStream.toString());
                }
            }
        }
        return geotiffFiles;
    }

    private void ensurePostGISRasterSupportEnabled(String postGISContainerId, String database) {
        createComplexCommand(postGISContainerId,
                "psql", "-U", postgreSQLEndpoint.getUsername(), "-d", database, "-w")
                .withHereDocument("CREATE EXTENSION IF NOT EXISTS postgis_raster;" +
                        "SET SESSION postgis.enable_outdb_rasters = True;" +
                        "SET SESSION postgis.gdal_enabled_drivers = 'COG';")
                .exec();
    }

    private void uploadRasters(String postGISContainerId, String database, String layername,
            List<String> geotiffFiles) {

        ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
        ByteArrayOutputStream errorStream = new ByteArrayOutputStream();

        String execId = createComplexCommand(postGISContainerId, "bash", "-c",
                // https://postgis.net/docs/using_raster_dataman.html#RT_Raster_Loader
                "raster2pgsql -d -C -t auto -R -F -I -M -Y"
                        + geotiffFiles.stream().collect(Collectors.joining(" ", " ", " "))
                        + layername
                        + " | psql -U " + postgreSQLEndpoint.getUsername() + " -d " + database + " -w")
                .withOutputStream(outputStream)
                .withErrorStream(errorStream)
                .exec();

        if (0 != getCommandErrorCode(execId)) {
            throw new RuntimeException(
                    "Docker exec command wrote the following to stderr:\n" + errorStream.toString());
        }
    }

    private String generateRasterOutPath(String basePathIn, String filePath, String layerName) {
        return FileUtils.replaceExtension(
                Path.of("/geotiffs", layerName).resolve(Path.of(basePathIn).relativize(Path.of(filePath))).toString(),
                ".tif");
    }

}
