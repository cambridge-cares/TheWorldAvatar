package com.cmclinnovations.stack.clients.gdal;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Map;

import com.cmclinnovations.stack.clients.docker.ContainerClient;
import com.cmclinnovations.stack.clients.docker.DockerClient.TempDir;
import com.cmclinnovations.stack.clients.postgis.PostGISEndpointConfig;

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

        try (TempDir tmpDir = makeTempDir(containerId)) {
            sendFilesContent(containerId, Map.of(layername, fileContents.getBytes()),
                    tmpDir.getPath());

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

}
