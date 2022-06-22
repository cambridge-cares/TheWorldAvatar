package com.cmclinnovations.apis;

import java.io.ByteArrayOutputStream;

public class GDALClient extends ContainerClient {

    private final PostGISEndpointConfig postgreSQLEndpoint;

    public GDALClient() {
        super();
        postgreSQLEndpoint = readEndpointConfig("postgis", PostGISEndpointConfig.class);
    }

    private String computePGSQLSourceString(String database) {
        return "PG:dbname=" + database + " host=" + postgreSQLEndpoint.getHostName()
                + " port=" + postgreSQLEndpoint.getPort() + " user=" + postgreSQLEndpoint.getUsername()
                + " password=" + postgreSQLEndpoint.getPassword();
    }

    public void uploadGeoJsonToPostGIS(String database, String layername, String json,
            Ogr2OgrOptions options) {

        String containerId = getDockerClient().getContainerId("gdal");

        ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
        ByteArrayOutputStream errorStream = new ByteArrayOutputStream();

        getDockerClient().createComplexCommand(containerId, options.appendToArgs("ogr2ogr", "-overwrite",
                "-f", "PostgreSQL",
                computePGSQLSourceString(database),
                "/vsistdin/",
                "-nln", layername))
                .withHereDocument(json)
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
