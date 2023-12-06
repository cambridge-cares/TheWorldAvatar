package com.cmclinnovations.stack.services;

import com.cmclinnovations.stack.services.config.ServiceConfig;

import java.nio.file.Path;
import java.util.Map;

import com.cmclinnovations.stack.clients.core.EndpointNames;
import com.cmclinnovations.stack.clients.postgis.PostGISEndpointConfig;

public final class PostGISService extends ContainerService {

    /**
     *
     */
    private static final String POSTGRES_USER_KEY = "POSTGRES_USER";

    public static final String TYPE = "postgres";

    private static final Path PGPASS_FILE = Path.of("/root", ".pgpass");

    private static final String DEFAULT_USERNAME = "postgres";
    private static final String DEFAULT_PORT = "5432";
    private static final String DEFAULT_PASSWORD_FILE = "/run/secrets/postgis_password";

    private final PostGISEndpointConfig endpointConfig;

    public PostGISService(String stackName, ServiceConfig config) {
        super(stackName, config);

        setEnvironmentVariableIfAbsent(POSTGRES_USER_KEY, DEFAULT_USERNAME);
        setEnvironmentVariableIfAbsent("PGUSER", getEnvironmentVariable(POSTGRES_USER_KEY));
        setEnvironmentVariableIfAbsent("POSTGRES_PASSWORD_FILE", DEFAULT_PASSWORD_FILE);
        setEnvironmentVariableIfAbsent("PGPASSFILE", PGPASS_FILE.toString());

        endpointConfig = new PostGISEndpointConfig(
                EndpointNames.POSTGIS, getHostName(), DEFAULT_PORT,
                getEnvironmentVariable(POSTGRES_USER_KEY), getEnvironmentVariable("POSTGRES_PASSWORD_FILE"));
    }

    @Override
    public void doPostStartUpConfiguration() {
        writeEndpointConfig(endpointConfig);

        sendFiles(Map.of(
                PGPASS_FILE.getFileName().toString(),
                ("localhost:"
                        + endpointConfig.getPort() + ":"
                        + "*:"
                        + endpointConfig.getUsername() + ":"
                        + endpointConfig.getPassword())
                        .getBytes()),
                PGPASS_FILE.getParent().toString());

        executeCommand("chmod", "0600", PGPASS_FILE.toString());
    }

}
