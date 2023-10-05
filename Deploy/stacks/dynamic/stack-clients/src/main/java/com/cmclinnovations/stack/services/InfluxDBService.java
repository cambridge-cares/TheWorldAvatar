package com.cmclinnovations.stack.services;

import com.cmclinnovations.stack.clients.core.EndpointNames;
import com.cmclinnovations.stack.services.config.ServiceConfig;
import java.nio.file.Path;
import java.util.Map;

public final class InfluxDBService extends ContainerService {

    // Define constants for default InfluxDB settings
    private static final String INFLUXDB_USER_KEY = "DOCKER_INFLUXDB_INIT_USERNAME";
    public static final String TYPE = "influxdb";
    private static final Path INFLUXDBPASS_FILE = Path.of("/root", ".influxdb");
    private static final String DEFAULT_USERNAME = "admin";
    private static final String DEFAULT_PORT = "8086";
    private static final String DEFAULT_PASSWORD_FILE = "/run/secrets/influxdb_password";
    private final ServiceConfig endpointConfig;

    public InfluxDBService(String stackName, ServiceConfig config) {
        super(stackName, config);

        setEnvironmentVariableIfAbsent("DOCKER_INFLUXDB_INIT_USERNAME", DEFAULT_USERNAME);
        setEnvironmentVariableIfAbsent("IFLUXDBUSER", getEnvironmentVariable(INFLUXDB_USER_KEY));
        setEnvironmentVariableIfAbsent("INFLUXDB_PASSWORD_FILE", DEFAULT_PASSWORD_FILE);
        setEnvironmentVariableIfAbsent("INFLUXDBPASSFILE", INFLUXDBPASS_FILE.toString());

        // Initialize the InfluxDB endpoint configuration
        endpointConfig = new ServiceConfig();  // Replace with actual class and constructor
    }

    @Override
    public void doPostStartUpConfiguration() {
        // Implement any post-startup configurations here
        // For example, write the endpoint configuration to a file
        writeEndpointConfig(endpointConfig);
    }
}
