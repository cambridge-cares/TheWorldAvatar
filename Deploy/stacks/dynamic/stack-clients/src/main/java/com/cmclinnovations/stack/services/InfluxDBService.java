package com.cmclinnovations.stack.services;

import com.cmclinnovations.stack.services.config.ServiceConfig;
import com.cmclinnovations.stack.clients.influxdb.InfluxDBEndpointConfig;
import java.nio.file.Path;
import java.util.Map;

import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.io.IOException;

public final class InfluxDBService extends ContainerService {

    // Constants for environment variable keys and default values
    private static final String DEFAULT_INIT_MODE = "setup";
    private static final String DEFAULT_USERNAME = "admin";
    private static final String DEFAULT_ORG = "TestOrg";
    private static final String DEFAULT_BUCKET = "TestBucket";
    private static final String DEFAULT_RETENTION = "1w";

    // Secrets files address
    private static final String DEFAULT_PASSWORD_FILE = "/run/secrets/influxdb_password";
    private static final String DEFAULT_TOKEN_FILE = "/run/secrets/influxdb_token";

    // Configuration file path for InfluxDB inside the container
    private static final Path INFLUXDB_CONFIG_PATH = Path.of("/etc/influxdb2/influxdb.conf");

    // Endpoint configuration object for InfluxDB
    private final InfluxDBEndpointConfig endpointConfig;

    public InfluxDBService(String stackName, ServiceConfig config) {
        super(stackName, config);

        // Set environment variables if they are not already set
        setEnvironmentVariableIfAbsent("DOCKER_INFLUXDB_INIT_MODE", DEFAULT_INIT_MODE);
        setEnvironmentVariableIfAbsent("DOCKER_INFLUXDB_INIT_USERNAME", DEFAULT_USERNAME);
        setEnvironmentVariableIfAbsent("DOCKER_INFLUXDB_INIT_ORG", DEFAULT_ORG);
        setEnvironmentVariableIfAbsent("DOCKER_INFLUXDB_INIT_BUCKET", DEFAULT_BUCKET);
        setEnvironmentVariableIfAbsent("DOCKER_INFLUXDB_INIT_RETENTION", DEFAULT_RETENTION);
        setEnvironmentVariableIfAbsent("INFLUXDB_PASSWORD_FILE", DEFAULT_PASSWORD_FILE);
        setEnvironmentVariableIfAbsent("INFLUXDB_TOKEN_FILE", DEFAULT_TOKEN_FILE);

        // Initialize the endpoint configuration
        endpointConfig = new InfluxDBEndpointConfig(
            "InfluxDB", 
            getHostName(), 
            "8086", 
            getEnvironmentVariable("DOCKER_INFLUXDB_INIT_USERNAME"), 
            getEnvironmentVariable("INFLUXDB_PASSWORD_FILE"),
            getEnvironmentVariable("DOCKER_INFLUXDB_INIT_ORG"),
            getEnvironmentVariable("DOCKER_INFLUXDB_INIT_BUCKET")
        );
    }

    @Override
    public void doPostStartUpConfiguration() {
        // Write endpoint configuration to some storage (file, database, etc.)
        writeEndpointConfig(endpointConfig);
    
        // Send the influxdb.conf file to the container
        sendFiles(Map.of(
                INFLUXDB_CONFIG_PATH.getFileName().toString(),
                getConfigFileContent()),
                INFLUXDB_CONFIG_PATH.getParent().toString());
    }

    // Helper method to get the content of the config file as a byte array
    private byte[] getConfigFileContent() {
        // Replace with actual logic for reading the file
        // For example, read the file located at the standard path inside the container
        try {
            return Files.readAllBytes(INFLUXDB_CONFIG_PATH);
        } catch (IOException e) {
            throw new RuntimeException("Failed to read InfluxDB configuration file", e);
        }
    }
}
