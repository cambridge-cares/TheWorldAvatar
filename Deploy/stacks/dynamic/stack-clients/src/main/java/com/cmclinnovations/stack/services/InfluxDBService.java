package com.cmclinnovations.stack.services;

import com.cmclinnovations.stack.clients.core.EndpointNames;
import com.cmclinnovations.stack.services.config.ServiceConfig;
import java.nio.file.Path;
import java.util.Map;

import java.util.logging.Logger;

public final class InfluxDBService extends ContainerService {

    private static final String TYPE = "influxdb";
    private static final Path SECRETS_FILE = Path.of("/stacks/dynamic/stack-manager/inputs/secrets");

    // Default environment variables for InfluxDB
    private static final String DEFAULT_INIT_MODE = "setup";
    private static final String DEFAULT_USERNAME = "admin";
    private static final String DEFAULT_PASSWORD = "admin123";
    private static final String DEFAULT_ORG = "TestOrg";
    private static final String DEFAULT_BUCKET = "TestBucket";
    private static final String DEFAULT_RETENTION = "1w";
    private static final String DEFAULT_ADMIN_TOKEN = "your_default_admin_token_here";

    private static final String DEFAULT_PASSWORD_FILE = "/run/secrets/influxdb_password";

    private static final Logger logger = Logger.getLogger(InfluxDBService.class.getName());

    public InfluxDBService(String stackName, ServiceConfig config) {
        super(stackName, config);

        logger.info("Starting post-startup configuration...");
        // Use the methods from ContainerService to set environment variables
        setEnvironmentVariableIfAbsent("DOCKER_INFLUXDB_INIT_MODE", DEFAULT_INIT_MODE);
        setEnvironmentVariableIfAbsent("DOCKER_INFLUXDB_INIT_USERNAME", DEFAULT_USERNAME);
        setEnvironmentVariableIfAbsent("DOCKER_INFLUXDB_INIT_PASSWORD", DEFAULT_PASSWORD);
        setEnvironmentVariableIfAbsent("DOCKER_INFLUXDB_INIT_ORG", DEFAULT_ORG);
        setEnvironmentVariableIfAbsent("DOCKER_INFLUXDB_INIT_BUCKET", DEFAULT_BUCKET);
        setEnvironmentVariableIfAbsent("DOCKER_INFLUXDB_INIT_RETENTION", DEFAULT_RETENTION);
        setEnvironmentVariableIfAbsent("DOCKER_INFLUXDB_INIT_ADMIN_TOKEN", DEFAULT_ADMIN_TOKEN);
    }

    @Override
    public void doPostStartUpConfiguration() {
        // Write the environment variables to the secrets file
        writeSecretsFile(SECRETS_FILE);

        // Any other post-startup configuration can be done here
    }

    private void writeSecretsFile(Path filePath) {
        // Create a map of secret key-value pairs to be written to the secrets file
        Map<String, byte[]> secretData = Map.of(
            "DOCKER_INFLUXDB_INIT_MODE", getEnvironmentVariable("DOCKER_INFLUXDB_INIT_MODE").getBytes(),
            "DOCKER_INFLUXDB_INIT_USERNAME", getEnvironmentVariable("DOCKER_INFLUXDB_INIT_USERNAME").getBytes(),
            "DOCKER_INFLUXDB_INIT_PASSWORD", getEnvironmentVariable("DOCKER_INFLUXDB_INIT_PASSWORD").getBytes(),
            "DOCKER_INFLUXDB_INIT_ORG", getEnvironmentVariable("DOCKER_INFLUXDB_INIT_ORG").getBytes(),
            "DOCKER_INFLUXDB_INIT_BUCKET", getEnvironmentVariable("DOCKER_INFLUXDB_INIT_BUCKET").getBytes(),
            "DOCKER_INFLUXDB_INIT_RETENTION", getEnvironmentVariable("DOCKER_INFLUXDB_INIT_RETENTION").getBytes(),
            "DOCKER_INFLUXDB_INIT_ADMIN_TOKEN", getEnvironmentVariable("DOCKER_INFLUXDB_INIT_ADMIN_TOKEN").getBytes()
        );
        
        // Use the sendFiles method from ContainerService to send the secrets to the specified file path
        sendFiles(secretData, filePath.toString());
    }
    
}
