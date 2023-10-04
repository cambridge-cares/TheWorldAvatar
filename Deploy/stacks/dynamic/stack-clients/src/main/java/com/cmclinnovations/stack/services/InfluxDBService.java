package com.cmclinnovations.stack.services;

import com.cmclinnovations.stack.services.config.ServiceConfig;

import java.nio.file.Path;
import java.util.Map;

import com.cmclinnovations.stack.clients.core.EndpointNames;
import com.cmclinnovations.stack.clients.postgis.PostGISEndpointConfig;

public final class InfluxDBService extends ContainerService {

    private static final String INFLUXDB_USER_KEY = "DOCKER_INFLUXDB_INIT_USERNAME";

    public static final String TYPE = "influxdb";
    private static final Path influxdb_FILE = Path.of("/root", ".influxdb");
    private static final String DEFAULT_USERNAME = "admin";
    private static final String DEFAULT_PORT = "8086";
    private static final String DEFAULT_PASSWORD_FILE = "/run/secrets/influxdb_password";

    private final PostGISEndpointConfig endpointConfig;

}
