package com.cmclinnovations.dispersion;

public class Config {
    public static final String DATABASE = System.getenv("DATABASE");
    public static final String SCOPE_TABLE_NAME = System.getenv("SCOPE_TABLE_NAME");
    public static final String AERMOD_AGENT_IRI = System.getenv("AERMOD_AGENT_IRI");
    public static final String AERMOD_AGENT_URL = System.getenv("AERMOD_AGENT_URL");
    public static final String WEATHER_AGENT_URL = System.getenv("WEATHER_AGENT_URL");
    public static final String SHIP_INPUT_AGENT = System.getenv("SHIP_INPUT_AGENT");
    public static final String GEOSERVER_WORKSPACE = System.getenv("GEOSERVER_WORKSPACE");
    public static final String STACK_URL = System.getenv("STACK_URL");
    public static final EndpointConfig ENDPOINT_CONFIG = new EndpointConfig();
    public static final String SENSORS_TABLE_NAME = System.getenv("SENSORS_TABLE_NAME");
    public static final String VIRTUAL_SENSOR_AGENT_IRI = System.getenv("VIRTUAL_SENSOR_AGENT_IRI");
    public static final String DISPERSION_CONTOURS_TABLE = System.getenv("DISPERSION_CONTOURS_TABLE");
    public static final String SHIPS_LAYER_NAME = System.getenv("SHIPS_LAYER_NAME");
    public static final String BUILDINGS_TABLE = System.getenv("BUILDINGS_TABLE");
    public static final String STATIC_SOURCE_TABLE = System.getenv("STATIC_SOURCE_TABLE");
    public static final String VIRTUAL_SENSOR_AGENT_URL = System.getenv("VIRTUAL_SENSOR_AGENT_URL");
    public static final String ELEVATION_CONTOURS_TABLE = System.getenv("ELEVATION_CONTOURS_TABLE");

    private Config() {
        throw new IllegalStateException();
    }
}
