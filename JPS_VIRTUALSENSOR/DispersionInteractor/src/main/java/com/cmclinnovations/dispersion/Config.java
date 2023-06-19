package com.cmclinnovations.dispersion;

public class Config {
    public static final String DATABASE = System.getenv("DATABASE");
    public static final String SCOPE_TABLE_NAME = System.getenv("SCOPE_TABLE_NAME");
    public static final String AERMOD_AGENT_IRI = System.getenv("AERMOD_AGENT_IRI");
    public static final String AERMOD_AGENT_URL = System.getenv("AERMOD_AGENT_URL");
    public static final String WEATHER_AGENT_URL = System.getenv("WEATHER_AGENT_URL");
    public static final String SHIP_INPUT_AGENT = System.getenv("SHIP_INPUT_AGENT");
    public static final String GEOSERVER_WORKSPACE = System.getenv("GEOSERVER_WORKSPACE");
    public static final EndpointConfig ENDPOINT_CONFIG = new EndpointConfig();

    private Config() {
        throw new IllegalStateException();
    }
}
