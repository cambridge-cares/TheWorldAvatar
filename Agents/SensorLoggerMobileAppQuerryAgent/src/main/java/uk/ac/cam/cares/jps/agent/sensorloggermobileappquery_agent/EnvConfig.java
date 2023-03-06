package uk.ac.cam.cares.jps.agent.sensorloggermobileappquery_agent;

public class EnvConfig {
    public static final String DATABASE = System.getenv("DATABASE");
    public static final String SCOPE_TABLE_NAME = System.getenv("SCOPE_TABLE_NAME");
    public static final EndpointConfig ENDPOINT_CONFIG = new EndpointConfig();


    private EnvConfig() {
        throw new IllegalStateException();
    }
}

