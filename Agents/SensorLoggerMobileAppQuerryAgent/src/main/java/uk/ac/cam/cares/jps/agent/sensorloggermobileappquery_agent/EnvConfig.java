package uk.ac.cam.cares.jps.agent.sensorloggermobileappquery_agent;

public class EnvConfig {
    public static final String DATABASE = System.getenv("DATABASE");

    private EnvConfig() {
        throw new IllegalStateException();
    }
}

