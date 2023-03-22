package uk.ac.cam.cares.jps.agent.sensorloggermobileappagent;

public class EnvConfig {
    public static final String DATABASE = System.getenv("DATABASE");
    private EnvConfig() {
        throw new IllegalStateException();
    }
}

