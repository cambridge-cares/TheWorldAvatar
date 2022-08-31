package com.cmclinnovations.ship;

public class EnvConfig {
    public static final String DATABASE = System.getenv("DATABASE");
    public static final String DATA_DIR = System.getenv("DATA_DIR");
    public static final String LAST_READ_FILE = System.getenv("LAST_READ_FILE");
    public static final String TIME_OFFSET_FILE = System.getenv("TIME_OFFSET_FILE");

    private EnvConfig() {
        throw new IllegalStateException();
    }
}
