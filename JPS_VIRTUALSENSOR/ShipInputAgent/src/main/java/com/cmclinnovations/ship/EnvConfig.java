package com.cmclinnovations.ship;

public class EnvConfig {
    public static final String DATABASE = System.getenv("DATABASE");
    public static final String DATA_DIR = System.getenv("DATA_DIR");

    private EnvConfig() {
        throw new IllegalStateException();
    }
}
