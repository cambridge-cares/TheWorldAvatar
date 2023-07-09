package com.cmclinnovations.virtualsensor;

public class EnvConfig {
    public static final String DATABASE = System.getenv("DATABASE");

    private EnvConfig() {
        throw new IllegalStateException();
    }
}
