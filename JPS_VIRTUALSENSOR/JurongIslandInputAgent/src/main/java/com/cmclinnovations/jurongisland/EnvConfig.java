package com.cmclinnovations.jurongisland;

public class EnvConfig {
    public static final String QUERY_ENDPOINT = System.getenv("QUERY_ENDPOINT");
    public static final String NUMBER_SOURCES = System.getenv("NUMBER_SOURCES");
    public static final String DENSITY = System.getenv("DENSITY");
    public static final String TEMPERATURE = System.getenv("TEMPERATURE");

    private EnvConfig() {
        throw new IllegalStateException();
    }
}
