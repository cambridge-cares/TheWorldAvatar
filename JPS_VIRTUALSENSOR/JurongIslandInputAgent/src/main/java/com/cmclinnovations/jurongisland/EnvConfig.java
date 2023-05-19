package com.cmclinnovations.jurongisland;

public class EnvConfig {
    public static final String DATABASE = System.getenv("DATABASE");
    public static final String QUERY_ENDPOINT = System.getenv("QUERY_ENDPOINT");
    public static final String NUMBER_SOURCES = System.getenv("NUMBER_SOURCES");

    private EnvConfig() {
        throw new IllegalStateException();
    }
}
