package com.cmclinnovations.shipdata;

public class EnvConfig {
    private static final String STACK_NAME = System.getenv("STACK_NAME");
    public static final String DATABASE = System.getenv("DATABASE");
    public static final String SHIP_DATA_AGENT_IRI = System.getenv("SHIP_DATA_AGENT_IRI");

    private EnvConfig() {
        throw new IllegalStateException();
    }
}
