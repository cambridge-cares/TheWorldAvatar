package com.cmclinnovations.dispersion;

public class EnvConfig {
    public static final String DATABASE = System.getenv("DATABASE");
    public static final String SCOPE_TABLE_NAME = System.getenv("SCOPE_TABLE_NAME");
    public static final String EPISODE_AGENT_IRI = System.getenv("EPISODE_AGENT_IRI");
    public static final String EPISODE_AGENT_URL = System.getenv("EPISODE_AGENT_URL");

    private EnvConfig() {
        throw new IllegalStateException();
    }
}
