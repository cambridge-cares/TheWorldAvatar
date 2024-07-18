package com.cmclinnovations.filteragent;

/**
 * constants obtained from environment variables
 */
public class EnvConfig {
    public static final String DEFAULT_NAMESPACE = System.getenv("DEFAULT_NAMESPACE");
    public static final String DEFAULT_QUERY = System.getenv("DEFAULT_QUERY");

    private EnvConfig() {
        throw new IllegalStateException("EnvConfig");
    }
}
