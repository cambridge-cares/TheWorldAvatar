package com.cmclinnovations.emissions;

public class EnvConfig {
    private static final String STACK_NAME = System.getenv("STACK_NAME");
    public static final String DATABASE = System.getenv("DATABASE");

    private EnvConfig() {
        throw new IllegalStateException();
    }
}
