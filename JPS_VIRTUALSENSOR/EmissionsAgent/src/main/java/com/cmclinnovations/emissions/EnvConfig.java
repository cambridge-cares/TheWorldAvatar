package com.cmclinnovations.emissions;

public class EnvConfig {
    private static final String STACK_NAME = System.getenv("STACK_NAME");
    public static final String PYTHON_SERVICE_URL = System.getenv("PYTHON_SERVICE_URL").replace("${STACK_NAME}",
            STACK_NAME);
    public static final String DATABASE = System.getenv("DATABASE");

    private EnvConfig() {
        throw new IllegalStateException();
    }
}
