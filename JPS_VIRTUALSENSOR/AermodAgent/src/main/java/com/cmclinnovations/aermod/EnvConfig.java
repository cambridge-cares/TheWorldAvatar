package com.cmclinnovations.aermod;

public class EnvConfig {
    public static final String PYTHON_SERVICE_URL = System.getenv("PYTHON_SERVICE_URL");
    public static final String AERMET_EXE = System.getenv("AERMET_EXE");
    public static final String AERMOD_EXE = System.getenv("AERMOD_EXE");
    public static final String DATABASE = System.getenv("DATABASE");
    public static final String WEATHER_AGENT = System.getenv("WEATHER_AGENT");
    public static final String SIMULATION_DIR = System.getenv("SIMULATION_DIR");

    private EnvConfig() {
        throw new IllegalStateException();
    }
}

