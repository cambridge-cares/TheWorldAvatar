package com.cmclinnovations.aermod;

public class EnvConfig {
    public static final String PYTHON_SERVICE_URL = System.getenv("PYTHON_SERVICE_URL");
    public static final String PYTHON_SERVICE_ELEVATION_URL = System.getenv("PYTHON_SERVICE_ELEVATION_URL");
    public static final String AERMAP_EXE = System.getenv("AERMAP_EXE");
    public static final String AERMET_EXE = System.getenv("AERMET_EXE");
    public static final String BPIPPRM_EXE = System.getenv("BPIPPRM_EXE");
    public static final String NUMBER_SOURCES = System.getenv("NUMBER_SOURCES");
    public static final String NUMBER_BUILDINGS = System.getenv("NUMBER_BUILDINGS");
    public static final String AERMOD_EXE = System.getenv("AERMOD_EXE");
    public static final String DATABASE = System.getenv("DATABASE");
    public static final String WEATHER_AGENT = System.getenv("WEATHER_AGENT");
    public static final String OPENMETEO_AGENT_RUN = System.getenv("OPENMETEO_AGENT_RUN");
    public static final String OPENMETEO_AGENT_DELETE = System.getenv("OPENMETEO_AGENT_DELETE");
    public static final String SIMULATION_DIR = System.getenv("SIMULATION_DIR");
    public static final String FILE_SERVER = System.getenv("FILE_SERVER_URL");
    public static final String GEOSERVER_WORKSPACE = System.getenv("GEOSERVER_WORKSPACE");
    public static final String DISPERSION_LAYER = System.getenv("DISPERSION_LAYER");
    public static final String DISPERSION_STYLE_NAME = System.getenv("DISPERSION_STYLE_NAME");
    public static final String SOURCE_LAYER = System.getenv("SOURCE_LAYER");
    public static final String VIS_FOLDER = System.getenv("VIS_FOLDER");
    public static final String GEOSERVER_URL = System.getenv("GEOSERVER_URL");
    public static final String INCLUDE_ELEVATION = System.getenv("INCLUDE_ELEVATION");

    private EnvConfig() {
        throw new IllegalStateException();
    }
}

