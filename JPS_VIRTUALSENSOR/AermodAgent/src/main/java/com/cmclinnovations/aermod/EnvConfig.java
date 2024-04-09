package com.cmclinnovations.aermod;

public class EnvConfig {
    public static final String PYTHON_SERVICE_URL = System.getenv("PYTHON_SERVICE_URL");
    public static final String AERMAP_EXE = System.getenv("AERMAP_EXE");
    public static final String AERMET_EXE = System.getenv("AERMET_EXE");
    public static final String BPIPPRM_EXE = System.getenv("BPIPPRM_EXE");
    public static final String AERMOD_EXE = System.getenv("AERMOD_EXE");
    public static final String DATABASE = System.getenv("DATABASE");
    public static final String WEATHER_AGENT = System.getenv("WEATHER_AGENT");
    public static final String SIMULATION_DIR = System.getenv("SIMULATION_DIR");
    public static final String FILE_SERVER = System.getenv("FILE_SERVER_URL");
    public static final String GEOSERVER_WORKSPACE = System.getenv("GEOSERVER_WORKSPACE");
    public static final String ELEVATION_TABLE = System.getenv("ELEVATION_TABLE");
    public static final String DISPERSION_RASTER_TABLE = System.getenv("DISPERSION_RASTER_TABLE");
    public static final String DISPERSION_CONTOURS_TABLE = System.getenv("DISPERSION_CONTOURS_TABLE");
    public static final String SHIP_IRI_LOOKUP_TABLE = System.getenv("SHIP_IRI_LOOKUP_TABLE");
    public static final String BUILDINGS_TABLE = System.getenv("BUILDINGS_TABLE");
    public static final String STATIC_SOURCE_TABLE = System.getenv("STATIC_SOURCE_TABLE");
    public static final String ELEVATION_CONTOURS_TABLE = System.getenv("ELEVATION_CONTOURS_TABLE");
    public static final String PARALLELISE_EMISSIONS_UPDATE = System.getenv("PARALLELISE_EMISSIONS_UPDATE");
    public static final double TARGET_EMISSION_VOLUME_FRACTION = Double
            .parseDouble(System.getenv("TARGET_EMISSION_VOLUME_FRACTION"));

    private EnvConfig() {
        throw new IllegalStateException();
    }
}
