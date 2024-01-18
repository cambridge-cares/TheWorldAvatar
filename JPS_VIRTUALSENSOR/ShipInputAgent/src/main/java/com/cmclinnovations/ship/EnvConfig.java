package com.cmclinnovations.ship;

import org.json.JSONArray;

public class EnvConfig {
    public static final String DATABASE = System.getenv("DATABASE");
    public static final String DATA_DIR = System.getenv("DATA_DIR");
    public static final String LAST_READ_FILE = System.getenv("LAST_READ_FILE");
    public static final String TIME_OFFSET_FILE = System.getenv("TIME_OFFSET_FILE");
    public static final String GEOSERVER_WORKSPACE = System.getenv("GEOSERVER_WORKSPACE");
    public static final String EMISSIONS_AGENT_URL = System.getenv("EMISSIONS_AGENT_URL");
    public static final String EMISSIONS_AGENT_IRI = System.getenv("EMISSIONS_AGENT_IRI");
    public static final String PYTHON_SERVICE_URL = System.getenv("PYTHON_SERVICE_URL");
    public static final String PARALLELISE_CALCULATIONS = System.getenv("PARALLELISE_CALCULATIONS");
    public static final String SHIP_IRI_LOOKUP_TABLE = System.getenv("SHIP_IRI_LOOKUP_TABLE");
    public static final boolean USE_LIVE_DATA = Boolean.parseBoolean(System.getenv("USE_LIVE_DATA"));
    public static final String API_KEY = System.getenv("API_KEY");
    public static final JSONArray BOUNDING_BOXES = new JSONArray(System.getenv("BOUNDING_BOXES"));

    private EnvConfig() {
        throw new IllegalStateException();
    }
}
