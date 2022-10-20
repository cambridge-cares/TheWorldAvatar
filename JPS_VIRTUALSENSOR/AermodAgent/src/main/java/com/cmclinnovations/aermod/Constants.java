package com.cmclinnovations.aermod;

public class Constants {
    // rdf types
    public static final String PREFIX_DISP = "http://www.theworldavatar.com/dispersion/";
    public static final String REPORTING_STATION = "https://www.theworldavatar.com/kg/ontoems/ReportingStation";
    public static final String NX = PREFIX_DISP + "nx";
    public static final String NY = PREFIX_DISP + "ny";
    public static final String SCOPE = PREFIX_DISP + "Scope";
    public static final String SIMULATION_TIME = PREFIX_DISP + "SimulationTime";

    private Constants() {
        throw new IllegalStateException();
    }
}
