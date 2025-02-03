package uk.ac.cam.cares.jps.agent.ceavisualisation;

enum Area {
    GFA("gfa"),
    ROOF_AREA("roof_area"),
    NORTH_AREA("north_area"),
    SOUTH_AREA("south_area"),
    WEST_AREA("west_area"),
    EAST_AREA("east_area");

    private final String value;

    Area(String value) {
        this.value = value;
    }

    public String getValue() {
        return value;
    }
}
