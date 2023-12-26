package uk.ac.cam.cares.jps.agent.ceavisualisation;

import java.util.Set;

enum Column {
    BUILDING("gfa", Set.of(Annual.GRID, Annual.ELECTRICITY, Annual.HEATING, Annual.COOLING)),
    ROOF("roof_area", Set.of(Annual.PV_ROOF, Annual.PVT_PLATE_E_ROOF, Annual.PVT_PLATE_Q_ROOF, Annual.PVT_TUBE_E_ROOF, Annual.PVT_TUBE_Q_ROOF, Annual.SC_PLATE_ROOF, Annual.SC_TUBE_ROOF)),
    NORTH("north_area", Set.of(Annual.PV_NORTH, Annual.PVT_PLATE_E_NORTH, Annual.PVT_PLATE_Q_NORTH, Annual.PVT_TUBE_E_NORTH, Annual.PVT_TUBE_Q_NORTH, Annual.SC_PLATE_NORTH, Annual.SC_TUBE_NORTH)),
    SOUTH("south_area", Set.of(Annual.PV_SOUTH, Annual.PVT_PLATE_E_SOUTH, Annual.PVT_PLATE_Q_SOUTH, Annual.PVT_TUBE_E_SOUTH, Annual.PVT_TUBE_Q_SOUTH, Annual.SC_PLATE_SOUTH, Annual.SC_TUBE_SOUTH)),
    WEST("west_area", Set.of(Annual.PV_WEST, Annual.PVT_PLATE_E_WEST, Annual.PVT_PLATE_Q_WEST, Annual.PVT_TUBE_E_WEST, Annual.PVT_TUBE_Q_WEST, Annual.SC_PLATE_WEST, Annual.SC_TUBE_WEST)),
    EAST("east_area", Set.of(Annual.PV_EAST, Annual.PVT_PLATE_E_EAST, Annual.PVT_PLATE_Q_EAST, Annual.PVT_TUBE_E_EAST, Annual.PVT_TUBE_Q_EAST, Annual.SC_PLATE_EAST, Annual.SC_TUBE_EAST));

    private final String area;
    private final Set<Annual> annuals;

    Column(String area, Set<Annual> annuals) {
        this.area = area;
        this.annuals = annuals;
    }

    public String getArea() {
        return this.area;
    }

    public Set<Annual> getAnnuals() {
        return this.annuals;
    }
}
