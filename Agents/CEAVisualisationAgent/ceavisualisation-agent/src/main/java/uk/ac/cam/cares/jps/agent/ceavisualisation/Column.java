package uk.ac.cam.cares.jps.agent.ceavisualisation;

import java.util.*;

enum Column {
    BUILDING(Area.GFA, Set.of(Annual.GRID, Annual.ELECTRICITY, Annual.HEATING, Annual.COOLING)),
    ROOF(Area.ROOF_AREA, Set.of(Annual.PV_ROOF, Annual.PVT_PLATE_E_ROOF, Annual.PVT_PLATE_Q_ROOF, Annual.PVT_TUBE_E_ROOF, Annual.PVT_TUBE_Q_ROOF, Annual.SC_PLATE_ROOF, Annual.SC_TUBE_ROOF)),
    NORTH(Area.NORTH_AREA, Set.of(Annual.PV_NORTH, Annual.PVT_PLATE_E_NORTH, Annual.PVT_PLATE_Q_NORTH, Annual.PVT_TUBE_E_NORTH, Annual.PVT_TUBE_Q_NORTH, Annual.SC_PLATE_NORTH, Annual.SC_TUBE_NORTH)),
    SOUTH(Area.SOUTH_AREA, Set.of(Annual.PV_SOUTH, Annual.PVT_PLATE_E_SOUTH, Annual.PVT_PLATE_Q_SOUTH, Annual.PVT_TUBE_E_SOUTH, Annual.PVT_TUBE_Q_SOUTH, Annual.SC_PLATE_SOUTH, Annual.SC_TUBE_SOUTH)),
    WEST(Area.WEST_AREA, Set.of(Annual.PV_WEST, Annual.PVT_PLATE_E_WEST, Annual.PVT_PLATE_Q_WEST, Annual.PVT_TUBE_E_WEST, Annual.PVT_TUBE_Q_WEST, Annual.SC_PLATE_WEST, Annual.SC_TUBE_WEST)),
    EAST(Area.EAST_AREA, Set.of(Annual.PV_EAST, Annual.PVT_PLATE_E_EAST, Annual.PVT_PLATE_Q_EAST, Annual.PVT_TUBE_E_EAST, Annual.PVT_TUBE_Q_EAST, Annual.SC_PLATE_EAST, Annual.SC_TUBE_EAST));

    private static final Map<String, Set<Annual>> map = new HashMap<>();

    private static final List<String> columns = new ArrayList<>();

    static {
        for (Column column: values()) {
            map.put(column.area.getValue(), column.annuals);
            columns.add(column.area.getValue());
            for (Annual annual : column.annuals) {
                columns.add(annual.getAnnual());
                columns.add(annual.getAnnualPerArea());

            }
        }
    }

    private final Area area;
    private final Set<Annual> annuals;

     Column(Area area, Set<Annual> annuals) {
        this.area = area;
        this.annuals = annuals;
    }

    public static Set<Annual> getAnnuals(String area) {
        return map.get(area);
    }

    public static List<String> getColumns() {
         return columns;
    }
}
