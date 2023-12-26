package uk.ac.cam.cares.jps.agent.ceavisualisation;

import org.apache.commons.lang3.tuple.Pair;

enum Annual {
    GRID("grid","grid_per_gfa"),
    ELECTRICITY("electricity","electricity_per_gfa"),
    HEATING("heating","heating_per_gfa"),
    COOLING("cooling","cooling_per_gfa"),
    PV_ROOF("pv_roof","pv_roof_per_area"),
    PV_NORTH("pv_north","pv_north_per_area"),
    PV_SOUTH("pv_south","pv_south_per_area"),
    PV_WEST("pv_west","pv_west_per_area"),
    PV_EAST("pv_east","pv_east_per_area"),
    PVT_PLATE_Q_ROOF("pvt_plate_q_roof","pvt_plate_q_roof_per_area"),
    PVT_PLATE_Q_NORTH("pvt_plate_q_north","pvt_plate_q_north_per_area"),
    PVT_PLATE_Q_SOUTH("pvt_plate_q_south","pvt_plate_q_south_per_area"),
    PVT_PLATE_Q_WEST("pvt_plate_q_west","pvt_plate_q_west_per_area"),
    PVT_PLATE_Q_EAST("pvt_plate_q_east","pvt_plate_q_east_per_area"),
    PVT_PLATE_E_ROOF("pvt_plate_e_roof","pvt_plate_e_roof_per_area"),
    PVT_PLATE_E_NORTH("pvt_plate_e_north","pvt_plate_e_north_per_area"),
    PVT_PLATE_E_SOUTH("pvt_plate_e_south","pvt_plate_e_south_per_area"),
    PVT_PLATE_E_WEST("pvt_plate_e_west","pvt_plate_e_west_per_area"),
    PVT_PLATE_E_EAST("pvt_plate_e_east","pvt_plate_e_east_per_area"),
    PVT_TUBE_Q_ROOF("pvt_tube_q_roof","pvt_tube_q_roof_per_area"),
    PVT_TUBE_Q_NORTH("pvt_tube_q_north","pvt_tube_q_north_per_area"),
    PVT_TUBE_Q_SOUTH("pvt_tube_q_south","pvt_tube_q_south_per_area"),
    PVT_TUBE_Q_WEST("pvt_tube_q_west","pvt_tube_q_west_per_area"),
    PVT_TUBE_Q_EAST("pvt_tube_q_east","pvt_tube_q_east_per_area"),
    PVT_TUBE_E_ROOF("pvt_tube_e_roof","pvt_tube_e_roof_per_area"),
    PVT_TUBE_E_NORTH("pvt_tube_e_north","pvt_tube_e_north_per_area"),
    PVT_TUBE_E_SOUTH("pvt_tube_e_south","pvt_tube_e_south_per_area"),
    PVT_TUBE_E_WEST("pvt_tube_e_west","pvt_tube_e_west_per_area"),
    PVT_TUBE_E_EAST("pvt_tube_e_east","pvt_tube_e_east_per_area"),
    SC_PLATE_ROOF("sc_plate_roof","sc_plate_roof_per_area"),
    SC_PLATE_NORTH("sc_plate_north","sc_plate_north_per_area"),
    SC_PLATE_SOUTH("sc_plate_south","sc_plate_south_per_area"),
    SC_PLATE_WEST("sc_plate_west","sc_plate_west_per_area"),
    SC_PLATE_EAST("sc_plate_east","sc_plate_east_per_area"),
    SC_TUBE_ROOF("sc_tube_roof","sc_tube_roof_per_area"),
    SC_TUBE_NORTH("sc_tube_north","sc_tube_north_per_area"),
    SC_TUBE_SOUTH("sc_tube_south","sc_tube_south_per_area"),
    SC_TUBE_WEST("sc_tube_west","sc_tube_west_per_area"),
    SC_TUBE_EAST("sc_tube_east","sc_tube_east_per_area");
    
    private final Pair<String, String> value;
    Annual(String annual, String annualPerArea) {
        this.value = Pair.of(annual, annualPerArea);
    }

    public String getAnnual() {
        return this.value.getKey();
    }

    public String getAnnualPerArea() {
        return this.value.getValue();
    }
}
