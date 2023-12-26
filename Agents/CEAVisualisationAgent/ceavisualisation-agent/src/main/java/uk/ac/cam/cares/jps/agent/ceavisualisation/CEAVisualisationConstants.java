package uk.ac.cam.cares.jps.agent.ceavisualisation;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public enum CEAVisualisationConstants {
    GRID("grid"),
    ELECTRICITY("electricity"),
    HEATING("heating"),
    COOLING("cooling"),
    GFA("gfa"),
    ROOF_AREA("roof_area"),
    NORTH_AREA("north_area"),
    SOUTH_AREA("south_area"),
    WEST_AREA("west_area"),
    EAST_AREA("east_area"),
    PV_ROOF("pv_roof"),
    PV_NORTH("pv_north"),
    PV_SOUTH("pv_south"),
    PV_WEST("pv_west"),
    PV_EAST("pv_east"),
    PVT_PLATE_Q_ROOF("pvt_plate_q_roof"),
    PVT_PLATE_Q_NORTH("pvt_plate_q_north"),
    PVT_PLATE_Q_SOUTH("pvt_plate_q_south"),
    PVT_PLATE_Q_WEST("pvt_plate_q_west"),
    PVT_PLATE_Q_EAST("pvt_plate_q_east"),
    PVT_PLATE_E_ROOF("pvt_plate_e_roof"),
    PVT_PLATE_E_NORTH("pvt_plate_e_north"),
    PVT_PLATE_E_SOUTH("pvt_plate_e_south"),
    PVT_PLATE_E_WEST("pvt_plate_e_west"),
    PVT_PLATE_E_EAST("pvt_plate_e_east"),
    PVT_TUBE_Q_ROOF("pvt_tube_q_roof"),
    PVT_TUBE_Q_NORTH("pvt_tube_q_north"),
    PVT_TUBE_Q_SOUTH("pvt_tube_q_south"),
    PVT_TUBE_Q_WEST("pvt_tube_q_west"),
    PVT_TUBE_Q_EAST("pvt_tube_q_east"),
    PVT_TUBE_E_ROOF("pvt_tube_e_roof"),
    PVT_TUBE_E_NORTH("pvt_tube_e_north"),
    PVT_TUBE_E_SOUTH("pvt_tube_e_south"),
    PVT_TUBE_E_WEST("pvt_tube_e_west"),
    PVT_TUBE_E_EAST("pvt_tube_e_east"),
    SC_PLATE_ROOF("sc_plate_roof"),
    SC_PLATE_NORTH("sc_plate_north"),
    SC_PLATE_SOUTH("sc_plate_south"),
    SC_PLATE_WEST("sc_plate_west"),
    SC_PLATE_EAST("sc_plate_east"),
    SC_TUBE_ROOF("sc_tube_roof"),
    SC_TUBE_NORTH("sc_tube_north"),
    SC_TUBE_SOUTH("sc_tube_south"),
    SC_TUBE_WEST("sc_tube_west"),
    SC_TUBE_EAST("sc_tube_east");

    private final String value;

    CEAVisualisationConstants(String value) {
        this.value = value;
    }
    
    public String getValue() {
        return value;
    }
}
