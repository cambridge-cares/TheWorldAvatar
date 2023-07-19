package uk.ac.cam.cares.jps.agent.cea;

import java.util.Arrays;
import java.util.List;

public class CEAConstants {
    public static final String KEY_GRID_CONSUMPTION = "GridConsumption";
    public static final String KEY_ELECTRICITY_CONSUMPTION = "ElectricityConsumption";
    public static final String KEY_HEATING_CONSUMPTION = "HeatingConsumption";
    public static final String KEY_COOLING_CONSUMPTION = "CoolingConsumption";
    public static final String KEY_ROOF_SOLAR_SUITABLE_AREA = "RoofSolarSuitableArea";
    public static final String KEY_SOUTH_WALL_SOLAR_SUITABLE_AREA = "SouthWallSolarSuitableArea";
    public static final String KEY_NORTH_WALL_SOLAR_SUITABLE_AREA = "NorthWallSolarSuitableArea";
    public static final String KEY_EAST_WALL_SOLAR_SUITABLE_AREA = "EastWallSolarSuitableArea";
    public static final String KEY_WEST_WALL_SOLAR_SUITABLE_AREA = "WestWallSolarSuitableArea";
    public static final String KEY_PV_ROOF_SUPPLY= "PVRoofSupply";
    public static final String KEY_PV_WALL_SOUTH_SUPPLY = "PVWallSouthSupply";
    public static final String KEY_PV_WALL_NORTH_SUPPLY = "PVWallNorthSupply";
    public static final String KEY_PV_WALL_EAST_SUPPLY = "PVWallEastSupply";
    public static final String KEY_PV_WALL_WEST_SUPPLY = "PVWallWestSupply";
    public static final String KEY_PVT_PLATE_ROOF_E_SUPPLY = "PVTPlateRoofESupply";
    public static final String KEY_PVT_PLATE_WALL_SOUTH_E_SUPPLY = "PVTPlateWallSouthESupply";
    public static final String KEY_PVT_PLATE_WALL_NORTH_E_SUPPLY = "PVTPlateWallNorthESupply";
    public static final String KEY_PVT_PLATE_WALL_EAST_E_SUPPLY = "PVTPlateWallEastESupply";
    public static final String KEY_PVT_PLATE_WALL_WEST_E_SUPPLY = "PVTPlateWallWestESupply";
    public static final String KEY_PVT_PLATE_ROOF_Q_SUPPLY = "PVTPlateRoofQSupply";
    public static final String KEY_PVT_PLATE_WALL_SOUTH_Q_SUPPLY = "PVTPlateWallSouthQSupply";
    public static final String KEY_PVT_PLATE_WALL_NORTH_Q_SUPPLY = "PVTPlateWallNorthQSupply";
    public static final String KEY_PVT_PLATE_WALL_EAST_Q_SUPPLY = "PVTPlateWallEastQSupply";
    public static final String KEY_PVT_PLATE_WALL_WEST_Q_SUPPLY = "PVTPlateWallWestQSupply";
    public static final String KEY_PVT_TUBE_ROOF_E_SUPPLY = "PVTTubeRoofESupply";
    public static final String KEY_PVT_TUBE_WALL_SOUTH_E_SUPPLY = "PVTTubeWallSouthESupply";
    public static final String KEY_PVT_TUBE_WALL_NORTH_E_SUPPLY = "PVTTubeWallNorthESupply";
    public static final String KEY_PVT_TUBE_WALL_EAST_E_SUPPLY = "PVTTubeWallEastESupply";
    public static final String KEY_PVT_TUBE_WALL_WEST_E_SUPPLY = "PVTTubeWallWestESupply";
    public static final String KEY_PVT_TUBE_ROOF_Q_SUPPLY = "PVTTubeRoofQSupply";
    public static final String KEY_PVT_TUBE_WALL_SOUTH_Q_SUPPLY = "PVTTubeWallSouthQSupply";
    public static final String KEY_PVT_TUBE_WALL_NORTH_Q_SUPPLY = "PVTTubeWallNorthQSupply";
    public static final String KEY_PVT_TUBE_WALL_EAST_Q_SUPPLY = "PVTTubeWallEastQSupply";
    public static final String KEY_PVT_TUBE_WALL_WEST_Q_SUPPLY = "PVTTubeWallWestQSupply";
    public static final String KEY_THERMAL_PLATE_ROOF_SUPPLY= "ThermalPlateRoofSupply";
    public static final String KEY_THERMAL_PLATE_WALL_SOUTH_SUPPLY = "ThermalPlateWallSouthSupply";
    public static final String KEY_THERMAL_PLATE_WALL_NORTH_SUPPLY = "ThermalPlateWallNorthSupply";
    public static final String KEY_THERMAL_PLATE_WALL_EAST_SUPPLY = "ThermalPlateWallEastSupply";
    public static final String KEY_THERMAL_PLATE_WALL_WEST_SUPPLY = "ThermalPlateWallWestSupply";
    public static final String KEY_THERMAL_TUBE_ROOF_SUPPLY= "ThermalTubeRoofSupply";
    public static final String KEY_THERMAL_TUBE_WALL_SOUTH_SUPPLY = "ThermalTubeWallSouthSupply";
    public static final String KEY_THERMAL_TUBE_WALL_NORTH_SUPPLY = "ThermalTubeWallNorthSupply";
    public static final String KEY_THERMAL_TUBE_WALL_EAST_SUPPLY = "ThermalTubeWallEastSupply";
    public static final String KEY_THERMAL_TUBE_WALL_WEST_SUPPLY = "ThermalTubeWallWestSupply";

    public static final List<String> TIME_SERIES = Arrays.asList(KEY_GRID_CONSUMPTION,KEY_ELECTRICITY_CONSUMPTION,KEY_HEATING_CONSUMPTION,KEY_COOLING_CONSUMPTION, KEY_PV_ROOF_SUPPLY, KEY_PV_WALL_NORTH_SUPPLY, KEY_PV_WALL_SOUTH_SUPPLY, KEY_PV_WALL_EAST_SUPPLY, KEY_PV_WALL_WEST_SUPPLY, KEY_PVT_PLATE_ROOF_E_SUPPLY, KEY_PVT_PLATE_WALL_NORTH_E_SUPPLY, KEY_PVT_PLATE_WALL_SOUTH_E_SUPPLY, KEY_PVT_PLATE_WALL_EAST_E_SUPPLY, KEY_PVT_PLATE_WALL_WEST_E_SUPPLY, KEY_PVT_PLATE_ROOF_Q_SUPPLY, KEY_PVT_PLATE_WALL_NORTH_Q_SUPPLY, KEY_PVT_PLATE_WALL_SOUTH_Q_SUPPLY, KEY_PVT_PLATE_WALL_EAST_Q_SUPPLY, KEY_PVT_PLATE_WALL_WEST_Q_SUPPLY, KEY_PVT_TUBE_ROOF_E_SUPPLY, KEY_PVT_TUBE_WALL_NORTH_E_SUPPLY, KEY_PVT_TUBE_WALL_SOUTH_E_SUPPLY, KEY_PVT_TUBE_WALL_EAST_E_SUPPLY, KEY_PVT_TUBE_WALL_WEST_E_SUPPLY, KEY_PVT_TUBE_ROOF_Q_SUPPLY, KEY_PVT_TUBE_WALL_NORTH_Q_SUPPLY, KEY_PVT_TUBE_WALL_SOUTH_Q_SUPPLY, KEY_PVT_TUBE_WALL_EAST_Q_SUPPLY, KEY_PVT_TUBE_WALL_WEST_Q_SUPPLY, KEY_THERMAL_PLATE_ROOF_SUPPLY, KEY_THERMAL_PLATE_WALL_NORTH_SUPPLY, KEY_THERMAL_PLATE_WALL_SOUTH_SUPPLY, KEY_THERMAL_PLATE_WALL_EAST_SUPPLY, KEY_THERMAL_PLATE_WALL_WEST_SUPPLY, KEY_THERMAL_TUBE_ROOF_SUPPLY, KEY_THERMAL_TUBE_WALL_NORTH_SUPPLY, KEY_THERMAL_TUBE_WALL_SOUTH_SUPPLY, KEY_THERMAL_TUBE_WALL_EAST_SUPPLY, KEY_THERMAL_TUBE_WALL_WEST_SUPPLY);
    public static final List<String> SCALARS = Arrays.asList(KEY_ROOF_SOLAR_SUITABLE_AREA, KEY_SOUTH_WALL_SOLAR_SUITABLE_AREA, KEY_NORTH_WALL_SOLAR_SUITABLE_AREA, KEY_EAST_WALL_SOLAR_SUITABLE_AREA, KEY_WEST_WALL_SOLAR_SUITABLE_AREA);

}
