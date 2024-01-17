package uk.ac.cam.cares.jps.agent.historicalhouse45utilitiesagent;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class BuildingIRISingletonTest {
    private static final String citygml = "CityGML_Building_123";
    private static final String building = "Building_5515";
    private static final String firstFloor = "Storey_5781";
    private static final String groundFloor = "Storey_162";
    private static final String attic = "Storey_836";
    private static final String elecMeter = "ElectricityMeter_245";
    private static final String waterMeter = "WaterMeter_6572";
    private static final String oilMeter = "OilMeter_3918";

    @Test
    void testSingleInstance() {
        BuildingIRISingleton first = BuildingIRISingleton.getInstance();
        BuildingIRISingleton second = BuildingIRISingleton.getInstance();
        assertEquals(first, second);
    }

    @Test
    void testGetInstance() {
        assertNotNull(BuildingIRISingleton.getInstance());
    }

    @Test
    void testGetAndSetOntoCityGMLBuildingIRI() {
        BuildingIRISingleton singleton = BuildingIRISingleton.getInstance();
        singleton.setOntoCityGmlBuildingIri(citygml);
        assertEquals(citygml, singleton.getOntoCityGmlBuildingIri());
    }

    @Test
    void testGetAndSetBuildingIri() {
        BuildingIRISingleton singleton = BuildingIRISingleton.getInstance();
        singleton.setBuildingIri(building);
        assertEquals(building, singleton.getBuildingIri());
    }

    @Test
    void testGetAndSetGroundFloorIri() {
        BuildingIRISingleton singleton = BuildingIRISingleton.getInstance();
        singleton.setGroundFloorIri(groundFloor);
        assertEquals(groundFloor, singleton.getGroundFloorIri());
    }

    @Test
    void testGetAndSetFirstFloorIri() {
        BuildingIRISingleton singleton = BuildingIRISingleton.getInstance();
        singleton.setFirstFloorIri(firstFloor);
        assertEquals(firstFloor, singleton.getFirstFloorIri());
    }

    @Test
    void testGetAndSetAtticIri() {
        BuildingIRISingleton singleton = BuildingIRISingleton.getInstance();
        singleton.setAtticIri(attic);
        assertEquals(attic, singleton.getAtticIri());
    }

    @Test
    void testGetAndSetElecMeterIri() {
        BuildingIRISingleton singleton = BuildingIRISingleton.getInstance();
        singleton.setElecMeterIri(elecMeter);
        assertEquals(elecMeter, singleton.getElecMeterIri());
    }

    @Test
    void testGetAndSetWaterMeterIri() {
        BuildingIRISingleton singleton = BuildingIRISingleton.getInstance();
        singleton.setWaterMeterIri(waterMeter);
        assertEquals(waterMeter, singleton.getWaterMeterIri());
    }

    @Test
    void testGetAndSetOilMeterIri() {
        BuildingIRISingleton singleton = BuildingIRISingleton.getInstance();
        singleton.setOilMeterIri(oilMeter);
        assertEquals(oilMeter, singleton.getOilMeterIri());
    }
}