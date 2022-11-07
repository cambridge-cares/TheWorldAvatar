package uk.ac.cam.cares.jps.agent.historicalhouse45utilitiesagent;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class BuildingIRISingletonTest {
    private static final String citygml = "CityGML_Building_123";
    private static final String building = "Building_5515";
    private static final String firstFloor = "Storey_5781";
    private static final String groundFloor = "Storey_162";
    private static final String attic = "Storey_836";

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
        singleton.setOntoCityGmlBuildingIri(building);
        assertEquals(building, singleton.getOntoCityGmlBuildingIri());
    }

    @Test
    void testGetAndSetGroundFloorIri() {
        BuildingIRISingleton singleton = BuildingIRISingleton.getInstance();
        singleton.setOntoCityGmlBuildingIri(groundFloor);
        assertEquals(groundFloor, singleton.getOntoCityGmlBuildingIri());
    }

    @Test
    void testGetAndSetFirstFloorIri() {
        BuildingIRISingleton singleton = BuildingIRISingleton.getInstance();
        singleton.setOntoCityGmlBuildingIri(firstFloor);
        assertEquals(firstFloor, singleton.getOntoCityGmlBuildingIri());
    }

    @Test
    void testGetAndSetAtticIri() {
        BuildingIRISingleton singleton = BuildingIRISingleton.getInstance();
        singleton.setOntoCityGmlBuildingIri(attic);
        assertEquals(attic, singleton.getOntoCityGmlBuildingIri());
    }
}