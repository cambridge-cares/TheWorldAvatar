package uk.ac.cam.cares.jps.agent.ifc2ontobim.ifcparser.storage;

import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.JunitTestUtils;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.ifc2x3.model.IfcProjectRepresentation;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.ifc2x3.zone.*;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.ifcparser.storage.SpatialZoneStorage;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.utils.NamespaceMapper;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

import static org.junit.jupiter.api.Assertions.*;

class SpatialZoneStorageTest {
    private static SpatialZoneStorage testMappings;
    private static final String TEST_BASE_URI = "http://www.example.org/";
    private static final String NON_EXISTENT_IRI = TEST_BASE_URI + "DOES/NOT/EXIST_1223";
    private static final String TEST_ZONE_PLACEMENT_IRI = TEST_BASE_URI + JunitTestUtils.BIM_PLACEMENT_CLASS + "_532";
    private static final String TEST_PROJECT_CLASS = "IfcProjectRepresentation";
    private static final String TEST_PROJECT_IRI = TEST_BASE_URI + TEST_PROJECT_CLASS + "_12";
    private static final String TEST_SITE_CLASS = "IfcSiteRepresentation";
    private static final String TEST_BUILDING_CLASS = "IfcBuildingRepresentation";
    private static final String TEST_STOREY_CLASS = "IfcStoreyRepresentation";
    private static final String TEST_ROOM_CLASS = "IfcRoomRepresentation";
    private static final String TEST_SITE_IRI = TEST_BASE_URI + TEST_SITE_CLASS + "_142";
    private static final String TEST_BUILDING_IRI = TEST_BASE_URI + TEST_BUILDING_CLASS + "_214";
    private static final String TEST_STOREY_IRI = TEST_BASE_URI + TEST_STOREY_CLASS + "_315";
    private static final String TEST_ROOM_IRI = TEST_BASE_URI + TEST_ROOM_CLASS + "_462";
    private static final String TEST_SITE_NAME = "Free land";
    private static final String TEST_SITE_UID = "9135e13";
    private static final String TEST_BUILDING_NAME = "Building host";
    private static final String TEST_BUILDING_UID = "018fkjr18";
    private static final String TEST_STOREY_NAME = "First Floor";
    private static final String TEST_STOREY_UID = "e0asf781";
    private static final String TEST_ROOM_NAME = "Kitchen";
    private static final String TEST_ROOM_UID = "lu1276r9a";
    private static final Double TEST_ELEV = 125.0;
    private static final String NON_EXISTING_ERROR = NON_EXISTENT_IRI + " does not exist in mappings!";

    @BeforeAll
    static void genSingleton() {
        testMappings = SpatialZoneStorage.Singleton();
        NamespaceMapper.setBaseNameSpace(TEST_BASE_URI);
    }

    @BeforeEach
    void resetSingleton() {
        SpatialZoneStorage.resetSingleton();
    }

    @AfterAll
    static void resetNamespace(){ NamespaceMapper.setBaseNameSpace("");}

    @Test
    void testGetZoneFail() {
        JPSRuntimeException thrownError = assertThrows(JPSRuntimeException.class, () -> testMappings.getZone(NON_EXISTENT_IRI));
        assertEquals(NON_EXISTING_ERROR, thrownError.getMessage());
    }

    @Test
    void testAddAndGetProject() {
        // Create a new sample representation
        IfcProjectRepresentation sampleProject = new IfcProjectRepresentation(null, null, null);
        // Execute method
        testMappings.add(TEST_PROJECT_IRI, sampleProject.getIri());
        // Assert if they are equals
        assertEquals(sampleProject.getIri(), testMappings.getZone(TEST_PROJECT_IRI));
    }

    @Test
    void testAddAndGetSite() {
        // Create a new sample representation
        IfcSiteRepresentation sampleSite = new IfcSiteRepresentation(TEST_SITE_NAME, TEST_SITE_UID, TEST_ZONE_PLACEMENT_IRI, null, null, null, TEST_ELEV.toString(), null);
        // Execute method
        testMappings.add(TEST_SITE_IRI, sampleSite.getBotSiteIRI());
        // Assert if they are equals
        assertEquals(sampleSite.getBotSiteIRI(), testMappings.getZone(TEST_SITE_IRI));
    }

    @Test
    void testAddAndGetBuilding() {
        // Create a new sample representation
        IfcBuildingRepresentation sampleBuilding = new IfcBuildingRepresentation(TEST_BUILDING_NAME, TEST_BUILDING_UID,
                TEST_ZONE_PLACEMENT_IRI, null, TEST_SITE_IRI, TEST_ELEV.toString(), TEST_ELEV.toString(), null);
        // Execute method
        testMappings.add(TEST_BUILDING_IRI, sampleBuilding.getBotBuildingIRI());
        // Assert if they are equals
        assertEquals(sampleBuilding.getBotBuildingIRI(), testMappings.getZone(TEST_BUILDING_IRI));
    }

    @Test
    void testAddAndGetStorey() {
        // Create a new sample representation
        IfcStoreyRepresentation sampleStorey = new IfcStoreyRepresentation(TEST_STOREY_NAME,
                TEST_STOREY_UID, TEST_ZONE_PLACEMENT_IRI, TEST_BUILDING_IRI, TEST_ELEV.toString(), null);
        // Execute method
        testMappings.add(TEST_STOREY_IRI, sampleStorey.getBotStoreyIRI());
        // Assert if they are equals
        assertEquals(sampleStorey.getBotStoreyIRI(), testMappings.getZone(TEST_STOREY_IRI));
    }

    @Test
    void testAddAndGetRoom() {
        // Create a new sample representation
        IfcRoomRepresentation sampleRoom = new IfcRoomRepresentation(TEST_ROOM_NAME,
                TEST_ROOM_UID, TEST_ZONE_PLACEMENT_IRI, TEST_STOREY_IRI);
        // Execute method
        testMappings.add(TEST_ROOM_IRI, sampleRoom.getBimRoomIRI());
        // Assert if they are equals
        assertEquals(sampleRoom.getBimRoomIRI(), testMappings.getZone(TEST_ROOM_IRI));
    }
}