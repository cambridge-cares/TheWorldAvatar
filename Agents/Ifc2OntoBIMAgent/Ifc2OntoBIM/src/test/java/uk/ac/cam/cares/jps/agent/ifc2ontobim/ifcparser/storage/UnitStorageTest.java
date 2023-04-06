package uk.ac.cam.cares.jps.agent.ifc2ontobim.ifcparser.storage;

import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.JunitTestUtils;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.ifc2x3.Unit;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.utils.NamespaceMapper;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

import static org.junit.jupiter.api.Assertions.*;

class UnitStorageTest {
    private static UnitStorage testMappings;
    private static final String TEST_BASE_URI = "http://www.example.org/";
    private static final String NON_EXISTENT_IRI = TEST_BASE_URI + "DOES/NOT/EXIST_1223";
    private static final String LENGTH_IDENTIFIER = "meter";
    private static final String LENGTH = "Length";
    private static final String LENGTH_CLASS = JunitTestUtils.omUri + LENGTH;
    private static final String LENGTH_SYMBOL = "m";
    private static final String NON_EXISTING_ERROR = NON_EXISTENT_IRI + " does not exist in mappings!";

    @BeforeAll
    static void genSingleton() {
        testMappings = UnitStorage.Singleton();
        NamespaceMapper.setBaseNameSpace(TEST_BASE_URI);
    }

    @BeforeEach
    void resetSingleton() {
        SpatialZoneStorage.resetSingleton();
    }

    @AfterAll
    static void resetNamespace(){ NamespaceMapper.setBaseNameSpace("");}

    @Test
    void testGetIriFail() {
        JPSRuntimeException thrownError = assertThrows(JPSRuntimeException.class, () -> testMappings.getIri(NON_EXISTENT_IRI));
        assertEquals(NON_EXISTING_ERROR, thrownError.getMessage());
    }

    @Test
    void testAddAndGetIri() {
        // Create a new sample representation
        Unit sample = new Unit(LENGTH_CLASS, LENGTH_SYMBOL);
        // Execute method
        testMappings.add(LENGTH_IDENTIFIER, sample.getIri());
        // Assert if they are equals
        assertEquals(sample.getIri(), testMappings.getIri(LENGTH_IDENTIFIER));
    }
}