package uk.ac.cam.cares.jps.agent.ifc2ontobim.ifc2x3.element;

import org.apache.jena.rdf.model.Statement;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.JunitTestUtils;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.utils.NamespaceMapper;

import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

class IfcModelRepresentationTest {
    private static final String testBaseUri = "http://www.example.org/";
    private static final String testIri1 = testBaseUri + "IfcDoor_332";
    private static final String testName1 = "Wooden Door";
    private static final String testUID1 = "01294ha";
    private static final String testPlacementIriVal = "515";
    private static final String testPlacementIri1 = testBaseUri + "IfcLocalPlacement_" + testPlacementIriVal;
    private static final String testBimPlacementIri = testBaseUri + "LocalPlacement_" + testPlacementIriVal;
    private static final String testIri2 = testBaseUri + "IfcWall_322";
    private static final String testName2 = "Concrete Wall";
    private static final String testUID2 = "9sdg781";
    private static final String testPlacementIri2 = testBaseUri + "LocalPlacement_8172";
    private static final String testClassName = "IfcModelRepresentation";
    private static final String testClass = JunitTestUtils.bimUri + testClassName;

    @BeforeAll
    static void createNamespace(){ NamespaceMapper.setBaseNameSpace(testBaseUri); }
    @AfterAll
    static void resetNamespace(){
        NamespaceMapper.setBaseNameSpace("");
    }

    @Test
    void testConstructor() {
        IfcModelRepresentation sample = new IfcModelRepresentation(testName1, testUID1, testPlacementIri1);
        // Test that the sample fields are correct
        assertEquals(testBaseUri, sample.getPrefix());
        assertNotEquals(testIri1, sample.getIfcRepIri());
        assertTrue(sample.getIfcRepIri().contains(testBaseUri + testClassName + "_"));
        assertEquals(testName1, sample.getName());
        assertEquals(testUID1, sample.getUid());
        assertEquals(testBimPlacementIri, sample.getPlacementIri());

        IfcModelRepresentation sample2 = new IfcModelRepresentation(testName2, testUID2, testPlacementIri2);
        // Test that the sample fields are correct
        assertEquals(testBaseUri, sample2.getPrefix());
        assertNotEquals(testIri2, sample2.getIfcRepIri());
        assertTrue(sample2.getIfcRepIri().contains(testBaseUri + testClassName + "_"));
        assertEquals(testName2, sample2.getName());
        assertEquals(testUID2, sample2.getUid());
        assertEquals(testPlacementIri2, sample2.getPlacementIri());
    }

    @Test
    void testAddIfcModelRepresentationStatements() {
        // Set up
        LinkedHashSet<Statement> sampleSet = new LinkedHashSet<>();
        IfcModelRepresentation sample = new IfcModelRepresentation(testName1, testUID1, testPlacementIri1);
        // Execute method
        sample.addIfcModelRepresentationStatements(sampleSet);
        // Clean up results as one string
        String result = JunitTestUtils.appendStatementsAsString(sampleSet);
        // Generated expected statement lists and verify their existence
        JunitTestUtils.doesExpectedListExist(genExpectedCommonStatements(), result);
    }

    private List<String> genExpectedCommonStatements() {
        List<String> expected = new ArrayList<>();
        expected.add(testBaseUri + "IfcModelRepresentation_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.w3.org/1999/02/22-rdf-syntax-ns#type, " + testClass);
        expected.add(testBaseUri + "IfcModelRepresentation_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.w3.org/2000/01/rdf-schema#label, \"" + testName1);
        expected.add(testBaseUri + "IfcModelRepresentation_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasIfcId, \"" + testUID1);
        expected.add(testBaseUri + "IfcModelRepresentation_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasLocalPosition, " + testBimPlacementIri);
        return expected;
    }
}