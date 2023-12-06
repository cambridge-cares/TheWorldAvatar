package uk.ac.cam.cares.jps.agent.ifc2ontobim.ifc2x3.zone;

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

class IfcAbstractRepresentationTest {
    private static final String testBaseUri = "https://www.example.org/";
    private static final String testIri1 = testBaseUri + "IfcAbstractRepresentation_142";
    private static final String testClassName1 = "IfcSiteRepresentation";
    private static final String testClass = JunitTestUtils.bimUri + testClassName1;
    private static final String testName1 = "Free land";
    private static final String testUID1 = "afi193";
    private static final String testPlacementIriVal = "324";
    private static final String testPlacementIri1 = testBaseUri + "IfcLocalPlacement_" + testPlacementIriVal;
    private static final String testBimPlacementIri = testBaseUri + "LocalPlacement_" + testPlacementIriVal;
    private static final String testIri2 = testBaseUri + "IfcAbstractRepresentation_142";
    private static final String testClassName2 = "IfcStoreyRepresentation";
    private static final String testName2 = "First floor";
    private static final String testUID2 = "b18aqw1";
    private static final String testPlacementIri2 = testBaseUri + "LocalPlacement_5120";

    @BeforeAll
    static void createNamespace(){ NamespaceMapper.setBaseNameSpace(testBaseUri); }
    @AfterAll
    static void resetNamespace(){ NamespaceMapper.setBaseNameSpace("");}

    @Test
    void testConstructor() {
        IfcAbstractRepresentation sample = new IfcAbstractRepresentation(testClassName1, testName1, testUID1, testPlacementIri1);
        // Test that the sample fields are correct
        assertEquals(testBaseUri, sample.getPrefix());
        assertNotEquals(testIri1, sample.getIri());
        assertTrue(sample.getIri().contains(testBaseUri + testClassName1 + "_"));
        assertEquals(testName1, sample.getName());
        assertEquals(testUID1, sample.getUid());
        assertEquals(testBimPlacementIri, sample.getPlacementIri());

        IfcAbstractRepresentation sample2 = new IfcAbstractRepresentation(testClassName2, testName2, testUID2, testPlacementIri2);
        // Test that the sample fields are correct
        assertEquals(testBaseUri, sample2.getPrefix());
        assertNotEquals(testIri2, sample2.getIri());
        assertTrue(sample2.getIri().contains(testBaseUri + testClassName2 + "_"));
        assertEquals(testName2, sample2.getName());
        assertEquals(testUID2, sample2.getUid());
        assertEquals(testPlacementIri2, sample2.getPlacementIri());
    }

    @Test
    void testAddIfcAbstractRepresentationStatements() {
        // Set up
        LinkedHashSet<Statement> sampleSet = new LinkedHashSet<>();
        IfcAbstractRepresentation sample = new IfcAbstractRepresentation(testClassName1, testName1, testUID1, testPlacementIri1);
        // Execute method
        sample.addIfcAbstractRepresentationStatements(sampleSet, testClass);
        // Clean up results as one string
        String result = JunitTestUtils.appendStatementsAsString(sampleSet);
        // Generated expected statement lists and verify their existence
        JunitTestUtils.doesExpectedListExist(genExpectedCommonStatements(true ), result);
    }

    @Test
    void testAddIfcAbstractRepresentationStatementsMissingName() {
        // Set up
        LinkedHashSet<Statement> sampleSet = new LinkedHashSet<>();
        IfcAbstractRepresentation sample = new IfcAbstractRepresentation(testClassName1, "", testUID1, testPlacementIri1);
        // Execute method
        sample.addIfcAbstractRepresentationStatements(sampleSet, testClass);
        // Clean up results as one string
        String result = JunitTestUtils.appendStatementsAsString(sampleSet);
        // Generated expected statement lists and verify their existence
        JunitTestUtils.doesExpectedListExist(genExpectedCommonStatements(false), result);
    }

    private List<String> genExpectedCommonStatements(boolean reqName) {
        List<String> expected = new ArrayList<>();
        expected.add(testBaseUri + "IfcSiteRepresentation_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, " + JunitTestUtils.RDF_TYPE + ", " + testClass);
        if(reqName) expected.add(testBaseUri + "IfcSiteRepresentation_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, " + JunitTestUtils.RDFS_LABEL + ", \"" + testName1);
        expected.add(testBaseUri + "IfcSiteRepresentation_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, https://www.theworldavatar.com/kg/ontobim/hasIfcId, \"" + testUID1);
        expected.add(testBaseUri + "IfcSiteRepresentation_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, https://www.theworldavatar.com/kg/ontobim/hasLocalPosition, " + testBimPlacementIri);
        return expected;
    }
}