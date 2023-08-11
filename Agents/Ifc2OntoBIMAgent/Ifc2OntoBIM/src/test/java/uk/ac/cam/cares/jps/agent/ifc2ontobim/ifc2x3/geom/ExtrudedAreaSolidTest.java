package uk.ac.cam.cares.jps.agent.ifc2ontobim.ifc2x3.geom;

import org.apache.jena.rdf.model.Statement;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.JunitTestUtils;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.ifcparser.OntoBimConstant;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.utils.NamespaceMapper;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.utils.StatementHandler;

import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertNotEquals;

class ExtrudedAreaSolidTest {
    private static final String testBaseUri1 = "https://www.example.org/";
    private static final String testIri1 = testBaseUri1 + "IfcExtrudedAreaSolid_5128";
    private static final String testBIMIri1 = testBaseUri1 + "ExtrudedAreaSolid_5128";
    private static final String testBaseUri2 = "https://www.example.org/test#";
    private static final String testIri2 = testBaseUri2 + "IfcExtrudedAreaSolid_6666";
    private static final String testPlacementIRI = testBaseUri1 + "LocalPlacement_7155";
    private static final String testDirectionIri = testBaseUri1 + "DirectionVector_78317";
    private static final String testProfileDefIRI = testBaseUri1 + "RectangleProfileDefinition_78314";
    private static final Double testDepth = 1.255;


    @BeforeEach
    void createNamespace() {
        NamespaceMapper.setBaseNameSpace(testBaseUri1);
    }

    @AfterAll
    static void resetNamespace() {
        NamespaceMapper.setBaseNameSpace("");
    }

    @Test
    void testConstructor() {
        ExtrudedAreaSolid sample = new ExtrudedAreaSolid(testIri1, testPlacementIRI, testDirectionIri, testDepth.toString(), testProfileDefIRI);
        ExtrudedAreaSolid sample2 = new ExtrudedAreaSolid(testIri2, testPlacementIRI, testDirectionIri, testDepth.toString(), testProfileDefIRI);
        // Test that the created samples are different
        assertNotEquals(sample, sample2);
    }

    @Test
    void testConstructStatements() {
        // Set up
        LinkedHashSet<Statement> sampleSet = new LinkedHashSet<>();
        ExtrudedAreaSolid sample = new ExtrudedAreaSolid(testIri1, testPlacementIRI, testDirectionIri, testDepth.toString(), testProfileDefIRI);
        // Execute method
        sample.constructStatements(sampleSet);
        // Clean up results as one string
        String result = JunitTestUtils.appendStatementsAsString(sampleSet);
        // Generated expected statement lists and verify their existence
        JunitTestUtils.doesExpectedListExist(genExpectedStatements(), result);
    }

    private List<String> genExpectedStatements() {
        List<String> expected = new ArrayList<>();
        expected.add(testBIMIri1 + ", " + JunitTestUtils.RDF_TYPE + ", https://www.theworldavatar.com/kg/ontobim/ExtrudedAreaSolid");
        expected.add(testBIMIri1 + ", https://www.theworldavatar.com/kg/ontobim/hasExtrusionStartPosition, " + testPlacementIRI);
        expected.add(testBIMIri1 + ", https://www.theworldavatar.com/kg/ontobim/hasExtrusionDirection, " + testDirectionIri);
        expected.add(testBIMIri1 + ", https://www.theworldavatar.com/kg/ontobim/hasExtrusionDepth, \"" + testDepth);
        expected.add(testBIMIri1 + ", https://www.theworldavatar.com/kg/ontobim/hasExtrusionProfile, " + testProfileDefIRI);
        return expected;
    }
}