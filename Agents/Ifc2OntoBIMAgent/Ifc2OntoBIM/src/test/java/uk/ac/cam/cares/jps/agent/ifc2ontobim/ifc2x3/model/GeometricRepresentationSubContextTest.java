package uk.ac.cam.cares.jps.agent.ifc2ontobim.ifc2x3.model;

import org.apache.jena.rdf.model.Statement;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.JunitTestUtils;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.utils.NamespaceMapper;

import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertNotEquals;

class GeometricRepresentationSubContextTest {
    private static final String testBaseUri1 = "https://www.example.org/";
    private static final String testIri1 = testBaseUri1 + "IfcGeometricRepresentationSubContext_2";
    private static final String testBIMIri1 = testBaseUri1 + "GeometricRepresentationSubContext_2";
    private static final String testBaseUri2 = "https://www.example.org/test#";
    private static final String testIri2 = testBaseUri2 + "IfcGeometricRepresentationSubContext_4";
    private static final String testParentContextIri = testBaseUri1 + "GeometricRepresentationContext_6";
    private static final String testTargetViewIri = JunitTestUtils.ifc2x3Uri + "MODEL_VIEW";
    private static final String testContextType = "Axis";
    private static final String testContextIdentifier = "Model";

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
        // First constructor
        GeometricRepresentationSubContext sample = new GeometricRepresentationSubContext(testIri1, testParentContextIri, testContextType, testContextIdentifier, testTargetViewIri);
        // Second constructor
        NamespaceMapper.setBaseNameSpace(testBaseUri2);
        GeometricRepresentationSubContext sample2 = new GeometricRepresentationSubContext(testIri2, testParentContextIri, testContextType, testContextIdentifier, testTargetViewIri);
        // Test that the created objects are different
        assertNotEquals(sample, sample2);
    }

    @Test
    void constructStatements() {
        // Set up
        LinkedHashSet<Statement> sampleSet = new LinkedHashSet<>();
        GeometricRepresentationSubContext sample = new GeometricRepresentationSubContext(testIri1, testParentContextIri, testContextType, testContextIdentifier, testTargetViewIri);
        // Execute method
        sample.constructStatements(sampleSet);
        // Clean up results as one string
        String result = JunitTestUtils.appendStatementsAsString(sampleSet);
        // Generated expected statement lists and verify their existence
        JunitTestUtils.doesExpectedListExist(genExpectedStatements(), result);
    }

    private List<String> genExpectedStatements() {
        List<String> expected = new ArrayList<>();
        expected.add(testBIMIri1 + ", https://www.w3.org/1999/02/22-rdf-syntax-ns#type, https://www.theworldavatar.com/kg/ontobim/GeometricRepresentationSubContext");
        expected.add(testBIMIri1 + ", https://www.theworldavatar.com/kg/ontobim/hasParentContext, " + testParentContextIri);
        expected.add(testBIMIri1 + ", https://www.theworldavatar.com/kg/ontobim/hasContextType, \"" + testContextType);
        expected.add(testBIMIri1 + ", https://www.theworldavatar.com/kg/ontobim/hasContextIdentifier, \"" + testContextIdentifier);
        expected.add(testBIMIri1 + ", https://www.theworldavatar.com/kg/ontobim/hasTargetView, " + testTargetViewIri);
        return expected;
    }
}