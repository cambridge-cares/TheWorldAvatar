package uk.ac.cam.cares.jps.agent.ifc2ontobim.ifc2x3.geom;

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

class RectangleProfileDefinitionTest {
    private static final String testBaseUri = "https://www.example.org/";
    private static final String testProfileType = JunitTestUtils.ifc2x3Uri + "AREA";
    private static final String testPlacementIRI = testBaseUri + "IfcLocalPlacement_235";
    private static final Double testXDim = 2.61;
    private static final Double testYDim = 0.12;


    @BeforeEach
    void createNamespace() {
        NamespaceMapper.setBaseNameSpace(testBaseUri);
    }

    @AfterAll
    static void resetNamespace() {
        NamespaceMapper.setBaseNameSpace("");
    }

    @Test
    void testConstructor() {
        RectangleProfileDefinition sample = new RectangleProfileDefinition(testProfileType, testPlacementIRI, testXDim.toString(), testYDim.toString());
        RectangleProfileDefinition sample2 = new RectangleProfileDefinition(testProfileType, testPlacementIRI, testXDim.toString(), testYDim.toString());
        // Test that the created samples are different
        assertNotEquals(sample, sample2);
        assertNotEquals(sample.getIri(), sample2.getIri());
    }

    @Test
    void testConstructStatements() {
        // Set up
        LinkedHashSet<Statement> sampleSet = new LinkedHashSet<>();
        RectangleProfileDefinition sample = new RectangleProfileDefinition(testProfileType, testPlacementIRI, testXDim.toString(), testYDim.toString());
        // Execute method
        sample.constructStatements(sampleSet);
        // Clean up results as one string
        String result = JunitTestUtils.appendStatementsAsString(sampleSet);
        // Generated expected statement lists and verify their existence
        JunitTestUtils.doesExpectedListExist(genExpectedStatements(), result);
    }

    private List<String> genExpectedStatements() {
        List<String> expected = new ArrayList<>();
        expected.add(testBaseUri + "RectangleProfileDefinition_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, https://www.w3.org/1999/02/22-rdf-syntax-ns#type, https://www.theworldavatar.com/kg/ontobim/RectangleProfileDefinition");
        expected.add(testBaseUri + "RectangleProfileDefinition_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, https://www.theworldavatar.com/kg/ontobim/hasProfileType, " + testProfileType);
        expected.add(testBaseUri + "RectangleProfileDefinition_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, https://www.theworldavatar.com/kg/ontobim/hasLocalPosition, " + testPlacementIRI);
        expected.add(testBaseUri + "RectangleProfileDefinition_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, https://www.theworldavatar.com/kg/ontobim/hasXDimensionExtent, \"" + testXDim);
        expected.add(testBaseUri + "RectangleProfileDefinition_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, https://www.theworldavatar.com/kg/ontobim/hasYDimensionExtent, \"" + testYDim);
        return expected;
    }
}