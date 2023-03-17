package uk.ac.cam.cares.jps.agent.ifc2ontobim.ifc2x3.model;

import org.apache.jena.rdf.model.Statement;
import org.junit.jupiter.api.Test;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.JunitTestUtils;

import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

class CartesianPointTest {
    private static final String testBaseUri1 = "http://www.example.org/";
    private static final String testIri1 = testBaseUri1 + "IfcCartesianPoint_512";
    private static final String testBaseUri2 = "http://www.example.org/test#";
    private static final String testIri2 = testBaseUri2 + "IfcCartesianPoint_1322";
    private static final String testClassName = "CartesianPoint";
    private static final Double testXCoord1 = 2.0;
    private static final Double testYCoord1 = 3.15;
    private static final Double testZCoord1 = 1.2139;
    private static final Double testXCoord2 = 4.319;
    private static final Double testYCoord2 = 5.51;

    @Test
    void testConstructor() {
        // First constructor
        CartesianPoint sample = new CartesianPoint(testIri1, testXCoord1.toString(), testYCoord1.toString(), testZCoord1.toString());
        // Test that the sample fields are correct
        assertTrue(sample.getIri().contains(testBaseUri1 + testClassName + "_"));
        Double[] sampleCoords = sample.getCoordinates();
        assertEquals(testXCoord1, sampleCoords[0]);
        assertEquals(testYCoord1, sampleCoords[1]);
        assertEquals(testZCoord1, sampleCoords[2]);
        // Second constructor
        CartesianPoint sample2 = new CartesianPoint(testIri2, testXCoord2.toString(), testYCoord2.toString(), null);
        assertTrue(sample2.getIri().contains(testBaseUri2 + testClassName + "_"));
        Double[] sample2Coords = sample2.getCoordinates();
        assertEquals(testXCoord2, sample2Coords[0]);
        assertEquals(testYCoord2, sample2Coords[1]);
        assertNull(sample2Coords[2]);
    }

    @Test
    void constructStatements() {
        // Set up
        LinkedHashSet<Statement> sampleSet = new LinkedHashSet<>();
        CartesianPoint sample = new CartesianPoint(testIri1, testXCoord1.toString(), testYCoord1.toString(), testZCoord1.toString());
        // Execute method
        sample.constructStatements(sampleSet);
        // Clean up results as one string
        String result = JunitTestUtils.appendStatementsAsString(sampleSet);
        // Generated expected statement lists and verify their existence
        JunitTestUtils.doesExpectedListExist(genExpectedStatements(testBaseUri1, testXCoord1, testYCoord1), result);
        JunitTestUtils.doesExpectedListExist(genExpectedOptionalStatements(testBaseUri1, testZCoord1), result);
    }

    @Test
    void constructStatementsOptionalValues() {
        // Set up
        LinkedHashSet<Statement> sampleSet = new LinkedHashSet<>();
        CartesianPoint sample = new CartesianPoint(testIri1, testXCoord1.toString(), testYCoord1.toString(), null);
        // Execute method
        sample.constructStatements(sampleSet);
        // Clean up results as one string
        String result = JunitTestUtils.appendStatementsAsString(sampleSet);
        // Generated expected statement lists and verify their existence
        JunitTestUtils.doesExpectedListExist(genExpectedStatements(testBaseUri1, testXCoord1, testYCoord1), result);
        // Verify the z coordinate statement is not generated
        JunitTestUtils.doesExpectedListNotExist(genExpectedOptionalStatements(testBaseUri1, testZCoord1), result);
    }

    private List<String> genExpectedStatements(String baseURI, Double xCoord, Double yCoord) {
        List<String> expected = new ArrayList<>();
        expected.add(baseURI + "CartesianPoint_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.w3.org/1999/02/22-rdf-syntax-ns#type, http://www.theworldavatar.com/kg/ontobim/CartesianPoint");
        expected.add(baseURI + "CartesianPoint_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasXCoordinate, \"" + xCoord);
        expected.add(baseURI + "CartesianPoint_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasYCoordinate, \"" + yCoord);
        return expected;
    }

    private List<String> genExpectedOptionalStatements(String baseURI, Double zCoord) {
        List<String> expected = new ArrayList<>();
        expected.add(baseURI + "CartesianPoint_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasZCoordinate, \"" + zCoord);
        return expected;
    }
}