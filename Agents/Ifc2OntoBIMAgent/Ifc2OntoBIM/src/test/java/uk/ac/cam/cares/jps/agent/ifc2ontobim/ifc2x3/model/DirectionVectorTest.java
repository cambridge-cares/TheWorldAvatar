package uk.ac.cam.cares.jps.agent.ifc2ontobim.ifc2x3.model;

import org.apache.jena.rdf.model.Statement;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.JunitTestGeometryUtils;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.JunitTestUtils;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.utils.NamespaceMapper;

import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

class DirectionVectorTest {
    private static final String testBaseUri = "http://www.example.org/";
    private static final String testClassName = "DirectionVector";
    private static final Double testXDirRatio1 = 2.0;
    private static final Double testYDirRatio1 = 3.15;
    private static final Double testZDirRatio1 = 1.2139;
    private static final Double testXDirRatio2 = 4.319;
    private static final Double testYDirRatio2 = 5.51;

    @BeforeAll
    static void createNamespace(){ NamespaceMapper.setBaseNameSpace(testBaseUri); }
    @AfterAll
    static void resetNamespace(){
        NamespaceMapper.setBaseNameSpace("");
    }

    @Test
    void testConstructor() {
        // First constructor
        DirectionVector sample = new DirectionVector(testXDirRatio1.toString(), testYDirRatio1.toString(), testZDirRatio1.toString());
        // Test that the sample fields are correct
        assertTrue(sample.getIri().contains(testBaseUri + testClassName + "_"));
        Double[] sampleCoords = sample.getDirRatios();
        assertEquals(testXDirRatio1, sampleCoords[0]);
        assertEquals(testYDirRatio1, sampleCoords[1]);
        assertEquals(testZDirRatio1, sampleCoords[2]);
        // Second constructor
        DirectionVector sample2 = new DirectionVector(testXDirRatio2.toString(), testYDirRatio2.toString(), null);
        assertTrue(sample2.getIri().contains(testBaseUri + testClassName + "_"));
        Double[] sample2Coords = sample2.getDirRatios();
        assertEquals(testXDirRatio2, sample2Coords[0]);
        assertEquals(testYDirRatio2, sample2Coords[1]);
        assertNull(sample2Coords[2]);
    }

    @Test
    void constructStatements() {
        // Set up
        LinkedHashSet<Statement> sampleSet = new LinkedHashSet<>();
        DirectionVector sample = new DirectionVector(testXDirRatio1.toString(), testYDirRatio1.toString(), testZDirRatio1.toString());
        // Execute method
        sample.constructStatements(sampleSet);
        // Clean up results as one string
        String result = JunitTestUtils.appendStatementsAsString(sampleSet);
        // Generated expected statement lists and verify their existence
        JunitTestUtils.doesExpectedListExist(JunitTestGeometryUtils.genExpectedDirectionStatements(testBaseUri, testXDirRatio1, testYDirRatio1, testZDirRatio1, true), result);
    }

    @Test
    void constructStatementsOptionalValues() {
        // Set up
        LinkedHashSet<Statement> sampleSet = new LinkedHashSet<>();
        DirectionVector sample = new DirectionVector(testXDirRatio1.toString(), testYDirRatio1.toString(), null);
        // Execute method
        sample.constructStatements(sampleSet);
        // Clean up results as one string
        String result = JunitTestUtils.appendStatementsAsString(sampleSet);
        // Generated expected statement lists and verify their existence
        JunitTestUtils.doesExpectedListExist(JunitTestGeometryUtils.genExpectedDirectionStatements(testBaseUri, testXDirRatio1, testYDirRatio1, null, true), result);
        // Verify the z coordinate statement is not generated
        JunitTestUtils.doesExpectedListNotExist(genExpectedOptionalStatements(testBaseUri, testZDirRatio1), result);
    }

    private List<String> genExpectedOptionalStatements(String baseURI, Double zCoord) {
        List<String> expected = new ArrayList<>();
        expected.add(baseURI + "DirectionVector_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasZDirectionRatio, \"" + zCoord);
        return expected;
    }
}