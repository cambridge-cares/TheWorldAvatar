package uk.ac.cam.cares.jps.agent.ifc2ontobim.ifc2x3;

import org.apache.jena.rdf.model.Statement;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.JunitTestUtils;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.utils.NamespaceMapper;

import java.util.LinkedHashSet;

import static org.junit.jupiter.api.Assertions.*;

class UnitTest {
    private static final String TEST_BASE_URI = "https://www.example.org/";

    @BeforeAll
    static void createNamespace(){ NamespaceMapper.setBaseNameSpace(TEST_BASE_URI); }
    @AfterAll
    static void resetNamespace(){ NamespaceMapper.setBaseNameSpace("");}

    @Test
    void testConstructor() {
        Unit sample = new Unit(JunitTestUtils.LENGTH_CLASS, JunitTestUtils.LENGTH_SYMBOL);
        Unit sample2 = new Unit(JunitTestUtils.LENGTH_CLASS, JunitTestUtils.LENGTH_SYMBOL);
        assertNotEquals(sample.getIri(), sample2.getIri());
    }

    @Test
    void testConstructStatements() {
        // Set up
        LinkedHashSet<Statement> sampleSet = new LinkedHashSet<>();
        Unit sample = new Unit(JunitTestUtils.LENGTH_CLASS, JunitTestUtils.LENGTH_SYMBOL);
        // Execute method
        sample.constructStatements(sampleSet);
        // Clean up results as one string
        String result = JunitTestUtils.appendStatementsAsString(sampleSet);
        // Generated expected statement lists and verify their existence
        JunitTestUtils.doesExpectedListExist(JunitTestUtils.genExpectedUnitStatements(TEST_BASE_URI), result);
    }
}