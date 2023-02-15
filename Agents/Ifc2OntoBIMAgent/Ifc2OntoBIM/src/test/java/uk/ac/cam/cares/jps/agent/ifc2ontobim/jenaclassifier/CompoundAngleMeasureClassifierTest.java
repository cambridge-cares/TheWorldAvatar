package uk.ac.cam.cares.jps.agent.ifc2ontobim.jenaclassifier;

import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.rdf.model.Statement;
import org.apache.jena.rdf.model.StmtIterator;
import org.apache.jena.vocabulary.RDF;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.JunitTestUtils;

import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;

class CompoundAngleMeasureClassifierTest {
    private static LinkedHashSet<Statement> testSet;
    private static final String baseIRI = "http://www.theworldavatar.com/test/";
    private static final String latInst = "IfcCompoundAngleMeasure_1634";
    private static final String longInst = "IfcCompoundAngleMeasure_156";
    private static final String compoundAngleClass = "CompoundPlaneAngle";
    private static final String latInstName = "Latitude";
    private static final String longInstName = "Longitude";

    @BeforeEach
    void genSampleStatements() {
        Model sampleModel = ModelFactory.createDefaultModel();
        // Generate the statements in the model
        sampleModel.createResource(baseIRI + "Site_16")
                .addProperty(RDF.type,
                        sampleModel.createResource(JunitTestUtils.botUri + "Site"))
                .addProperty(sampleModel.createProperty(JunitTestUtils.bimUri + "hasRefLatitude"),
                        sampleModel.createResource(baseIRI + latInst))
                .addProperty(sampleModel.createProperty(JunitTestUtils.bimUri + "hasRefLongitude"),
                        sampleModel.createResource(baseIRI + longInst));
        sampleModel.createResource(baseIRI + latInst)
                .addProperty(RDF.type,
                        sampleModel.createResource(JunitTestUtils.bimUri + compoundAngleClass));
        sampleModel.createResource(baseIRI + longInst)
                .addProperty(RDF.type,
                        sampleModel.createResource(JunitTestUtils.bimUri + compoundAngleClass));
        // Extract the statements into a LinkedHashSet
        testSet = new LinkedHashSet<>();
        StmtIterator iter = sampleModel.listStatements();
        while (iter.hasNext()) {
            Statement stmt = iter.nextStatement();
            testSet.add(stmt);
        }
    }

    @Test
    void testAddClassMapping() {
        Map<String, String> sampleMap = new HashMap<>();
        CompoundAngleMeasureClassifier.addClassMapping(testSet, sampleMap);
        assertTrue(sampleMap.size() == 2);
        assertEquals(latInstName, sampleMap.get(latInst));
        assertEquals(longInstName, sampleMap.get(longInst));
    }

    @Test
    void testAddClassMappingEmptySet() {
        Map<String, String> sampleMap = new HashMap<>();
        testSet = new LinkedHashSet<>();
        CompoundAngleMeasureClassifier.addClassMapping(testSet, sampleMap);
        // No mapping is generated as there is no values
        assertTrue(sampleMap.size() == 0);
    }
}