package uk.ac.cam.cares.jps.agent.ifc2ontobim.jenaclassifier;

import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.rdf.model.Statement;
import org.apache.jena.rdf.model.StmtIterator;
import org.apache.jena.vocabulary.RDF;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;

class CompoundAngleMeasureClassifierTest {
    private static LinkedHashSet<Statement> testSet;
    private static final String bimUri = "http://www.theworldavatar.com/ontology/ontobim/ontoBIM#";
    private static final String latInst = "IfcCompoundAngleMeasure_1634";
    private static final String longInst = "IfcCompoundAngleMeasure_156";
    private static final String latClass = "Latitude";
    private static final String longClass = "Longitude";

    @BeforeEach
    void genSampleStatements() {
        Model sampleModel = ModelFactory.createDefaultModel();
        // Generate the statements in the model
        sampleModel.createResource(bimUri + "Site_16")
                .addProperty(sampleModel.createProperty(bimUri + "hasLatitude"),
                        sampleModel.createResource(bimUri + latInst));
        sampleModel.createResource(bimUri + latInst)
                .addProperty(RDF.type,
                        sampleModel.createResource(bimUri + latClass));
        sampleModel.createResource(bimUri + longInst)
                .addProperty(RDF.type,
                        sampleModel.createResource(bimUri + longClass));
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
        assertEquals(latClass, sampleMap.get(latInst));
        assertEquals(longClass, sampleMap.get(longInst));
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