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

class CoveringClassifierTest {
    private static LinkedHashSet<Statement> testSet;
    private static final String bimUri = "http://www.theworldavatar.com/ontology/ontobim/ontoBIM#";
    private static final String inst = "IfcCovering_1634";
    private static final String secondInst = "IfcCovering_156";
    private static final String ceilingClass = "Ceiling";

    @BeforeEach
    void genSampleStatements() {
        Model sampleModel = ModelFactory.createDefaultModel();
        // Generate the statements in the model
        sampleModel.createResource(bimUri + inst)
                .addProperty(RDF.type,
                        sampleModel.createResource(bimUri + ceilingClass));
        sampleModel.createResource(bimUri + secondInst)
                .addProperty(RDF.type,
                        sampleModel.createResource(bimUri + ceilingClass));
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
        CoveringClassifier.addClassMapping("bim:" + ceilingClass, testSet, sampleMap);
        assertTrue(sampleMap.size() == 2);
        assertEquals(ceilingClass, sampleMap.get(inst));
        assertEquals(ceilingClass, sampleMap.get(secondInst));
    }

    @Test
    void testAddClassMappingEmptySet() {
        Map<String, String> sampleMap = new HashMap<>();
        testSet = new LinkedHashSet<>();
        CoveringClassifier.addClassMapping("bim:" + ceilingClass,testSet, sampleMap);
        // No mapping is generated as there is no values
        assertTrue(sampleMap.size() == 0);
    }
}