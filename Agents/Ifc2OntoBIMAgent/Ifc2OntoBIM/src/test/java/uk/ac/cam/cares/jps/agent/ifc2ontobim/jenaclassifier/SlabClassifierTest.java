package uk.ac.cam.cares.jps.agent.ifc2ontobim.jenaclassifier;

import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.rdf.model.Statement;
import org.apache.jena.rdf.model.StmtIterator;
import org.apache.jena.vocabulary.RDF;
import org.junit.jupiter.api.Test;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.JunitTestUtils;

import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;

class SlabClassifierTest {
    private static LinkedHashSet<Statement> testSet;
    private static final String inst = "IfcSlab_1634";
    private static final String secondInst = "IfcSlab_156";
    private static final String floorClass = "Floor";
    private static final String roofClass = "Roof";

    @Test
    void testAddClassMappingForFloor() {
        genSampleFloorStatements();
        Map<String, String> sampleMap = new HashMap<>();
        SlabClassifier.addClassMapping("bim:" + floorClass, testSet, sampleMap);
        assertTrue(sampleMap.size() == 2);
        assertEquals(floorClass, sampleMap.get(inst));
        assertEquals(floorClass, sampleMap.get(secondInst));
    }

    @Test
    void testAddClassMappingForRoof() {
        genSampleRoofStatements();
        Map<String, String> sampleMap = new HashMap<>();
        SlabClassifier.addClassMapping("bim:" + roofClass, testSet, sampleMap);
        assertTrue(sampleMap.size() == 2);
        assertEquals(roofClass, sampleMap.get(inst));
        assertEquals(roofClass, sampleMap.get(secondInst));
    }

    @Test
    void testAddClassMappingEmptySet() {
        Map<String, String> sampleMap = new HashMap<>();
        testSet = new LinkedHashSet<>();
        SlabClassifier.addClassMapping("bim:" + roofClass,testSet, sampleMap);
        // No mapping is generated as there is no values
        assertTrue(sampleMap.size() == 0);
    }

    private void genSampleFloorStatements() {
        Model sampleModel = ModelFactory.createDefaultModel();
        // Generate the statements in the model
        sampleModel.createResource(JunitTestUtils.bimUri + inst)
                .addProperty(RDF.type,
                        sampleModel.createResource(JunitTestUtils.bimUri + floorClass));
        sampleModel.createResource(JunitTestUtils.bimUri + secondInst)
                .addProperty(RDF.type,
                        sampleModel.createResource(JunitTestUtils.bimUri + floorClass));
        addStatementToSet(sampleModel);
    }

    private void genSampleRoofStatements() {
        Model sampleModel = ModelFactory.createDefaultModel();
        // Generate the statements in the model
        sampleModel.createResource(JunitTestUtils.bimUri + inst)
                .addProperty(RDF.type,
                        sampleModel.createResource(JunitTestUtils.bimUri + roofClass));
        sampleModel.createResource(JunitTestUtils.bimUri + secondInst)
                .addProperty(RDF.type,
                        sampleModel.createResource(JunitTestUtils.bimUri + roofClass));
        addStatementToSet(sampleModel);
    }

    private void addStatementToSet(Model sampleModel) {
        // Extract the statements into a LinkedHashSet
        testSet = new LinkedHashSet<>();
        StmtIterator iter = sampleModel.listStatements();
        while (iter.hasNext()) {
            Statement stmt = iter.nextStatement();
            testSet.add(stmt);
        }
    }
}