package uk.ac.cam.cares.jps.agent.ifc2ontobim.jenaclassifier;

import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.rdf.model.Statement;
import org.apache.jena.rdf.model.StmtIterator;
import org.apache.jena.vocabulary.RDF;
import org.junit.jupiter.api.Test;

import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;

class StairClassifierTest {
    private static LinkedHashSet<Statement> testSet;
    private static final String bimUri = "http://www.theworldavatar.com/ontology/ontobim/ontoBIM#";
    private static final String inst = "IfcStair_1634";
    private static final String landingInst = "IfcSlab_2256";
    private static final String structComponentInst = "IfcMember_546";
    private static final String predicate = "hasStairSubElement";
    private static final String stairClass = "Stair";
    private static final String landingClass = "Landing";
    private static final String structComponentClass = "StructuralComponent";

    @Test
    void testAddClassMapping() {
        genSampleStatements();
        Map<String, String> sampleMap = new HashMap<>();
        StairClassifier.addClassMapping(testSet, sampleMap);
        assertEquals(landingClass, sampleMap.get(landingInst));
        assertEquals(structComponentClass, sampleMap.get(structComponentInst));
    }

    @Test
    void testAddClassMappingEmptySet() {
        Map<String, String> sampleMap = new HashMap<>();
        testSet = new LinkedHashSet<>();
        StairClassifier.addClassMapping(testSet, sampleMap);
        // No mapping is generated as there is no values
        assertTrue(sampleMap.size() == 0);
    }

    private void genSampleStatements() {
        Model sampleModel = ModelFactory.createDefaultModel();
        // Generate the statements in the model
        sampleModel.createResource(bimUri + inst)
                .addProperty(RDF.type,
                        sampleModel.createResource(bimUri + stairClass));
        sampleModel.createResource(bimUri + inst)
                .addProperty(sampleModel.createProperty(bimUri + predicate),
                        sampleModel.createResource(bimUri + landingInst));
        sampleModel.createResource(bimUri + landingInst)
                .addProperty(RDF.type,
                        sampleModel.createResource(bimUri + landingClass));
        sampleModel.createResource(bimUri + inst)
                .addProperty(sampleModel.createProperty(bimUri + predicate),
                        sampleModel.createResource(bimUri + structComponentInst));
        sampleModel.createResource(bimUri + structComponentInst)
                .addProperty(RDF.type,
                        sampleModel.createResource(bimUri + structComponentClass));
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