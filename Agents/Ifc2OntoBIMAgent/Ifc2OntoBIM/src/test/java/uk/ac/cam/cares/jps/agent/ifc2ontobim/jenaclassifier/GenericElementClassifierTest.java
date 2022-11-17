package uk.ac.cam.cares.jps.agent.ifc2ontobim.jenaclassifier;

import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.rdf.model.Statement;
import org.apache.jena.rdf.model.StmtIterator;
import org.apache.jena.vocabulary.RDF;
import org.apache.jena.vocabulary.RDFS;
import org.junit.jupiter.api.Test;

import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;

class GenericElementClassifierTest {
    private static LinkedHashSet<Statement> testSet;
    private static final String bimUri = "http://www.theworldavatar.com/ontology/ontobim/ontoBIM#";
    private static final String furnitureInst = "IfcFurnishingElement_1634";
    private static final String furnishingClass = "IfcFurnishingElement";
    private static final String meterLiteral = "ElectricalMeter";
    private static final String meterClass = "ElectricityMeter";
    private static final String proxyInst = "IfcBuildingElementProxy_2256";
    private static final String proxyClass = "IfcBuildingElementProxy";
    private static final String solarPVLiteral = "Solar Panel";
    private static final String solarPVClass = "SolarPanel";

    @Test
    void testAddClassMappingForFurniture() {
        genSampleStatementForFurniture();
        Map<String, String> sampleMap = new HashMap<>();
        GenericElementClassifier.addClassMapping("bim:" + furnishingClass, testSet, sampleMap);
        assertEquals(meterClass, sampleMap.get(furnitureInst));
    }

    @Test
    void testAddClassMappingForBuildingElementProxy() {
        genSampleStatementForBuildingElementProxy();
        Map<String, String> sampleMap = new HashMap<>();
        GenericElementClassifier.addClassMapping("bim:" + proxyClass, testSet, sampleMap);
        assertEquals(solarPVClass, sampleMap.get(proxyInst));
    }

    @Test
    void testAddClassMappingEmptySet() {
        Map<String, String> sampleMap = new HashMap<>();
        testSet = new LinkedHashSet<>();
        GenericElementClassifier.addClassMapping("bim:" + furnishingClass, testSet, sampleMap);
        // No mapping is generated as there is no values
        assertTrue(sampleMap.size() == 0);
    }

    private void genSampleStatementForFurniture() {
        Model sampleModel = ModelFactory.createDefaultModel();
        // Generate the statements in the model
        sampleModel.createResource(bimUri + furnitureInst)
                .addProperty(RDF.type,
                        sampleModel.createResource(bimUri + furnishingClass));
        sampleModel.createResource(bimUri + furnitureInst)
                .addProperty(RDFS.label,
                        sampleModel.createLiteral(meterLiteral));
        addStatementToSet(sampleModel);
    }

    private void genSampleStatementForBuildingElementProxy() {
        Model sampleModel = ModelFactory.createDefaultModel();
        // Generate the statements in the model
        sampleModel.createResource(bimUri + proxyInst)
                .addProperty(RDF.type,
                        sampleModel.createResource(bimUri + proxyClass));
        sampleModel.createResource(bimUri + proxyInst)
                .addProperty(RDFS.label,
                        sampleModel.createLiteral(solarPVLiteral));
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