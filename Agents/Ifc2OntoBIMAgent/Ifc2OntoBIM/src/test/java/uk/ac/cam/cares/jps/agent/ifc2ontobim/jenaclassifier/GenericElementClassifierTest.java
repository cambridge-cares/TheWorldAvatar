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
    private static final String proxyInst2 = "IfcBuildingElementProxy_8515";
    private static final String proxyInst3 = "IfcBuildingElementProxy_7912";
    private static final String proxyInst4 = "IfcBuildingElementProxy_6125";
    private static final String proxyInst5 = "IfcBuildingElementProxy_1972";
    private static final String proxyInst6 = "IfcBuildingElementProxy_5322";
    private static final String proxyClass = "IfcBuildingElementProxy";
    private static final String solarPVLiteral = "Solar Panel";
    private static final String solarPVClass = "SolarPanel";
    private static final String elecMeterLiteral = "Electrical Meter";
    private static final String elecMeterClass = "ElectricityMeter";
    private static final String oilMeterLiteral = "Oil Meter";
    private static final String oilMeterClass = "OilMeter";
    private static final String pollutionMeterLiteral = "Pollution Meter";
    private static final String pollutionMeterClass = "PollutionMeter";
    private static final String co2SensorLiteral = "Carbon Dioxide Sensor";
    private static final String co2SensorClass = "CarbonDioxideGasSensor";
    private static final String illumSensorLiteral = "Illuminance Sensor";
    private static final String illumSensorClass = "IlluminanceSensor";

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
        assertEquals(elecMeterClass, sampleMap.get(proxyInst2));
        assertEquals(oilMeterClass, sampleMap.get(proxyInst3));
        assertEquals(pollutionMeterClass, sampleMap.get(proxyInst4));
        assertEquals(co2SensorClass, sampleMap.get(proxyInst5));
        assertEquals(illumSensorClass, sampleMap.get(proxyInst6));

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
                        sampleModel.createResource(bimUri + proxyClass))
                .addProperty(RDFS.label,
                        sampleModel.createLiteral(solarPVLiteral));
        sampleModel.createResource(bimUri + proxyInst2)
                .addProperty(RDF.type,
                        sampleModel.createResource(bimUri + proxyClass))
                .addProperty(RDFS.label,
                        sampleModel.createLiteral(elecMeterLiteral));
        sampleModel.createResource(bimUri + proxyInst3)
                .addProperty(RDF.type,
                        sampleModel.createResource(bimUri + proxyClass))
                .addProperty(RDFS.label,
                        sampleModel.createLiteral(oilMeterLiteral));
        sampleModel.createResource(bimUri + proxyInst4)
                .addProperty(RDF.type,
                        sampleModel.createResource(bimUri + proxyClass))
                .addProperty(RDFS.label,
                        sampleModel.createLiteral(pollutionMeterLiteral));
        sampleModel.createResource(bimUri + proxyInst5)
                .addProperty(RDF.type,
                        sampleModel.createResource(bimUri + proxyClass))
                .addProperty(RDFS.label,
                        sampleModel.createLiteral(co2SensorLiteral));
        sampleModel.createResource(bimUri + proxyInst6)
                .addProperty(RDF.type,
                        sampleModel.createResource(bimUri + proxyClass))
                .addProperty(RDFS.label,
                        sampleModel.createLiteral(illumSensorLiteral));

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