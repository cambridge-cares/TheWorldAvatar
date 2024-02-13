package uk.ac.cam.cares.jps.agent.historicalhouse45utilitiesagent.ontobim;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import uk.ac.cam.cares.jps.agent.historicalhouse45utilitiesagent.BuildingIRISingleton;

import java.util.ArrayList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

class InsertQueryBuilderTest {
    private static StringBuilder insertQueryBuilder;
    private static BuildingIRISingleton singleton;
    private static final String electricityIri = "electricity_6161";
    private static final String elecFirstFloorIri = "electricity_FirstFloor_912";
    private static final String elecGroundFloorIri = "electricity_GroundFloor_315";
    private static final String elecAtticIri = "electricity_Attic_581";
    private static final String waterIri = "water_321";
    private static final String oilIri = "oil_123";
    private static final String elecMeasureIri = "ElectricityConsumption_SensorDisplay_812";
    private static final String waterMeasureIri = "WaterConsumption_SensorDisplay_617";
    private static final String oilMeasureIri = "OilConsumption_SensorDisplay_4417";
    private static final String citygml = "CityGML_Building_123";
    private static final String building = "Building_5515";
    private static final String firstFloor = "Storey_5781";
    private static final String groundFloor = "Storey_162";
    private static final String attic = "Storey_836";
    private static final String elecMeter = "ElectricityMeter_112";
    private static final String waterMeter = "WaterMeter_113";
    private static final String oilMeter = "OilMeter_114";
    private static final String falseStatement = "ontoubemmp:consumesUtilities twa:";

    @BeforeAll
    static void setup() {
        singleton = BuildingIRISingleton.getInstance();
    }

    @BeforeEach
    void init() {
        insertQueryBuilder = new StringBuilder();
    }

    @Test
    void testAddUnitsInsertStatements() {
        InsertQueryBuilder.addUnitsInsertStatements(insertQueryBuilder);
        assertEquals("om:kilowattHour skos:notation 'kW.h'^^qudt:UCUMcs.\n" +
                        "om:cubicMetre skos:notation 'm3'^^qudt:UCUMcs.\n" +
                        "om:litre skos:notation 'L'^^qudt:UCUMcs.\n",
                insertQueryBuilder.toString());
    }

    @Test
    void testAddOntoBuiltEnvInsertStatements() {
        setSingletonValues();
        InsertQueryBuilder.addOntoBuiltEnvInsertStatements(insertQueryBuilder, singleton);
        assertEquals("<" + building + "> builtenv:hasOntoCityGMLRepresentation <" + citygml + ">.\n",
                insertQueryBuilder.toString());
    }

    @Test
    void testAddOntoBuiltEnvInsertStatementsEmptyStatements() {
        resetSingleton();
        InsertQueryBuilder.addOntoBuiltEnvInsertStatements(insertQueryBuilder, singleton);
        assertTrue(insertQueryBuilder.toString().isEmpty());
    }

    @Test
    void testAddElectricityInsertStatements() {
        resetSingleton();
        InsertQueryBuilder.addElectricityInsertStatements(electricityIri, insertQueryBuilder, singleton);
        List<String> expected = genExpectedElectricityList();
        expected.forEach(line -> assertAll(
                () -> assertTrue(insertQueryBuilder.toString().contains(line)),
                () -> assertFalse(insertQueryBuilder.toString().contains(falseStatement)) // Ensure this statement is not added since condition is null
        ));
    }

    @Test
    void testAddElectricityInsertStatementsWithStoreyIri() {
        setSingletonValues();
        InsertQueryBuilder.addElectricityInsertStatements(elecAtticIri, insertQueryBuilder, singleton);
        assertTrue(insertQueryBuilder.toString().contains(attic + "> ontoubemmp:consumesUtilities twa:MonthlyElectricityConsumption_Quantity_"));
        InsertQueryBuilder.addElectricityInsertStatements(elecGroundFloorIri, insertQueryBuilder, singleton);
        assertTrue(insertQueryBuilder.toString().contains(groundFloor + "> ontoubemmp:consumesUtilities twa:MonthlyElectricityConsumption_Quantity_"));
        InsertQueryBuilder.addElectricityInsertStatements(elecFirstFloorIri, insertQueryBuilder, singleton);
        assertTrue(insertQueryBuilder.toString().contains(firstFloor + "> ontoubemmp:consumesUtilities twa:MonthlyElectricityConsumption_Quantity_"));
    }

    @Test
    void testAddWaterInsertStatements() {
        resetSingleton();
        InsertQueryBuilder.addWaterInsertStatements(waterIri, insertQueryBuilder, singleton);
        List<String> expected = genExpectedWaterList();
        expected.forEach(line -> assertAll(
                () -> assertTrue(insertQueryBuilder.toString().contains(line)),
                () -> assertFalse(insertQueryBuilder.toString().contains(falseStatement)) // Ensure this statement is not added since condition is null
        ));
    }

    @Test
    void testAddWaterInsertStatementsWithStoreyIri() {
        setSingletonValues();
        InsertQueryBuilder.addWaterInsertStatements(waterIri, insertQueryBuilder, singleton);
        assertTrue(insertQueryBuilder.toString().contains(building + "> ontoubemmp:consumesUtilities twa:MonthlyWaterConsumption_Quantity_"));
    }

    @Test
    void testAddOilInsertStatements() {
        resetSingleton();
        InsertQueryBuilder.addOilInsertStatements(oilIri, insertQueryBuilder, singleton);
        List<String> expected = genExpectedOilList();
        expected.forEach(line -> assertAll(
                () -> assertTrue(insertQueryBuilder.toString().contains(line)),
                () -> assertFalse(insertQueryBuilder.toString().contains(falseStatement)) // Ensure this statement is not added since condition is null
        ));
    }

    @Test
    void testAddOilInsertStatementsWithStoreyIri() {
        setSingletonValues();
        InsertQueryBuilder.addOilInsertStatements(oilIri, insertQueryBuilder, singleton);
        assertTrue(insertQueryBuilder.toString().contains(building + "> ontoubemmp:consumesUtilities twa:MonthlyOilConsumption_Quantity_"));
    }

    @Test
    void testAddMeterInsertStatementsForElectricityMeter() {
        setSingletonValues();
        InsertQueryBuilder.addMeterInsertStatements(elecMeasureIri, insertQueryBuilder, singleton);
        List<String> expected = genExpectedElectricityMeterList();
        expected.forEach(line -> assertTrue(insertQueryBuilder.toString().contains(line)));
    }

    @Test
    void testAddMeterInsertStatementsForWaterMeter() {
        setSingletonValues();
        InsertQueryBuilder.addMeterInsertStatements(waterMeasureIri, insertQueryBuilder, singleton);
        assertTrue(insertQueryBuilder.toString().contains(waterMeter+ "> saref:hasFunction saref:MeteringFunction_"));
        assertTrue(insertQueryBuilder.toString().contains("om:hasValue <" + waterMeasureIri));

    }

    @Test
    void testAddMeterInsertStatementsForOilMeter() {
        setSingletonValues();
        InsertQueryBuilder.addMeterInsertStatements(oilMeasureIri, insertQueryBuilder, singleton);
        assertTrue(insertQueryBuilder.toString().contains(oilMeter+ "> saref:hasFunction saref:MeteringFunction_"));
        assertTrue(insertQueryBuilder.toString().contains("om:hasValue <" + oilMeasureIri));
    }

    @Test
    void testAddMeterInsertStatementsNoMeterInst() {
        resetSingleton();
        InsertQueryBuilder.addMeterInsertStatements(electricityIri, insertQueryBuilder, singleton);
        assertTrue(insertQueryBuilder.toString().isEmpty());
    }

    private static void resetSingleton() {
        singleton.setOntoCityGmlBuildingIri("");
        singleton.setBuildingIri("");
        singleton.setGroundFloorIri("");
        singleton.setFirstFloorIri("");
        singleton.setAtticIri("");
        singleton.setElecMeterIri("");
        singleton.setWaterMeterIri("");
        singleton.setOilMeterIri("");
    }

    private static void setSingletonValues() {
        singleton.setOntoCityGmlBuildingIri(citygml);
        singleton.setBuildingIri(building);
        singleton.setGroundFloorIri(groundFloor);
        singleton.setFirstFloorIri(firstFloor);
        singleton.setAtticIri(attic);
        singleton.setElecMeterIri(elecMeter);
        singleton.setWaterMeterIri(waterMeter);
        singleton.setOilMeterIri(oilMeter);
    }

    private static List<String> genExpectedElectricityList() {
        List<String> results = new ArrayList<>();
        results.add("twa:MonthlyElectricityConsumption_Quantity_");
        results.add("rdf:type ontoubemmp:MonthlyElectricityConsumption;\n" +
                "\tom:hasValue <" + electricityIri + ">.\n" +
                "<" + electricityIri + "> rdf:type om:Energy;\n" +
                "\tom:hasUnit om:kilowattHour.");
        return results;
    }

    private static List<String> genExpectedWaterList() {
        List<String> results = new ArrayList<>();
        results.add("twa:MonthlyWaterConsumption_Quantity_");
        results.add("rdf:type ontoubemmp:MonthlyWaterConsumption;\n" +
                "\tom:hasValue <" + waterIri + ">.\n" +
                "<" + waterIri + "> rdf:type om:Volume;\n" +
                "\tom:hasUnit om:cubicMetre.");
        return results;
    }

    private static List<String> genExpectedOilList() {
        List<String> results = new ArrayList<>();
        results.add("twa:MonthlyOilConsumption_Quantity_");
        results.add("rdf:type ontoubemmp:MonthlyOilConsumption;\n" +
                "\tom:hasValue <" + oilIri + ">.\n" +
                "<" + oilIri + "> rdf:type om:Volume;\n" +
                "\tom:hasUnit om:litre.");
        return results;
    }

    private static List<String> genExpectedElectricityMeterList() {
        List<String> results = new ArrayList<>();
        results.add(elecMeter + "> saref:hasFunction saref:MeteringFunction_");
        results.add("rdf:type saref:MeteringFunction;\n" +
                "\tsaref:hasMeterReading saref:Measurement_");
        results.add("rdf:type saref:Measurement;\n" +
                "\tom:hasValue <" + elecMeasureIri);
        return results;
    }
}