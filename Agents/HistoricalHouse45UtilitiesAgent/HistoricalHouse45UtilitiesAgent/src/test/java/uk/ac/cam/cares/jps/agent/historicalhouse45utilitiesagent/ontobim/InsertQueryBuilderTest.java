package uk.ac.cam.cares.jps.agent.historicalhouse45utilitiesagent.ontobim;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

class InsertQueryBuilderTest {
    private static StringBuilder insertQueryBuilder;
    private static final String electricityIri = "electricity_6161";
    private static final String waterIri = "water_321";
    private static final String oilIri = "oil_123";
    private static final String falseStatement = "ontoubemmp:consumesUtilities twa:";

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
    void testAddElectricityInsertStatements() {
        InsertQueryBuilder.addElectricityInsertStatements(electricityIri, insertQueryBuilder);
        List<String> expected = genExpectedElectricityList();
        expected.forEach(line -> assertAll(
                () -> assertTrue(insertQueryBuilder.toString().contains(line)),
                () -> assertFalse(insertQueryBuilder.toString().contains(falseStatement)) // Ensure this statement is not added since condition is null
        ));
    }


    @Test
    void testAddWaterInsertStatements() {
        InsertQueryBuilder.addWaterInsertStatements(waterIri, insertQueryBuilder);
        List<String> expected = genExpectedWaterList();
        expected.forEach(line -> assertAll(
                () -> assertTrue(insertQueryBuilder.toString().contains(line)),
                () -> assertFalse(insertQueryBuilder.toString().contains(falseStatement)) // Ensure this statement is not added since condition is null
        ));
    }

    @Test
    void testAddOilInsertStatements() {
        InsertQueryBuilder.addOilInsertStatements(oilIri, insertQueryBuilder);
        List<String> expected = genExpectedOilList();
        expected.forEach(line -> assertAll(
                () -> assertTrue(insertQueryBuilder.toString().contains(line)),
                () -> assertFalse(insertQueryBuilder.toString().contains(falseStatement)) // Ensure this statement is not added since condition is null
        ));
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
}