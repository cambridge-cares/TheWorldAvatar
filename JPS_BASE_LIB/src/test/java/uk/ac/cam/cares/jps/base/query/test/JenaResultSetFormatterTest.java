package uk.ac.cam.cares.jps.base.query;

import org.apache.jena.arq.querybuilder.SelectBuilder;
import org.apache.jena.query.*;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.vocabulary.RDF;
import org.json.JSONArray;
import org.json.JSONObject;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

class JenaResultSetFormatterTest {
    private static final String testNamespace = "test";
    private static final String testPrefix = testNamespace + ":";
    private static final String testUri = "http://www.theworldavatar.com/ontology/test/";
    private static final String joeFirstName = "Joe";
    private static final String johnFirstName = "John";
    private static final String joeLastName = "Helmer";
    private static final String johnLastName = "Blank";
    private static final String studentClass = "Student";
    private static final String hasLastNamePred = "hasLastName";
    private static final String firstName = "firstName";
    private static final String firstNameVar = "?" + firstName;
    private static final String lastName = "lastName";
    private static final String lastNameVar = "?" + lastName;
    private static final String commaChar = ",";
    private static Model sampleModel;

    @BeforeAll
    static void init() {
        sampleModel = genSampleModel();
    }

    @Test
    void testConvertToCSV() {
        // Set up for Single Variable Result
        String queryString = genSingleVariableSelectQuery().buildString();
        ResultSet results = execQuery(queryString);
        // Execute the method
        String csvOutput = JenaResultSetFormatter.convertToCSV(results);
        List<String> expected = genExpectedLinesForSingleVarCsvFormat();
        // Test that output follows expected output
        for (String line : expected) {
            assertTrue(csvOutput.contains(line));
        }

        // Set up for Dual Variable Result
        queryString = genTwoVariableSelectQuery().buildString();
        results = execQuery(queryString);
        // Execute the method
        csvOutput = JenaResultSetFormatter.convertToCSV(results);
        expected = genExpectedLinesForDoubleVarCsvFormat();
        // Test that output follows expected output
        for (String line : expected) {
            assertTrue(csvOutput.contains(line));
        }
    }

    @Test
    void testConvertToJSONW3CStandard() {
        // Set up for Single Variable Result
        String queryString = genSingleVariableSelectQuery().buildString();
        ResultSet results = execQuery(queryString);
        // Execute the method
        String jsonOutput = JenaResultSetFormatter.convertToJSONW3CStandard(results);
        List<String> expected = genExpectedLinesForSingleVarJsonW3cFormat();
        for (String line : expected) {
            assertTrue(jsonOutput.contains(line));
        }

        // Set up for Dual Variable Result
        queryString = genTwoVariableSelectQuery().buildString();
        results = execQuery(queryString);
        // Execute the method
        jsonOutput = JenaResultSetFormatter.convertToJSONW3CStandard(results);
        expected = genExpectedLinesForDoubleVarJsonW3cFormat();
        for (String line : expected) {
            assertTrue(jsonOutput.contains(line));
        }
    }

    @Test
    void testConvertToSimplifiedList() {
        // Set up for Single Variable Result
        String queryString = genSingleVariableSelectQuery().buildString();
        ResultSet results = execQuery(queryString);
        // Execute the method
        JSONObject jsonObjectOutput = JenaResultSetFormatter.convertToSimplifiedList(results);
        assertEquals(testUri + joeFirstName, jsonObjectOutput.getJSONArray("results").getJSONObject(0).get(firstName));
        assertEquals(testUri + johnFirstName, jsonObjectOutput.getJSONArray("results").getJSONObject(1).get(firstName));

        // Set up for Dual Variable Result
        queryString = genTwoVariableSelectQuery().buildString();
        results = execQuery(queryString);
        // Execute the method
        jsonObjectOutput = JenaResultSetFormatter.convertToSimplifiedList(results);
        assertEquals(testUri + joeFirstName, jsonObjectOutput.getJSONArray("results").getJSONObject(0).get(firstName));
        assertEquals(testUri + joeLastName, jsonObjectOutput.getJSONArray("results").getJSONObject(0).get(lastName));
        assertEquals(testUri + johnFirstName, jsonObjectOutput.getJSONArray("results").getJSONObject(1).get(firstName));
        assertEquals(testUri + johnLastName, jsonObjectOutput.getJSONArray("results").getJSONObject(1).get(lastName));
    }

    @Test
    void testConvertToSimplifiedListOverload() {
        String queryString = genSingleVariableSelectQuery().buildString();
        // Test if both workflow produce the same outcome
        // Direct convert: ResultSet -> JSONObject
        ResultSet results = execQuery(queryString);
        JSONObject jsonObjectOutput = JenaResultSetFormatter.convertToSimplifiedList(results);
        // Indirect convert: ResultSet -> JSONW3C String -> JSONObject
        results = execQuery(queryString);
        String intermediateResult = JenaResultSetFormatter.convertToJSONW3CStandard(results);
        JSONObject jsonOverloadedOutput = JenaResultSetFormatter.convertToSimplifiedList(intermediateResult);
        assertEquals(jsonObjectOutput.toString(), jsonOverloadedOutput.toString());
    }


    @Test
    void testConvertToSimplifiedJsonArray() {
        // Set up for Single Variable Result
        String queryString = genSingleVariableSelectQuery().buildString();
        ResultSet results = execQuery(queryString);
        // Execute the method
        String jsonResults = JenaResultSetFormatter.convertToJSONW3CStandard(results);
        JSONArray output = JenaResultSetFormatter.convertToSimplifiedJsonArray(jsonResults);
        assertEquals(testUri + joeFirstName, output.getJSONObject(0).get(firstName));
        assertEquals(testUri + johnFirstName, output.getJSONObject(1).get(firstName));

        // Set up for Dual Variable Result
        queryString = genTwoVariableSelectQuery().buildString();
        results = execQuery(queryString);
        // Execute the method
        jsonResults = JenaResultSetFormatter.convertToJSONW3CStandard(results);
        output = JenaResultSetFormatter.convertToSimplifiedJsonArray(jsonResults);
        assertEquals(testUri + joeFirstName, output.getJSONObject(0).get(firstName));
        assertEquals(testUri + joeLastName, output.getJSONObject(0).get(lastName));
        assertEquals(testUri + johnFirstName, output.getJSONObject(1).get(firstName));
        assertEquals(testUri + johnLastName, output.getJSONObject(1).get(lastName));
    }

    @Test
    void testConvertToListofStringArrays() {
        // Set up for required input
        String queryString = genSingleVariableSelectQuery().buildString();
        ResultSet results = execQuery(queryString);
        String jsonResults = JenaResultSetFormatter.convertToJSONW3CStandard(results);
        // Test method output
        List<String[]> output = JenaResultSetFormatter.convertToListofStringArrays(jsonResults, firstName);
        // Two instances indicate there should be two output String[]
        assertEquals(2, output.size());

        // First instance should be related to Joe
        String[] firstOutput = output.get(0);
        assertEquals(1, firstOutput.length);
        assertEquals(testUri + joeFirstName, firstOutput[0]);

        // Second instance should be related to John
        String[] secOutput = output.get(1);
        assertEquals(1, secOutput.length);
        assertEquals(testUri + johnFirstName, secOutput[0]);
    }

    @Test
    void testConvertToListofStringArraysWithKeys() {
        // Set up for required input
        String queryString = genTwoVariableSelectQuery().buildString();
        ResultSet results = execQuery(queryString);
        String jsonResults = JenaResultSetFormatter.convertToJSONW3CStandard(results);
        String[] keys = {firstName, lastName};
        // Test method output
        List<String[]> output = JenaResultSetFormatter.convertToListofStringArraysWithKeys(jsonResults, keys);
        // Two instances indicate there should be two output String[]
        assertEquals(2, output.size());

        // First instance should be related to Joe
        String[] firstOutput = output.get(0);
        // Two keys should have two result
        assertEquals(2, firstOutput.length);
        assertEquals(testUri + joeFirstName, firstOutput[0]);
        assertEquals(testUri + joeLastName, firstOutput[1]);

        // Second instance should be related to John
        String[] secOutput = output.get(1);
        assertEquals(2, secOutput.length);
        assertEquals(testUri + johnFirstName, secOutput[0]);
        assertEquals(testUri + johnLastName, secOutput[1]);
    }

    @Test
    void testGetKeys() {
        // Set up for Single Variable Result
        String queryString = genSingleVariableSelectQuery().buildString();
        ResultSet results = execQuery(queryString);
        String jsonOutput = JenaResultSetFormatter.convertToJSONW3CStandard(results);
        // Execute the method
        String[] keyResults = JenaResultSetFormatter.getKeys(jsonOutput);
        assertTrue(keyResults.length == 1);
        assertEquals(firstName, keyResults[0]);

        // Set up for Dual Variable Result
        queryString = genTwoVariableSelectQuery().buildString();
        results = execQuery(queryString);
        jsonOutput = JenaResultSetFormatter.convertToJSONW3CStandard(results);
        // Execute the method
        String[] doubleKeyResults = JenaResultSetFormatter.getKeys(jsonOutput);
        assertTrue(doubleKeyResults.length == 2);
        assertEquals(firstName, doubleKeyResults[0]);
        assertEquals(lastName, doubleKeyResults[1]);
    }

    @Test
    void testCreateJSONfromCSV() {
        // Repeated string values
        String columnHead1 = "column1";
        String columnHead2 = "column2";
        String columnHead3 = "column3";
        String value1 = "value1";
        String value2 = "value2";
        String value3 = "value3";
        String strDataType = "String";
        String typeKey = "type";
        String valueKey = "value";

        // Initialise csv rows and columns
        List<String[]> csvgroup = new ArrayList<>();
        String[] headerRow = {columnHead1, columnHead2, columnHead3}; // First row/ headers
        String[] firstValueRow = {value1, value2, value3}; // Second row/ first row of values
        csvgroup.add(headerRow);
        csvgroup.add(firstValueRow);
        // Initialise header data type
        String[] headertype = {strDataType, strDataType};
        String output = new JenaResultSetFormatter().createJSONfromCSV(csvgroup, headertype);
        JSONObject joOutput = new JSONObject(output);

        // Test the column headers is parsed in the right format
        JSONArray columnHeaders = joOutput.getJSONObject("head").getJSONArray("vars");
        assertEquals(columnHead1, columnHeaders.get(0));
        assertEquals(columnHead2, columnHeaders.get(1));
        assertEquals(columnHead3, columnHeaders.get(2));

        // Test the values are properly attached to the right columns
        JSONObject bindings = joOutput.getJSONObject("results").getJSONArray("bindings").getJSONObject(0);
        // Get and test the first column values are expected
        JSONObject firstCol = bindings.getJSONObject(columnHead1);
        assertEquals(strDataType, firstCol.get(typeKey));
        assertEquals(value1, firstCol.get(valueKey));
        // Get and test the second column values are expected
        JSONObject secondCol = bindings.getJSONObject(columnHead2);
        assertEquals(strDataType, secondCol.get(typeKey));
        assertEquals(value2, secondCol.get(valueKey));
        // Get and test the third column values are expected
        JSONObject thirdCol = bindings.getJSONObject(columnHead3);
        assertEquals(strDataType, thirdCol.get(typeKey));
        assertEquals(value3, thirdCol.get(valueKey));
    }

    private static Model genSampleModel() {
        // Create a local Model
        Model sampleModel = ModelFactory.createDefaultModel();
        sampleModel.createResource(testUri + johnFirstName)
                .addProperty(RDF.type,
                        sampleModel.createResource(testUri + studentClass))
                .addProperty(sampleModel.createProperty(testUri + hasLastNamePred),
                        sampleModel.createResource(testUri + johnLastName));
        sampleModel.createResource(testUri + joeFirstName)
                .addProperty(RDF.type,
                        sampleModel.createResource(testUri + studentClass))
                .addProperty(sampleModel.createProperty(testUri + hasLastNamePred),
                        sampleModel.createResource(testUri + joeLastName));
        return sampleModel;
    }

    private static SelectBuilder genSingleVariableSelectQuery() {
        SelectBuilder builder = new SelectBuilder();
        builder.addPrefix(testNamespace, testUri);
        builder.addVar(firstNameVar).addWhere(firstNameVar, RDF.type, testPrefix + studentClass);
        return builder;
    }

    private static SelectBuilder genTwoVariableSelectQuery() {
        SelectBuilder builder = genSingleVariableSelectQuery();
        builder.addVar(lastNameVar).addOptional(firstNameVar, testPrefix + hasLastNamePred, lastNameVar);
        return builder;
    }

    private static ResultSet execQuery(String queryString) {
        Query query = QueryFactory.create(queryString);
        // Executes the query on a local Jena Model
        try (QueryExecution qExec = QueryExecutionFactory.create(query, sampleModel)) {
            return ResultSetFactory.copyResults(qExec.execSelect());
        }
    }

    private static List<String> genExpectedLinesForSingleVarCsvFormat() {
        List<String> expected = new ArrayList<>();
        expected.add(firstName);
        expected.add(testUri + joeFirstName);
        expected.add(testUri + johnFirstName);
        return expected;
    }

    private static List<String> genExpectedLinesForDoubleVarCsvFormat() {
        List<String> expected = new ArrayList<>();
        expected.add(firstName + commaChar + lastName);
        expected.add(testUri + joeFirstName + commaChar + testUri + joeLastName);
        expected.add(testUri + johnFirstName + commaChar + testUri + johnLastName);
        return expected;
    }

    private static List<String> genExpectedLinesForSingleVarJsonW3cFormat() {
        List<String> expected = new ArrayList<>();
        String innerBindings = "\": { \"type\": \"uri\" , \"value\": \"";
        expected.add("{ \"head\": {\n" +
                "    \"vars\": [ \"" + firstName + "\" ]\n" +
                "  }");
        expected.add("\"results\": {\n" +
                "    \"bindings\": [");
        expected.add("\"" + firstName + innerBindings + testUri + joeFirstName);
        expected.add("\"" + firstName + innerBindings + testUri + johnFirstName);
        return expected;
    }

    private static List<String> genExpectedLinesForDoubleVarJsonW3cFormat() {
        List<String> expected = new ArrayList<>();
        String innerBindings = "\": { \"type\": \"uri\" , \"value\": \"";
        expected.add("\"head\": {\n" +
                "    \"vars\": [ \"" + firstName + "\" , \"" + lastName + "\" ]\n");
        expected.add("\"results\": {\n" +
                "    \"bindings\": [");
        expected.add(firstName + innerBindings + testUri + joeFirstName + "\" } ,\n" +
                "        \"" + lastName + innerBindings + testUri + joeLastName);
        expected.add(firstName + innerBindings + testUri + johnFirstName + "\" } ,\n" +
                "        \"" + lastName + innerBindings + testUri + johnLastName);
        return expected;
    }
}