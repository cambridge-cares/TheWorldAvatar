package uk.ac.cam.cares.jps.agent.historicalhouse45utilitiesagent.ontobim;

import org.apache.jena.query.QuerySolution;
import org.apache.jena.query.ResultSet;

import java.util.ArrayList;
import java.util.List;

/**
 * An adaptor that supplements the instantiated time series triples with their relationships to instances of the OntoBIM ontology.
 *
 * @author qhouyee
 */
public class OntoBimAdapter {
    protected static String BASE_URI;
    private static final String ELECTRICITY_KEYWORD = "ElectricityConsumption";
    private static final String WATER_KEYWORD = "WaterConsumption";
    private static final String OIL_KEYWORD = "OilConsumption";

    /**
     * Adds the supplementary triples for the instantiated timeseries to link to the OntoBIM instances.
     *
     * @param queryEndpoint  The remote SPARQL query endpoint.
     * @param updateEndpoint The remote SPARQL update endpoint.
     */
    public static void addSupplementaryTriples(String queryEndpoint, String updateEndpoint) {
        retrieveBaseUri(queryEndpoint);
        List<String> measureList = retrieveMeasureInstances(queryEndpoint);
        String insertQuery = createInsertQuery(measureList);
        QueryHandler.insertToEndpoint(insertQuery, updateEndpoint);
    }

    /**
     * Retrieves the base URI from the endpoint string.
     *
     * @param endpoint The remote SPARQL endpoint.
     */
    private static void retrieveBaseUri(String endpoint) {
        String[] split = endpoint.split("sparql");
        BASE_URI = split[0];
    }

    /**
     * Retrieves the time series instances for each measure in the remote endpoint.
     *
     * @param endpoint The remote SPARQL endpoint.
     * @return The query results stored as a List.
     */
    private static List<String> retrieveMeasureInstances(String endpoint) {
        String query = SelectQueryBuilder.genSelectQuery();
        ResultSet results = QueryHandler.execSelectQuery(query, endpoint);
        return storeResultsInList(results);
    }

    /**
     * Store the query results in a list.
     *
     * @param results The SELECT query results.
     * @return The individual query result stored as a List.
     */
    private static List<String> storeResultsInList(ResultSet results) {
        List<String> queryResults = new ArrayList<>();
        while (results.hasNext()) {
            QuerySolution soln = results.nextSolution();
            String iri = soln.get(SelectQueryBuilder.SUBJECT_VAR).toString();
            queryResults.add(iri);
        }
        return queryResults;
    }

    /**
     * Creates the INSERT DATA Query to be executed
     *
     * @param measureList The list of measures to be linked to OntoBIM instances.
     * @return The INSERT query in String format.
     */
    private static String createInsertQuery(List<String> measureList) {
        StringBuilder builder = new StringBuilder();
        QueryHandler.genPrefixMapping(builder);
        builder.append("INSERT DATA {");
        InsertQueryBuilder.addUnitsInsertStatements(builder);
        for (String measure : measureList) {
            addInsertStatements(measure, builder);
        }
        builder.append("}");
        return builder.toString();
    }

    /**
     * Add the statements for utility consumption to be inserted into the SPARQL endpoint.
     *
     * @param measureIRI  Utility IRI generated that should be linked to OntoBIM instances.
     * @param insertQuery INSERT DATA query generated using a String builder.
     */
    private static void addInsertStatements(String measureIRI, StringBuilder insertQuery) {
        if (measureIRI.contains("Sensordisplay")) {

        } else {
            if (measureIRI.contains(ELECTRICITY_KEYWORD)) {
                InsertQueryBuilder.addElectricityInsertStatements(measureIRI, insertQuery);
            } else if (measureIRI.contains(WATER_KEYWORD)) {
                InsertQueryBuilder.addWaterInsertStatements(measureIRI, insertQuery);
            } else if (measureIRI.contains(OIL_KEYWORD)) {
                InsertQueryBuilder.addOilInsertStatements(measureIRI, insertQuery);
            }
        }
    }
}
