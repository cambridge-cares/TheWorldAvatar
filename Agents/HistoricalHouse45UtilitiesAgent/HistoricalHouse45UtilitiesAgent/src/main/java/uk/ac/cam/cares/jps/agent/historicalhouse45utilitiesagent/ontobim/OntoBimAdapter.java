package uk.ac.cam.cares.jps.agent.historicalhouse45utilitiesagent.ontobim;

import org.apache.jena.query.QuerySolution;
import org.apache.jena.query.ResultSet;
import org.apache.jena.rdf.model.RDFNode;
import uk.ac.cam.cares.jps.agent.historicalhouse45utilitiesagent.BuildingIRISingleton;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

/**
 * An adaptor that supplements the instantiated time series triples with their relationships to instances of the OntoBIM ontology.
 *
 * @author qhouyee
 */
public class OntoBimAdapter {
    protected static String BASE_URI;
    private static BuildingIRISingleton singleton;
    private static final String ELECTRICITY_KEYWORD = "ElectricityConsumption";
    private static final String WATER_KEYWORD = "WaterConsumption";
    private static final String OIL_KEYWORD = "OilConsumption";
    private static final String BUILDING_KEYWORD = "/Building_";
    private static final String GROUNDFLOOR_KEYWORD = "Ground";
    private static final String FIRSTFLOOR_KEYWORD = "Level 1";
    private static final String ATTIC_KEYWORD = "Attic";

    /**
     * Adds the supplementary triples for the instantiated timeseries to link to the OntoBIM instances.
     *
     * @param queryEndpoint  The remote SPARQL query endpoint.
     * @param updateEndpoint The remote SPARQL update endpoint.
     * @param singletonInst  A singleton instance containing the required building and storey instances.
     */
    public static void addSupplementaryTriples(String queryEndpoint, String updateEndpoint, BuildingIRISingleton singletonInst) {
        singleton = singletonInst;
        retrieveBaseUri(queryEndpoint);
        List<String> measureList = retrieveMeasureInstances(queryEndpoint);
        retrieveZoneInstances(queryEndpoint);
        retrieveMeterInstances(queryEndpoint);
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
        String query = SelectQueryBuilder.genUtilityTSSelectQuery();
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
     * Retrieves the building and storey instances that is linked to the utility measure in the remote endpoint.
     *
     * @param endpoint The remote SPARQL endpoint.
     */
    private static void retrieveZoneInstances(String endpoint) {
        String query = SelectQueryBuilder.genZoneSelectQuery();
        ResultSet results = QueryHandler.execSelectQuery(query, endpoint);
        storeResultsAsVar(results);
    }

    /**
     * Retrieves the utility meters instances that is linked to the utility measure in the remote endpoint.
     *
     * @param endpoint The remote SPARQL endpoint.
     */
    private static void retrieveMeterInstances(String endpoint) {
        String query = SelectQueryBuilder.genMeterSelectQuery();
        ResultSet results = QueryHandler.execSelectQuery(query, endpoint);
        storeResultsAsVar(results);
    }

    /**
     * Store the query results into variables.
     *
     * @param results The SELECT query results.
     */
    private static void storeResultsAsVar(ResultSet results) {
        while (results.hasNext()) {
            QuerySolution soln = results.nextSolution();
            // When there is no name ie a null value, use of Optional class to parse the name
            Optional<RDFNode> nameContainer = Optional.ofNullable(soln.get(SelectQueryBuilder.NAME_VAR));
            String name = nameContainer.isPresent() ? nameContainer.toString() : "";

            // Assign values to their reference only when there is a retrievable value
            // Similar to the code above, except RDFNode cannot be an Optional class, and will lead to errors
            String zone = Optional.ofNullable(soln.get(SelectQueryBuilder.ZONE_VAR)).isPresent()
                    ? soln.get(SelectQueryBuilder.ZONE_VAR).toString() : "";
            String elecMeter = Optional.ofNullable(soln.get(SelectQueryBuilder.ELECMETER_VAR)).isPresent()
                    ? soln.get(SelectQueryBuilder.ELECMETER_VAR).toString() : "";
            String waterMeter = Optional.ofNullable(soln.get(SelectQueryBuilder.WATERMETER_VAR)).isPresent()
                    ? soln.get(SelectQueryBuilder.WATERMETER_VAR).toString() : "";
            String oilMeter = Optional.ofNullable(soln.get(SelectQueryBuilder.OILMETER_VAR)).isPresent()
                    ? soln.get(SelectQueryBuilder.OILMETER_VAR).toString() : "";

            if (zone.contains(BUILDING_KEYWORD)) {
                singleton.setBuildingIri(zone);
            } else if (name.contains(ATTIC_KEYWORD)) {
                singleton.setAtticIri(zone);
            } else if (name.contains(GROUNDFLOOR_KEYWORD)) {
                singleton.setGroundFloorIri(zone);
            } else if (name.contains(FIRSTFLOOR_KEYWORD)) {
                singleton.setFirstFloorIri(zone);
            }

            // Set their IRIs in the singleton if it exists
            // Keep the if statements as separate statements since they are found in one solution
            if (!elecMeter.isEmpty()) {
                singleton.setElecMeterIri(elecMeter);
            }
            if (!waterMeter.isEmpty()) {
                singleton.setWaterMeterIri(waterMeter);
            }
            if (!oilMeter.isEmpty()) {
                singleton.setOilMeterIri(oilMeter);
            }
        }
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
        InsertQueryBuilder.addOntoBuiltEnvInsertStatements(builder, singleton);
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
            InsertQueryBuilder.addMeterInsertStatements(measureIRI, insertQuery, singleton);
        } else {
            if (measureIRI.contains(ELECTRICITY_KEYWORD)) {
                InsertQueryBuilder.addElectricityInsertStatements(measureIRI, insertQuery, singleton);
            } else if (measureIRI.contains(WATER_KEYWORD)) {
                InsertQueryBuilder.addWaterInsertStatements(measureIRI, insertQuery, singleton);
            } else if (measureIRI.contains(OIL_KEYWORD)) {
                InsertQueryBuilder.addOilInsertStatements(measureIRI, insertQuery, singleton);
            }
        }
    }
}
