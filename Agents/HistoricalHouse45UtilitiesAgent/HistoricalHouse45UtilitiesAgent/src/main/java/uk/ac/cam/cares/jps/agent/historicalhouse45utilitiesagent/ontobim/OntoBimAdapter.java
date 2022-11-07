package uk.ac.cam.cares.jps.agent.historicalhouse45utilitiesagent.ontobim;

import org.apache.jena.query.QuerySolution;
import org.apache.jena.query.ResultSet;
import org.apache.jena.rdf.model.RDFNode;

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
    private static final String ELECTRICITY_KEYWORD = "ElectricityConsumption";
    private static final String WATER_KEYWORD = "WaterConsumption";
    private static final String OIL_KEYWORD = "OilConsumption";
    private static final String BUILDING_KEYWORD = "/Building_";
    private static final String GROUNDFLOOR_KEYWORD = "Ground";
    private static final String FIRSTFLOOR_KEYWORD = "Level 1";
    private static final String ATTIC_KEYWORD = "Attic";
    protected static String BUILDING_INST ="";
    protected static String GROUND_FLOOR_INST ="";
    protected static String FIRST_FLOOR_INST ="";
    protected static String ATTIC_INST ="";


    /**
     * Adds the supplementary triples for the instantiated timeseries to link to the OntoBIM instances.
     *
     * @param queryEndpoint  The remote SPARQL query endpoint.
     * @param updateEndpoint The remote SPARQL update endpoint.
     */
    public static void addSupplementaryTriples(String queryEndpoint, String updateEndpoint) {
        retrieveBaseUri(queryEndpoint);
        List<String> measureList = retrieveMeasureInstances(queryEndpoint);
        retrieveZoneInstances(queryEndpoint);
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
     * Store the query results into variables.
     *
     * @param results The SELECT query results.
     */
    private static void storeResultsAsVar(ResultSet results) {
        while (results.hasNext()) {
            QuerySolution soln = results.nextSolution();
            String zone = soln.get(SelectQueryBuilder.ZONE_VAR).toString();
            // When there is no name ie a null value, use of Optional class to parse the name
            Optional<RDFNode> nameContainer = Optional.ofNullable(soln.get(SelectQueryBuilder.NAME_VAR));
            String name = nameContainer.isPresent() ? nameContainer.toString() :"";
            if (zone.contains(BUILDING_KEYWORD)) {
                BUILDING_INST = zone;
            } else if (name.contains(ATTIC_KEYWORD)) {
                ATTIC_INST = zone;
            } else if (name.contains(GROUNDFLOOR_KEYWORD)) {
                GROUND_FLOOR_INST = zone;
            } else if (name.contains(FIRSTFLOOR_KEYWORD)) {
                FIRST_FLOOR_INST = zone;
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
        InsertQueryBuilder.addOntoBuiltEnvInsertStatements(builder);
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
    /*
    // Missing sensor display too

        INSTBUILDING IRI ontobuiltenv:hasOntoCityGMLRepresentation <>  ;
    ontobuiltenv:hasPVsuitableRoofArea twahouse:RoofArea_48b4592f-6862-4b42-8fd2-4bd0e965e531.
// MANUALY ADD
            twahouse:RoofArea_48b4592f-6862-4b42-8fd2-4bd0e965e531
    rdf:type	om:Area ;
    om:hasUnit	om:squareMetre ;
    om:hasNumericalValue "5555.55"^^xsd:double.
// MANUALY ADD
    om:squareMetre
    skos:notation 	"m2"^^qudt:UCUMcs .
     */
}
