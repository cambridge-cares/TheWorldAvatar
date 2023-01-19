package uk.ac.cam.cares.jps.agent.sparql;

import java.util.*;

/**
 * An adaptor that supplements the instantiated time series triples with their relationships to other nodes.
 *
 * @author qhouyee
 */
public class SparqlAdapter {
    private static String BASE_URI;
    private static final String SEMICOLON = ":";
    private static final String ELECTRICITY_INST = "ElectricityConsumption_Quantity_";
    private static final String COST_INST = "CostInTimeInterval_Quantity_";

    /**
     * Adds the supplementary triples for the instantiated time series to link to the required instances.
     *
     * @param queryEndpoint  The remote SPARQL query endpoint.
     * @param updateEndpoint The remote SPARQL update endpoint.
     * @param iriMappings    The IRI mappings generated in TSPropertiesHandler.
     */
    public static void addSupplementaryTriples(String queryEndpoint, String updateEndpoint, Map<String, Map<String, String>> iriMappings) {
        retrieveBaseUri(queryEndpoint);
        Map<String, List<String>> timeSeriesMap = genPumpTSMap(iriMappings);
        String insertQuery = createInsertQuery(timeSeriesMap);
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
     * Generate a map that maps pump IRI to their time series.
     *
     * @param iriPropMappings The IRI mappings generated in TSPropertiesHandler.
     * @return A Map linking each pump IRI to their time series as a list.
     */
    private static Map<String, List<String>> genPumpTSMap(Map<String, Map<String, String>> iriPropMappings) {
        Map<String, List<String>> timeSeriesMap = new HashMap<>();
        Map<String, String> pumpMatcherMap = PumpMatcher.genMappings();
        // Get the number of time series per group
        int groupTSSize = iriPropMappings.values().iterator().next().size();
        for (String group : iriPropMappings.keySet()) {
            // Initialise an empty list with size equivalent to the number of time series per group
            List<String> timeSeriesList = new ArrayList<>(Arrays.asList());
            for (int i = 0; i < groupTSSize; i++) {
                timeSeriesList.add("");
            }
            // Retrieve and add time series to a fixed position
            for (String timeSeriesIri : iriPropMappings.get(group).values()) {
                if (timeSeriesIri.contains("ElectricityConsumption")) {
                    timeSeriesList.set(0, timeSeriesIri);
                } else if (timeSeriesIri.contains("ElectricityPrice")) {
                    timeSeriesList.set(1, timeSeriesIri);
                }
            }
            // Add the pump IRI and their time series into the map if they exist
            String pumpMatchedGroup = pumpMatcherMap.get(group);
            if (pumpMatchedGroup != null) {
                timeSeriesMap.put(pumpMatchedGroup, timeSeriesList);
            }
        }
        return timeSeriesMap;
    }

    /**
     * Creates the INSERT DATA Query to be executed
     *
     * @param timeSeriesMap A hashmap linking each pump to their timeseries. Electricity consumption timeseries assumed as first, while cost is second
     * @return The INSERT query in String format.
     */
    private static String createInsertQuery(Map<String, List<String>> timeSeriesMap) {
        InsertBuilder builder = new InsertBuilder();
        QueryHandler.genInsertPrefixMapping(builder, BASE_URI);
        // Don't add unit triples here - these should be defined in OM. (And OM doesn't use skos:notation...)
        //builder.addTriples(OntologyConstant.OM_KWH, OntologyConstant.SKOS_NOTATION, OntologyConstant.KWH_LITERAL);
        // For each pump, add their triples
        for (String pumpIRI : timeSeriesMap.keySet()) {
            // Add Electricity consumption triples
            String elecQuantityIRI = OntologyConstant.BASE_PREFIX + SEMICOLON + ELECTRICITY_INST + UUID.randomUUID();
            builder.addTriples(pumpIRI, OntologyConstant.UBEMMP_CONSUMES_UTILITIES, elecQuantityIRI, 1);
            builder.addTriples(elecQuantityIRI, OntologyConstant.RDFTYPE, OntologyConstant.UBEMMP_ELECCONSUMPTION);
            builder.addTriples(elecQuantityIRI, OntologyConstant.OM_HASVALUE, timeSeriesMap.get(pumpIRI).get(0), 3);
            builder.addTriples(timeSeriesMap.get(pumpIRI).get(0), OntologyConstant.OM_HASUNIT, OntologyConstant.OM_KWH, 1);
            builder.addTriples(timeSeriesMap.get(pumpIRI).get(0), OntologyConstant.RDFS_LABEL, OntologyConstant.CONSUMPTION_LITERAL, 1);
            // Add Utility cost triples
            String costQuantityIRI = OntologyConstant.BASE_PREFIX + SEMICOLON + COST_INST + UUID.randomUUID();
            builder.addTriples(pumpIRI, OntologyConstant.ONTOCAPE_HAS_UTILITY_COST, costQuantityIRI, 1);
            builder.addTriples(costQuantityIRI, OntologyConstant.RDFTYPE, OntologyConstant.HEATNETWORK_COST_INTERVAL);
            builder.addTriples(costQuantityIRI, OntologyConstant.OM_HASVALUE, timeSeriesMap.get(pumpIRI).get(1), 3);
            builder.addTriples(timeSeriesMap.get(pumpIRI).get(1), OntologyConstant.OM_HASUNIT, OntologyConstant.OM_EUR, 1);
            builder.addTriples(timeSeriesMap.get(pumpIRI).get(1), OntologyConstant.RDFS_LABEL, OntologyConstant.COST_LITERAL, 1);
        }
        return builder.buildString();
    }
}
