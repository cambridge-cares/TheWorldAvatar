package uk.ac.cam.cares.jps.agent.historicalhouse45utilitiesagent.ontobim;

import org.apache.jena.arq.querybuilder.SelectBuilder;
import org.apache.jena.query.*;
import org.apache.jena.sparql.exec.UpdateExec;
import org.apache.jena.sparql.exec.http.QueryExecutionHTTP;

/**
 * Provides the functions to generate prefix mappings, execute all SPARQL query types, and provides common properties.
 *
 * @author qhouyee
 */
class QueryHandler {
    private static final String START_PREFIX = "PREFIX ";
    private static final String RDF_URI = "http://www.w3.org/1999/02/22-rdf-syntax-ns#";
    private static final String RDFS_URI = "http://www.w3.org/2000/01/rdf-schema#";
    private static final String OM_URI = "http://www.ontology-of-units-of-measure.org/resource/om-2/";
    private static final String BOT_URI = "https://w3id.org/bot#";
    private static final String SAREF_URI = "https://saref.etsi.org/core/";
    private static final String SKOS_URI = "http://www.w3.org/2004/02/skos/core#";
    private static final String QUDT_URI = "http://qudt.org/schema/qudt/";
    private static final String TIMESERIES_URI = "https://www.theworldavatar.com/kg/ontotimeseries/";
    private static final String UBEMMP_URI = "https://www.theworldavatar.com/kg/ontoubemmp/";
    private static final String ONTOBUILTENV_URI = "http://www.theworldavatar.com/ontology/ontobuiltenv/OntoBuiltEnv.owl#";
    private static final String BIM_URI = "http://www.theworldavatar.com/ontology/ontobim/ontoBIM#";


    /**
     * Generate the prefix to namespace mapping for a SELECT Builder.
     *
     * @param builder The SELECT builder for querying.
     */
    protected static void genPrefixMapping(SelectBuilder builder) {
        // Exclude the semicolon : in the Prefixes
        builder.addPrefix(SelectQueryBuilder.TIMESERIES_PREFIX
                .substring(0, SelectQueryBuilder.TIMESERIES_PREFIX.length() - 1), TIMESERIES_URI);
        builder.addPrefix(SelectQueryBuilder.RDF_PREFIX
                .substring(0, SelectQueryBuilder.RDF_PREFIX.length() - 1), RDF_URI);
        builder.addPrefix(SelectQueryBuilder.RDFS_PREFIX
                .substring(0, SelectQueryBuilder.RDFS_PREFIX.length() - 1), RDFS_URI);
        builder.addPrefix(SelectQueryBuilder.BOT_PREFIX
                .substring(0, SelectQueryBuilder.BOT_PREFIX.length() - 1), BOT_URI);
        builder.addPrefix(SelectQueryBuilder.ONTOBIM_PREFIX
                .substring(0, SelectQueryBuilder.ONTOBIM_PREFIX.length() - 1), BIM_URI);
    }

    /**
     * Generate the prefix to namespace mapping for an INSERT DATA query .
     *
     * @param insertQuery INSERT DATA query generated using a String builder.
     */
    protected static void genPrefixMapping(StringBuilder insertQuery) {
        insertQuery.append(START_PREFIX + InsertQueryBuilder.BASE_PREFIX + InsertQueryBuilder.OPEN_ANCHOR + OntoBimAdapter.BASE_URI + InsertQueryBuilder.CLOSED_ANCHOR);
        insertQuery.append(START_PREFIX + InsertQueryBuilder.RDF_PREFIX + InsertQueryBuilder.OPEN_ANCHOR + RDF_URI + InsertQueryBuilder.CLOSED_ANCHOR);
        insertQuery.append(START_PREFIX + InsertQueryBuilder.OM_PREFIX + InsertQueryBuilder.OPEN_ANCHOR + OM_URI + InsertQueryBuilder.CLOSED_ANCHOR);
        insertQuery.append(START_PREFIX + InsertQueryBuilder.SAREF_PREFIX + InsertQueryBuilder.OPEN_ANCHOR + SAREF_URI + InsertQueryBuilder.CLOSED_ANCHOR);
        insertQuery.append(START_PREFIX + InsertQueryBuilder.SKOS_PREFIX + InsertQueryBuilder.OPEN_ANCHOR + SKOS_URI + InsertQueryBuilder.CLOSED_ANCHOR);
        insertQuery.append(START_PREFIX + InsertQueryBuilder.QUDT_PREFIX + InsertQueryBuilder.OPEN_ANCHOR + QUDT_URI + InsertQueryBuilder.CLOSED_ANCHOR);
        insertQuery.append(START_PREFIX + InsertQueryBuilder.UBEMMP_PREFIX + InsertQueryBuilder.OPEN_ANCHOR + UBEMMP_URI + InsertQueryBuilder.CLOSED_ANCHOR);
        insertQuery.append(START_PREFIX + InsertQueryBuilder.ONTOBUILTENV_PREFIX + InsertQueryBuilder.OPEN_ANCHOR + ONTOBUILTENV_URI + InsertQueryBuilder.CLOSED_ANCHOR);
    }

    /**
     * Executes the SPARQL SELECT query on a remote SPARQL endpoint.
     *
     * @param queryString   A string containing the required SELECT statements.
     * @param queryEndpoint The remote SPARQL endpoint to execute the query on.
     * @return The query results for further processing.
     */
    protected static ResultSet execSelectQuery(String queryString, String queryEndpoint) {
        Query query = QueryFactory.create(queryString);
        try (QueryExecution qExec = QueryExecutionHTTP.service(queryEndpoint, query)) {
            return ResultSetFactory.copyResults(qExec.execSelect());
        }
    }

    /**
     * Inserts the requisite triples on a remote SPARQL update endpoint.
     *
     * @param query          A string containing the required INSERT DATA statements.
     * @param updateEndpoint The remote SPARQL endpoint to update new values.
     */
    protected static void insertToEndpoint(String query, String updateEndpoint) {
        UpdateExec.service(updateEndpoint).update(query).execute();
    }
}
