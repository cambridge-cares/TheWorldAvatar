package uk.ac.cam.cares.jps.agent.sparql;

import org.apache.jena.sparql.exec.UpdateExec;

/**
 * Provides the functions to generate prefix mappings, execute all SPARQL query types, and provides common properties.
 *
 * @author qhouyee
 */
class QueryHandler {
    /**
     * Generate the prefix to namespace mapping for an INSERT DATA query .
     *
     * @param builder       INSERT Builder to generate mapping for.
     * @param baseNameSpace Base URI.
     */
    protected static void genInsertPrefixMapping(InsertBuilder builder, String baseNameSpace) {
        builder.addPrefix(OntologyConstant.BASE_PREFIX, baseNameSpace);
        builder.addPrefix(OntologyConstant.RDF_PREFIX, OntologyConstant.RDF_URI);
        builder.addPrefix(OntologyConstant.OM_PREFIX, OntologyConstant.OM_URI);
        builder.addPrefix(OntologyConstant.SKOS_PREFIX, OntologyConstant.SKOS_URI);
        builder.addPrefix(OntologyConstant.QUDT_PREFIX, OntologyConstant.QUDT_URI);
        builder.addPrefix(OntologyConstant.UBEMMP_PREFIX, OntologyConstant.UBEMMP_URI);
        builder.addPrefix(OntologyConstant.ONTOHEATNETWORK_PREFIX, OntologyConstant.ONTOHEATNETWORK_URI);
        builder.addPrefix(OntologyConstant.ONTOCAPE_PREFIX, OntologyConstant.ONTOCAPE_URI);
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
