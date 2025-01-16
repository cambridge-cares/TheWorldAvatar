package uk.ac.cam.cares.jps.agent.dashboard.stack.sparql.utils;

import org.apache.jena.rdfconnection.RDFConnection;

/**
 * Functional interface for defining actions to be performed on a given SPARQL endpoint.
 *
 * @author qhouyee
 */
@FunctionalInterface
public interface SparqlAction {
    /**
     * Executes a SPARQL action on the specified SPARQL endpoint.
     *
     * @param conn     The RDFConnection object to the SPARQL endpoint.
     * @param endpoint The URL of the SPARQL endpoint.
     * @throws Exception If an error occurs while executing the SPARQL action.
     */
    void execute(RDFConnection conn, String endpoint) throws Exception;
}
