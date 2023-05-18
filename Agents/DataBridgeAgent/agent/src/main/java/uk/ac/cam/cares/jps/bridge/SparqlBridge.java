package uk.ac.cam.cares.jps.bridge;

import org.apache.jena.query.*;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdfconnection.RDFConnection;
import org.apache.jena.sparql.exec.http.QueryExecutionHTTP;

/**
 * A bridge that connects and transfer data between 2 SPARQL endpoints.
 *
 * @author qhouyee
 */
public class SparqlBridge {
    private final String source;
    private final String target;

    /**
     * Standard Constructor setting the SPARQL endpoints.
     *
     * @param srcEndpoint Sets the source SPARQL endpoint.
     * @param tgtEndpoint Sets the target SPARQL endpoint.
     */
    public SparqlBridge(String srcEndpoint, String tgtEndpoint) {
        this.source = srcEndpoint;
        this.target = tgtEndpoint;
    }

    /**
     * Transfer all triples from the source to target endpoint.
     */
    public void transfer() {
        // Create a connection using try-with-resources to close connection when complete
        try (RDFConnection conn = RDFConnection.connect(this.target)) {
            Model model = queryAllTriples();
            conn.load(model);
        }
    }

    /**
     * Query all triples from the source endpoint.
     *
     * @return The query results for further processing.
     */
    private Model queryAllTriples() {
        StringBuilder queryString = new StringBuilder();
        queryString.append("CONSTRUCT {?s ?p ?o} WHERE {?s ?p ?o}");
        Query query = QueryFactory.create(queryString.toString());
        try (QueryExecution qExec = QueryExecutionHTTP.service(this.source, query)) {
            return qExec.execConstruct();
        }
    }
}
