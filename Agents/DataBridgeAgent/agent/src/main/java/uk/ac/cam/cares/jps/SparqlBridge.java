package uk.ac.cam.cares.jps;

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
    private final String origin;
    private final String destination;

    /**
     * Standard Constructor setting the SPARQL endpoints.
     *
     * @param originEndpoint      Sets the SPARQL endpoint designated as origin.
     * @param destinationEndpoint Sets the SPARQL endpoint designated as destination.
     */
    public SparqlBridge(String originEndpoint, String destinationEndpoint) {
        this.origin = originEndpoint;
        this.destination = destinationEndpoint;
    }

    /**
     * Transfer all triples from the origin endpoint to destination.
     */
    public void transfer() {
        // Create a connection using try-with-resources to close connection when complete
        try (RDFConnection conn = RDFConnection.connect(this.destination)) {
            Model model = queryAllTriples();
            conn.load(model);
        }
    }

    /**
     * Query all triples from the origin endpoint.
     *
     * @return The query results for further processing.
     */
    private Model queryAllTriples() {
        StringBuilder queryString = new StringBuilder();
        queryString.append("CONSTRUCT {?s ?p ?o} WHERE {?s ?p ?o}");
        Query query = QueryFactory.create(queryString.toString());
        try (QueryExecution qExec = QueryExecutionHTTP.service(this.origin, query)) {
            return qExec.execConstruct();
        }
    }
}
