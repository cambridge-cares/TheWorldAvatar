package uk.ac.cam.cares.jps;


import org.apache.jena.query.*;
import org.apache.jena.rdfconnection.RDFConnection;
import org.apache.jena.sparql.exec.http.QueryExecutionHTTP;
import org.apache.jena.update.UpdateFactory;
import org.apache.jena.update.UpdateRequest;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

import java.util.ArrayDeque;
import java.util.Queue;

public class IntegrationTestUtils {
    public static final String SRC_SPARQL_ENDPOINT = "http://172.17.0.1:9998/blazegraph/namespace/kb/sparql";
    public static final String TGT_SPARQL_ENDPOINT = "http://172.17.0.1:9997/blazegraph/namespace/kb/sparql";
    public static final String SAMPLE_RDF_SUBJECT = "http://www.example.org/subject";
    public static final String SAMPLE_RDF_PREDICATE = "http://www.example.org/predicate";
    public static final String SAMPLE_RDF_OBJECT = "http://www.example.org/object";
    public static final String SPARQL_DELETE = "DELETE WHERE {?s ?p ?o}";
    public static final String SPARQL_INSERT = "INSERT DATA {<" + SAMPLE_RDF_SUBJECT + "> <" + SAMPLE_RDF_PREDICATE + "> <" + SAMPLE_RDF_OBJECT + ">}";

    public static Queue<String> query(String endpoint) {
        // Generate results as a queue
        Queue<String> result = new ArrayDeque<>();
        // Create query to retrieve all triples
        StringBuilder queryString = new StringBuilder();
        queryString.append("SELECT ?s ?p ?o WHERE {?s ?p ?o}");
        Query query = QueryFactory.create(queryString.toString());
        try (QueryExecution qExec = QueryExecutionHTTP.service(endpoint, query)) {
            ResultSet results = qExec.execSelect();
            while (results.hasNext()) {
                QuerySolution soln = results.nextSolution();
                result.offer(soln.get("s").toString());
                result.offer(soln.get("p").toString());
                result.offer(soln.get("o").toString());
            }
        }
        return result;
    }

    public static void updateEndpoint(String endpoint, String updateQuery) {
        try (RDFConnection conn = RDFConnection.connect(endpoint)) {
            UpdateRequest update = UpdateFactory.create(updateQuery);
            conn.update(update);
        } catch (Exception e) {
            throw new JPSRuntimeException("Unable to update queries at SPARQL endpoint: " + e.getMessage());
        }
    }
}
