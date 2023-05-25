package uk.ac.cam.cares.jps.agent.ifc2ontobim;

import org.apache.jena.query.*;
import org.apache.jena.rdfconnection.RDFConnection;
import org.apache.jena.sparql.exec.http.QueryExecutionHTTP;
import org.apache.jena.update.UpdateFactory;
import org.apache.jena.update.UpdateRequest;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;


public class JunitIntegrationTestUtils {
    public static final String SPARQL_ENDPOINT = "http://172.17.0.1:9999/blazegraph/namespace/kb/sparql";

    public static String[] query() {
        // Generate result array
        String[] result = new String[3];
        // Create query to retrieve all triples
        StringBuilder queryString = new StringBuilder();
        queryString.append("SELECT ?s ?p ?o WHERE {?s ?p ?o}");
        Query query = QueryFactory.create(queryString.toString());
        try (QueryExecution qExec = QueryExecutionHTTP.service(SPARQL_ENDPOINT, query)) {
            ResultSet results = qExec.execSelect();
            while (results.hasNext()) {
                QuerySolution soln = results.nextSolution();
                result[0] = soln.get("s").toString();
                result[1] = soln.get("p").toString();
                result[2] = soln.get("o").toString();
            }
        }
        return result;
    }

    public static void clear() {
        try (RDFConnection conn = RDFConnection.connect(SPARQL_ENDPOINT)) {
            UpdateRequest update = UpdateFactory.create("DELETE WHERE {?s ?p ?o}");
            conn.update(update);
        } catch (Exception e) {
            throw new JPSRuntimeException("Unable to clear triples at SPARQL endpoint: " + e.getMessage());
        }
    }
}