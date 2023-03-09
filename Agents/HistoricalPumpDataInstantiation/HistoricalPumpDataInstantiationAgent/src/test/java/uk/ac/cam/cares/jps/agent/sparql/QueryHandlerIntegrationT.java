package uk.ac.cam.cares.jps.agent.sparql;

import org.apache.jena.query.*;
import org.apache.jena.sparql.exec.http.QueryExecutionHTTP;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.testcontainers.containers.GenericContainer;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.junit.jupiter.Testcontainers;
import org.testcontainers.utility.DockerImageName;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * This integration test targets the QueryHandler class and its public methods that interact with an external Knowledge graph.
 * Will not run when building a new Docker image, as it does not have access to the host Docker daemon.
 */
@Testcontainers
class QueryHandlerIntegrationT {
    @Container
    private static final GenericContainer blazegraph = new GenericContainer(DockerImageName.parse("nawer/blazegraph:latest"))
            .withExposedPorts(9999);
    private static String endpoint;
    private static final String subjectIRI = "http://www.test.org.com/subject";
    private static final String predIRI = "http://www.test.org.com/pred";
    private static final String objectIRI = "http://www.test.org.com/object";
    private static final String subjectVar = "subject";

    @BeforeAll
    static void genEndpoint() {
        // Get host and port name to form KG endpoint
        endpoint = "http://" + blazegraph.getHost() + ":" + blazegraph.getFirstMappedPort();
        endpoint = endpoint + "/blazegraph/namespace/kb/sparql"; // Default namespace is "kb"
    }

    @Test
    void testInsertToEndpoint() {
        // Generate query
        InsertBuilder builder = new InsertBuilder();
        builder.addTriples(subjectIRI, predIRI, objectIRI, 4);
        String insertQuery = builder.buildString();
        // Execute method
        QueryHandler.insertToEndpoint(insertQuery, endpoint);
        // Retrieve the results
        String selectQuery = "SELECT * WHERE {?" + subjectVar + " <" + predIRI + "> <" + objectIRI + ">}";
        Query query = QueryFactory.create(selectQuery);
        try (QueryExecution qExec = QueryExecutionHTTP.service(endpoint, query)) {
            ResultSet results = qExec.execSelect();
            // Test that the subject inserted is correct
            while (results.hasNext()) {
                QuerySolution soln = results.nextSolution();
                String iri = soln.get(subjectVar).toString();
                assertEquals(subjectIRI, iri);
            }
        }
    }
}
