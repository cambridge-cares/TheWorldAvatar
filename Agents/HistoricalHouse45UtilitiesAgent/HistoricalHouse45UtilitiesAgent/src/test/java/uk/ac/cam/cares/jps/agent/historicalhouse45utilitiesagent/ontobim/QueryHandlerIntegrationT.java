package uk.ac.cam.cares.jps.agent.historicalhouse45utilitiesagent.ontobim;

import org.apache.jena.query.QuerySolution;
import org.apache.jena.query.ResultSet;
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

    @BeforeAll
    static void genEndpoint() {
        // Get host and port name to form KG endpoint
        endpoint = "http://" + blazegraph.getHost() + ":" + blazegraph.getFirstMappedPort();
        endpoint = endpoint + "/blazegraph/namespace/kb/sparql"; // Default namespace is "kb"
    }

    @Test
    void testExecSelectQueryAndInsertToEndpoint() {
        String subjectIRI = "http://www.test.org.com/subject";
        String subjectVar = "subject";
        // Test that data can be inserted and queried remotely
        String insertQuery = "INSERT DATA {<" + subjectIRI + "> <http://www.test.org.com/pred> <http://www.test.org.com/object>}";
        QueryHandler.insertToEndpoint(insertQuery, endpoint);
        String selectQuery = "SELECT * WHERE {?" + subjectVar + " <http://www.test.org.com/pred> <http://www.test.org.com/object>}";
        ResultSet results = QueryHandler.execSelectQuery(selectQuery, endpoint);
        // Test that the subject inserted and retrieved is correct
        while (results.hasNext()) {
            QuerySolution soln = results.nextSolution();
            String iri = soln.get(subjectVar).toString();
            assertEquals(subjectIRI, iri);
        }
    }
}
