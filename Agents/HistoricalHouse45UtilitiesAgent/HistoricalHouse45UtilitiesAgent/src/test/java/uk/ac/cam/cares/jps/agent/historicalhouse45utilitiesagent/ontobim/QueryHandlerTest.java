package uk.ac.cam.cares.jps.agent.historicalhouse45utilitiesagent.ontobim;

import org.apache.jena.arq.querybuilder.SelectBuilder;
import org.apache.jena.query.QuerySolution;
import org.apache.jena.query.ResultSet;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.testcontainers.containers.GenericContainer;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.junit.jupiter.Testcontainers;
import org.testcontainers.utility.DockerImageName;

import java.util.ArrayList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

@Testcontainers
class QueryHandlerTest {
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
    void testGenPrefixMappingSelectBuilder() {
        SelectBuilder builder = new SelectBuilder();
        QueryHandler.genPrefixMapping(builder);
        String namespaces = builder.buildString();
        assertTrue(namespaces.contains("ontotimeseries: <https://github.com/cambridge-cares/TheWorldAvatar/blob/main/JPS_Ontology/ontology/ontotimeseries/OntoTimeSeries.owl#>"));
    }

    @Test
    void testGenPrefixMappingInsertQueryBuilder() {
        StringBuilder builder = new StringBuilder();
        QueryHandler.genPrefixMapping(builder);
        String namespaces = builder.toString();
        List<String> expected = genExpectedPrefixList();
        expected.forEach(line -> assertTrue(namespaces.contains(line)));
    }

    @Test
    @Disabled("Requires a running Docker engine to compose containers for a blazegraph database. Disable this test if not available or dockerising")
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

    private static List<String> genExpectedPrefixList() {
        List<String> results = new ArrayList<>();
        results.add("PREFIX rdf:<http://www.w3.org/1999/02/22-rdf-syntax-ns#>");
        results.add("PREFIX om:<http://www.ontology-of-units-of-measure.org/resource/om-2/>");
        results.add("PREFIX skos:<http://www.w3.org/2004/02/skos/core#>");
        results.add("PREFIX qudt:<http://qudt.org/schema/qudt>");
        results.add("PREFIX ontoubemmp:<https://www.theworldavatar.com/kg/ontoubemmp/>");
        return results;
    }
}