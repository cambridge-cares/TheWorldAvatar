package uk.ac.cam.cares.jps.agent.sparql;

import org.apache.jena.query.*;
import org.apache.jena.sparql.exec.http.QueryExecutionHTTP;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.testcontainers.containers.GenericContainer;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.junit.jupiter.Testcontainers;
import org.testcontainers.utility.DockerImageName;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;

import static org.junit.jupiter.api.Assertions.assertTrue;

@Testcontainers
class SparqlAdapterIntegrationT {
    @Container
    private static final GenericContainer blazegraph = new GenericContainer(DockerImageName.parse("nawer/blazegraph:latest"))
            .withExposedPorts(9999);
    private static String endpoint;
    private static String baseURI;
    private static final String elecPrice = "ElectricityPrice";
    private static final String elecPriceInst = elecPrice + "_1515";
    private static final String elecConsumption = "ElectricityConsumption";
    private static final String elecConsumptionInst = elecConsumption + "_3013";

    @BeforeAll
    static void init() {
        // Get host and port name to form KG endpoint
        endpoint = "http://" + blazegraph.getHost() + ":" + blazegraph.getFirstMappedPort();
        baseURI = endpoint + "/blazegraph/namespace/kb/"; // Default namespace is "kb"
        endpoint = baseURI + "sparql";
    }

    @Test
    void addSupplementaryTriples() {
        Map<String, Map<String, String>> sampleIriMappings = genSampleIriMappings();
        SparqlAdapter.addSupplementaryTriples(endpoint, endpoint, sampleIriMappings);
        String results = retrieveTriples();
        List<String> expectedStatements = genExpectedStatements();
        for (String statement : expectedStatements) {
            assertTrue(Pattern.compile(statement).matcher(results).find());
        }
    }

    private static Map<String, Map<String, String>> genSampleIriMappings() {
        Map<String, Map<String, String>> sampleIriMappings = new HashMap<>();
        Map<String, String> nestedMap = new HashMap<>();
        nestedMap.put(elecPrice, baseURI + elecPriceInst);
        nestedMap.put(elecConsumption, baseURI + elecConsumptionInst);
        sampleIriMappings.put("PwMwFehrbach", nestedMap);
        return sampleIriMappings;
    }

    private static String retrieveTriples() {
        String subjectVar = "s";
        String predVar = "p";
        String objectVar = "o";
        StringBuilder triples = new StringBuilder();
        // Create select query
        String selectQuery = "SELECT * WHERE {?" + subjectVar + " ?" + predVar + " ?" + objectVar + "}";
        Query query = QueryFactory.create(selectQuery);
        // Execute query
        try (QueryExecution qExec = QueryExecutionHTTP.service(endpoint, query)) {
            ResultSet results = qExec.execSelect();
            // Extract and store the statements into a result string
            while (results.hasNext()) {
                QuerySolution soln = results.nextSolution();
                String statement = soln.get(subjectVar).toString();
                statement += " " + soln.get(predVar).toString();
                statement += " " + soln.get(objectVar).toString() + "\n";
                triples.append(statement);
            }
            return triples.toString();
        }
    }

    private static List<String> genExpectedStatements() {
        String pumpInst = "http://www.theworldavatar.com/kg/ps/pumpstation-1.29 ";
        String consumptionQuantityInst = baseURI + "ElectricityConsumption_Quantity_.*";
        String costQuantityInst = baseURI + "CostInTimeInterval_Quantity_.*";
        List<String> expectedStatements = new ArrayList<>();
        expectedStatements.add(pumpInst + OntologyConstant.ONTOCAPE_URI + "hasUtilityCost " + costQuantityInst);
        expectedStatements.add(pumpInst + OntologyConstant.UBEMMP_URI + "consumesUtilities " + consumptionQuantityInst);
        expectedStatements.add(costQuantityInst + OntologyConstant.OM_URI + "hasValue " + baseURI + elecPriceInst);
        expectedStatements.add(costQuantityInst + OntologyConstant.RDF_URI + "type " + OntologyConstant.ONTOHEATNETWORK_URI + "CostInTimeInterval");
        expectedStatements.add(consumptionQuantityInst + OntologyConstant.OM_URI + "hasValue " + baseURI + elecConsumptionInst);
        expectedStatements.add(consumptionQuantityInst + OntologyConstant.RDF_URI + "type " + OntologyConstant.UBEMMP_URI + "ElectricityConsumption");
        expectedStatements.add(baseURI + elecConsumptionInst + " " + OntologyConstant.OM_URI + "hasUnit " + OntologyConstant.OM_URI + "kilowattHour");
        expectedStatements.add(OntologyConstant.OM_URI + "kilowattHour " + OntologyConstant.SKOS_URI + "notation kW\\.h\\^\\^" + OntologyConstant.QUDT_URI + "UCUMcs");
        return expectedStatements;
    }
}