package uk.ac.cam.cares.jps.base.derivation;

import java.util.ArrayList;
import java.util.List;
import java.util.Random;
import java.util.UUID;

import org.json.JSONArray;
import org.junit.Assert;
import org.junit.BeforeClass;
import org.junit.Test;
import org.junit.jupiter.api.AfterAll;
import org.testcontainers.containers.GenericContainer;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.junit.jupiter.Testcontainers;
import org.testcontainers.utility.DockerImageName;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

@Testcontainers
public class DerivationClientIntegrationTest {
    static RemoteStoreClient storeClient;
    static DerivationClient devClient;
    static String kgUrl;
    static final String agentIRI = "http://agentIRI";
    static final String agentURL = "http://agentURL";
    static final String derivationBaseUrl = "http://derivation/";
    static final Integer numberOfIRIs = 300;
    static List<String> agentIriList = new ArrayList<>();
    static List<Boolean> forUpdateFlagList = new ArrayList<>();

    // NOTE: requires access to the docker.cmclinnovations.com registry from the machine the test is run on.
    // For more information regarding the registry, see: https://github.com/cambridge-cares/TheWorldAvatar/wiki/Docker%3A-Image-registry
    @Container
    private static GenericContainer<?> blazegraph = new GenericContainer<>(DockerImageName.parse("docker.cmclinnovations.com/blazegraph_for_tests:1.0.0"))
            .withExposedPorts(9999); // the port is set as 9999 to match with the value set in the docker image

    @BeforeClass
    public static void initialise()
            throws NoSuchMethodException, SecurityException {
        // create the container in a clean state
        try {
            blazegraph.start();
        } catch (Exception e) {
            throw new JPSRuntimeException("DerivationClientIntegrationTest: Docker container startup failed. Please try running tests again");
        }

        // initialise all variables to be used
        kgUrl = "http://" + blazegraph.getHost() + ":" + blazegraph.getFirstMappedPort() + "/blazegraph/namespace/kb/sparql";
        System.out.println(kgUrl);
        storeClient = new RemoteStoreClient(kgUrl, kgUrl);
        devClient = new DerivationClient(storeClient, derivationBaseUrl);
        for (int i = 0; i < numberOfIRIs; i++) {
            agentIriList.add(agentIRI);
            forUpdateFlagList.add(true);
        }
    }

    @AfterAll
    public static void stopContainers() {
        // close containers after all tests
        if (blazegraph.isRunning()) {
            blazegraph.stop();
        }
    }

    @Test
    public void testBulkCreateDerivationsStressTest() {
        // number of derivations -- test run time:
        // 10 -- 2s; 100 -- 6s; 1000 -- 270s
        // create derivations markup in bulk
        List<List<String>> entitiesList = generateIRIs(numberOfIRIs);
        List<List<String>> inputsList = generateIRIs(numberOfIRIs);
        List<String> derivations = devClient.bulkCreateDerivations(entitiesList, agentIriList, inputsList);

        // check that the triples were inserted
        // as the SPARQL update is done in one-go, checking the first derivation is sufficient
        Assert.assertTrue(derivationsInstantiated(entitiesList.get(0), agentIriList.get(0), inputsList.get(0), derivations.get(0), DerivationSparql.ONTODERIVATION_DERIVATION));
    }

    @Test
    public void testBulkCreateDerivationsWithTimeSeriesStressTest() {
        // number of derivations -- test run time:
        // 10 -- 2s; 100 -- 6s; 1000 -- 360s
        // create derivations markup in bulk
        List<List<String>> entitiesList = generateIRIs(numberOfIRIs);
        List<List<String>> inputsList = generateIRIs(numberOfIRIs);
        List<String> derivations = devClient.bulkCreateDerivationsWithTimeSeries(entitiesList, agentIriList, inputsList);

        // check that the triples were inserted
        // as the SPARQL update is done in one-go, checking the first derivation is sufficient
        Assert.assertTrue(derivationsInstantiated(entitiesList.get(0), agentIriList.get(0), inputsList.get(0), derivations.get(0), DerivationSparql.ONTODERIVATION_DERIVATIONWITHTIMESERIES));
    }

    @Test
    public void testBulkCreateDerivationsAsyncStressTest() {
        // number of derivations -- test run time:
        // 10 -- 2s; 100 -- 9s; 1000 -- 230s
        // create derivations markup in bulk
        List<List<String>> entitiesList = generateIRIs(numberOfIRIs);
        List<List<String>> inputsList = generateIRIs(numberOfIRIs);
        List<String> derivations = devClient.bulkCreateAsyncDerivations(entitiesList, agentIriList, inputsList, forUpdateFlagList);

        // check that the triples were inserted
        // as the SPARQL update is done in one-go, checking the first derivation is sufficient
        Assert.assertTrue(derivationsInstantiated(entitiesList.get(0), agentIriList.get(0), inputsList.get(0), derivations.get(0), DerivationSparql.ONTODERIVATION_DERIVATIONASYN));
    }

    @Test
    public void testBulkCreateDerivationsAsyncForNewInfoStressTest() {
        // number of derivations -- test run time:
        // 10 -- 2s; 100 -- 5s; 1000 -- 145s
        // create derivations markup in bulk
        List<List<String>> inputsList = generateIRIs(numberOfIRIs);
        List<String> derivations = devClient.bulkCreateAsyncDerivationsForNewInfo(agentIriList, inputsList);

        // check that the triples were inserted
        // as the SPARQL update is done in one-go, checking the first derivation is sufficient
        Assert.assertTrue(derivationsInstantiated(new ArrayList<>(), agentIriList.get(0), inputsList.get(0), derivations.get(0), DerivationSparql.ONTODERIVATION_DERIVATIONASYN));
    }

    public boolean derivationsInstantiated(List<String> entities, String agentIRI,
            List<String> inputs, String derivation, String derivationType) {
        StringBuilder query = new StringBuilder();
        query.append("PREFIX derived: <" + DerivationSparql.derivednamespace + ">");
        query.append("PREFIX time: <http://www.w3.org/2006/time#>");
        query.append("ASK {");
        // output (only take one IRI is sufficient)
        if (!entities.isEmpty()) {
            query.append(String.format("<%s> derived:belongsTo <%s>. ", entities.get(0), derivation));
        }
        // agent
        query.append(String.format("<%s> derived:isDerivedUsing <%s>. ", derivation, agentIRI));
        // input (only take one IRI is sufficient)
        query.append(String.format("<%s> derived:isDerivedFrom <%s>. ", derivation, inputs.get(0)));
        query.append(String.format("<%s> time:hasTime/time:inTimePosition/time:numericPosition ?ts_input. ", inputs.get(0)));
        // derivation timestamp
        query.append(String.format("<%s> time:hasTime/time:inTimePosition/time:numericPosition ?ts_derivation. ", derivation));
        // derivation type
        query.append(String.format("<%s> a <%s>. ", derivation, derivationType));
        // status for async derivations
        if (entities.isEmpty() && derivationType.equals(DerivationSparql.ONTODERIVATION_DERIVATIONASYN)) {
            query.append(String.format("<%s> derived:hasStatus ?status. ?status a <%s>. ", derivation, DerivationSparql.derivednamespace + "Requested"));
        }
        query.append("}");
        JSONArray result = storeClient.executeQuery(query.toString());
        Assert.assertEquals(1, result.length());
        return result.getJSONObject(0).getBoolean("ASK");
    }

    public List<List<String>> generateIRIs(Integer numberOfIRIs) {
        List<List<String>> iris = new ArrayList<>();
        for (int i = 0; i < numberOfIRIs; i++) {
            iris.add(createListOfRandomIRIs());
        }
        return iris;
    }

    public List<String> createListOfRandomIRIs() {
        Random rand = new Random();
        int n = rand.nextInt(30);
        n += 1; // this makes sure n > 0
        List<String> iris = new ArrayList<>();
        for (int i = 0; i < n; i++) {
            iris.add("http://" + UUID.randomUUID().toString());
        }
        return iris;
    }
}
