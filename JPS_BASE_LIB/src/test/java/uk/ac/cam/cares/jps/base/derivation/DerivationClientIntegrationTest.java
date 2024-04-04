package uk.ac.cam.cares.jps.base.derivation;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Random;
import java.util.UUID;

import org.json.JSONArray;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.junit.jupiter.Testcontainers;

import uk.ac.cam.cares.jps.base.BlazegraphContainer;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

/**
 * NOTE: all tests provided in this class are stress tests. It is recommended to
 * run one test at a time.
 * Or you can run testCleanUpFinishedDerivationUpdateStressTest by itself first,
 * then comment out it and run the rest altogether.
 */
@Testcontainers
class DerivationClientIntegrationTest {
    private static RemoteStoreClient storeClient;
    private static DerivationClient devClient;
    private static final String agentIRI = "http://agentIRI";
    private static final String derivationBaseUrl = "http://derivation/";
    private static final Integer numberOfIRIs = 300;
    private static List<String> agentIriList = new ArrayList<>();
    private static List<Boolean> forUpdateFlagList = new ArrayList<>();

    @Container
    private static final BlazegraphContainer blazegraph = new BlazegraphContainer();

    @BeforeAll
    static void initialise() {

        // initialise all variables to be used

        storeClient = blazegraph.getRemoteStoreClient();
        devClient = new DerivationClient(storeClient, derivationBaseUrl);
        for (int i = 0; i < numberOfIRIs; i++) {
            agentIriList.add(agentIRI);
            forUpdateFlagList.add(true);
        }
    }

    @Test
    @Disabled("Temporarily disabled until stress/load tests can be separated from unit tests.")
    void testBulkCreateDerivationsStressTest() {
        // number of derivations -- test run time:
        // 10 -- 2s; 100 -- 6s; 1000 -- 270s
        // create derivations markup in bulk
        List<List<String>> entitiesList = generateIRIs(numberOfIRIs);
        List<List<String>> inputsList = generateIRIs(numberOfIRIs);
        List<String> derivations = devClient.bulkCreateDerivations(entitiesList, agentIriList, inputsList);

        // check that the triples were inserted
        // as the SPARQL update is done in one-go, checking the first derivation is
        // sufficient
        Assertions.assertTrue(derivationsInstantiated(entitiesList.get(0), agentIriList.get(0), inputsList.get(0),
                derivations.get(0), DerivationSparql.ONTODERIVATION_DERIVATION));
    }

    @Test
    @Disabled("Temporarily disabled until stress/load tests can be separated from unit tests.")
    void testBulkCreateDerivationsWithTimeSeriesStressTest() {
        // number of derivations -- test run time:
        // 10 -- 2s; 100 -- 6s; 1000 -- 360s
        // create derivations markup in bulk
        List<List<String>> entitiesList = generateIRIs(numberOfIRIs);
        List<List<String>> inputsList = generateIRIs(numberOfIRIs);
        List<String> derivations = devClient.bulkCreateDerivationsWithTimeSeries(entitiesList, agentIriList,
                inputsList);

        // check that the triples were inserted
        // as the SPARQL update is done in one-go, checking the first derivation is
        // sufficient
        Assertions.assertTrue(derivationsInstantiated(entitiesList.get(0), agentIriList.get(0), inputsList.get(0),
                derivations.get(0), DerivationSparql.ONTODERIVATION_DERIVATIONWITHTIMESERIES));
    }

    @Test
    @Disabled("Temporarily disabled until stress/load tests can be separated from unit tests.")
    void testBulkCreateDerivationsAsyncStressTest() {
        // number of derivations -- test run time:
        // 10 -- 2s; 100 -- 9s; 1000 -- 230s
        // create derivations markup in bulk
        List<List<String>> entitiesList = generateIRIs(numberOfIRIs);
        List<List<String>> inputsList = generateIRIs(numberOfIRIs);
        List<String> derivations = devClient.bulkCreateAsyncDerivations(entitiesList, agentIriList, inputsList,
                forUpdateFlagList);

        // check that the triples were inserted
        // as the SPARQL update is done in one-go, checking the first derivation is
        // sufficient
        Assertions.assertTrue(derivationsInstantiated(entitiesList.get(0), agentIriList.get(0), inputsList.get(0),
                derivations.get(0), DerivationSparql.ONTODERIVATION_DERIVATIONASYN));
    }

    @Test
    @Disabled("Temporarily disabled until stress/load tests can be separated from unit tests.")
    void testBulkCreateDerivationsAsyncForNewInfoStressTest() {
        // number of derivations -- test run time:
        // 10 -- 2s; 100 -- 5s; 1000 -- 145s
        // create derivations markup in bulk
        List<List<String>> inputsList = generateIRIs(numberOfIRIs);
        List<String> derivations = devClient.bulkCreateAsyncDerivationsForNewInfo(agentIriList, inputsList);

        // check that the triples were inserted
        // as the SPARQL update is done in one-go, checking the first derivation is
        // sufficient
        Assertions.assertTrue(derivationsInstantiated(new ArrayList<>(), agentIriList.get(0), inputsList.get(0),
                derivations.get(0), DerivationSparql.ONTODERIVATION_DERIVATIONASYN));
    }

    /**
     * Stress test for the cleanUpFinishedDerivationUpdate method.
     * The test is inspired by the King's Lynn use case where the number of
     * (non-derivation) triples
     * is around 620k, and the number of normal derivations is around 1200.
     * The agent is required to clean up 1 async derivation with ~450 inputs and ~5
     * outputs.
     * In this test, the non-derivation triples were set to 500k, the number of
     * normal derivation
     * is the same as numberOfIRIs (300 as default), and the number of async
     * derivations is 1 with
     * 2000 inputs and 5 outputs. The whole test takes around 60s once the docker
     * container is up.
     *
     * @throws IOException
     */
    @Test
    @Disabled("Temporarily disabled until stress/load tests can be separated from unit tests.")
    void testCleanUpFinishedDerivationUpdateStressTest() throws IOException {
        // stress test:
        // step 1: upload 600K random triples
        System.out.println(System.currentTimeMillis() / 1000);
        uploadRandomTriples(500_000);
        System.out.println(System.currentTimeMillis() / 1000);
        System.out.println("Uploaded 500K random triples");

        // step 2: create numberOfIRIs random derivations
        List<List<String>> entitiesList = generateIRIs(numberOfIRIs);
        List<List<String>> inputsList = generateIRIs(numberOfIRIs);
        List<String> derivations = devClient.bulkCreateDerivations(entitiesList, agentIriList, inputsList);
        System.out.println(System.currentTimeMillis() / 1000);
        System.out.println("Created " + numberOfIRIs + " derivations");

        // step 3: create 1 async derivation (2000 inputs, 5 outputs) to be cleaned up
        // prepare inputs and outputs
        List<String> inputs = createListOfRandomIRIs(2000);
        List<String> outputs = createListOfRandomIRIs(5);
        // add rdf:type of those inputs and outputs
        String inputsType = "http://input_rdf_type";
        String outputsType = "http://output_rdf_type";
        String agentIRI = "http://agent_iri";
        String agentURL = "http://agent_url";
        List<String> newDerivedIRI = initForCleanUpTests(inputs, inputsType, outputs, outputsType, agentIRI, agentURL);

        // create derivation and prepare it for clean up
        String derivation = devClient.createAsyncDerivation(outputs, agentIRI, inputs, true);
        System.out.println(System.currentTimeMillis() / 1000);
        System.out.println("Created 1 async derivation with 2000 inputs and 5 outputs");
        JSONArray results = storeClient.executeQuery(
                String.format("SELECT ?statusIRI WHERE { <%s> <%s> ?statusIRI . }", derivation,
                        DerivationSparql.derivednamespace + "hasStatus"));
        String statusIRI = results.getJSONObject(0).getString("statusIRI");
        System.out.println(System.currentTimeMillis() / 1000);
        System.out.println("Status IRI: " + statusIRI);
        // no need to addTimeInstance as time instances are automatically added when
        // creating derivation markup
        // update timestamp for pure inputs, otherwise updateFinishedAsyncDerivation
        // called by
        // cleanUpFinishedDerivationUpdate will not execute
        devClient.updateTimestamps(inputs);
        System.out.println(System.currentTimeMillis() / 1000);
        System.out.println("Updated timestamps for inputs");
        devClient.sparqlClient.updateStatusBeforeSetupJob(derivation);
        System.out.println(System.currentTimeMillis() / 1000);
        System.out.println("Updated status of the async derivation to \"InProgress\" status");
        devClient.updateStatusAtJobCompletion(derivation, newDerivedIRI, new ArrayList<>());
        System.out.println(System.currentTimeMillis() / 1000);
        System.out.println("Updated status of the async derivation to \"Finished\" status");
        JSONArray results2 = storeClient.executeQuery(
                String.format("SELECT ?retrievedInputsAt WHERE { <%s> <%s> ?retrievedInputsAt . }", derivation,
                        DerivationSparql.derivednamespace + "retrievedInputsAt"));
        long retrievedInputsAt = results2.getJSONObject(0).getLong("retrievedInputsAt");
        System.out.println(System.currentTimeMillis() / 1000);
        System.out.println("retrievedInputsAt: " + retrievedInputsAt);
        System.out.println("Finished setting up the derivation for clean up");

        // execute clean up
        System.out.println("Executing clean up");
        System.out.println(System.currentTimeMillis() / 1000);
        devClient.cleanUpFinishedDerivationUpdate(derivation);
        System.out.println("Finished clean up");
        System.out.println(System.currentTimeMillis() / 1000);

        // tests:
        // there should be no status
        JSONArray results3 = storeClient.executeQuery(
                String.format("SELECT ?statusIRI WHERE { <%s> <%s> ?statusIRI . }", derivation,
                        DerivationSparql.derivednamespace + "hasStatus"));
        System.out.println("results3: " + results3);
        Assertions.assertEquals(0, results3.length());
        JSONArray results4 = storeClient.executeQuery(
                String.format("SELECT ?s ?o WHERE { OPTIONAL {?s ?p1 <%s> .} OPTIONAL {<%s> ?p2 ?o .} }",
                        statusIRI, statusIRI));
        System.out.println("results4: " + results4);
        Assertions.assertTrue(results4.getJSONObject(0).isEmpty());

        // there should be no retrievedTimestampAt
        JSONArray results5 = storeClient.executeQuery(
                String.format("SELECT ?retrievedInputsAt WHERE { <%s> <%s> ?retrievedInputsAt . }", derivation,
                        DerivationSparql.derivednamespace + "retrievedInputsAt"));
        System.out.println("results5: " + results5);
        Assertions.assertEquals(0, results5.length());

        // there should be no uuidLock
        JSONArray resultsUuidLock = storeClient.executeQuery(
                String.format("SELECT ?uuidLock WHERE { <%s> <%s> ?uuidLock . }", derivation,
                        DerivationSparql.derivednamespace + "uuidLock"));
        System.out.println("resultsUuidLock: " + resultsUuidLock);
        Assertions.assertEquals(0, resultsUuidLock.length());

        // outputs should be replaced
        JSONArray results6;
        for (String iri : newDerivedIRI) {
            results6 = storeClient.executeQuery(
                    String.format("ASK { <%s> <%s> <%s>. }",
                            iri, DerivationSparql.derivednamespace + "belongsTo", derivation));
            System.out.println("results6: " + results6);
            Assertions.assertTrue(results6.getJSONObject(0).getBoolean("ASK"));
        }
        // old outputs should be deleted
        JSONArray results7;
        for (String iri : outputs) {
            results7 = storeClient.executeQuery(
                    String.format("SELECT ?s ?o WHERE { OPTIONAL {?s ?p1 <%s> .} OPTIONAL {<%s> ?p2 ?o .} }",
                            iri, iri));
            System.out.println("results7: " + results7);
            Assertions.assertTrue(results7.getJSONObject(0).isEmpty());
        }

        // timestamp should be the same as the one assigned to retrievedInputsAt
        long newtime = devClient.sparqlClient.getTimestamp(derivation);
        System.out.println("newtime: " + newtime);
        Assertions.assertEquals(retrievedInputsAt, newtime);
    }

    ////////////////////
    // Helper methods //
    ////////////////////
    boolean derivationsInstantiated(List<String> entities, String agentIRI,
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
        query.append(
                String.format("<%s> time:hasTime/time:inTimePosition/time:numericPosition ?ts_input. ", inputs.get(0)));
        // derivation timestamp
        query.append(String.format("<%s> time:hasTime/time:inTimePosition/time:numericPosition ?ts_derivation. ",
                derivation));
        // derivation type
        query.append(String.format("<%s> a <%s>. ", derivation, derivationType));
        // status for async derivations
        if (entities.isEmpty() && derivationType.equals(DerivationSparql.ONTODERIVATION_DERIVATIONASYN)) {
            query.append(String.format("<%s> derived:hasStatus ?status. ?status a <%s>. ", derivation,
                    DerivationSparql.derivednamespace + "Requested"));
        }
        query.append("}");
        JSONArray result = storeClient.executeQuery(query.toString());
        Assertions.assertEquals(1, result.length());
        return result.getJSONObject(0).getBoolean("ASK");
    }

    List<String> initForCleanUpTests(List<String> inputs, String inputType,
            List<String> outputs, String outputType,
            String agentIRI, String agentURL) {
        initRdfType(inputs, inputType);
        initRdfType(outputs, outputType);
        List<String> newDerivedIRI = createListOfRandomIRIs(5);
        initRdfType(newDerivedIRI, outputType);
        devClient.createOntoAgentInstance(agentIRI, agentURL,
                Arrays.asList(inputType), Arrays.asList(outputType));
        return newDerivedIRI;
    }

    void initRdfType(List<String> iris, String rdfType) {
        StringBuilder bld = new StringBuilder();
        bld.append("INSERT DATA {");
        for (String iri : iris) {
            bld.append(String.format("<%s> a <%s>. ", iri, rdfType));
        }
        bld.append("}");
        storeClient.executeUpdate(bld.toString());
    }

    void uploadRandomTriples(Integer numberOfTriples) throws IOException {
        // prepare random triples
        List<String> s = createListOfRandomIRIs(numberOfTriples);
        List<String> p = createListOfRandomIRIs(numberOfTriples);
        List<String> o = createListOfRandomIRIs(numberOfTriples);

        // prepare triples as string to be written to file
        StringBuilder bld = new StringBuilder();
        for (int i = 0; i < numberOfTriples; i++) {
            bld.append(String.format("<%s> <%s> <%s>. \n", s.get(i), p.get(i), o.get(i)));
        }
        // create temp file
        File temp = File.createTempFile("pattern", ".nt");
        // delete temp file when program exits
        temp.deleteOnExit();
        // write to temp file
        BufferedWriter out = new BufferedWriter(new FileWriter(temp));
        out.write(bld.toString());
        out.close();
        storeClient.uploadFile(temp);
    }

    List<List<String>> generateIRIs(Integer numberOfIRIs) {
        List<List<String>> iris = new ArrayList<>();
        for (int i = 0; i < numberOfIRIs; i++) {
            iris.add(createListOfRandomIRIs());
        }
        return iris;
    }

    List<String> createListOfRandomIRIs(Integer... numOfIris) {
        int n;
        if (numOfIris.length == 0) {
            Random rand = new Random();
            n = rand.nextInt(30);
            n += 1; // this makes sure n > 0
        } else {
            n = numOfIris[0];
        }
        List<String> iris = new ArrayList<>();
        for (int i = 0; i < n; i++) {
            iris.add("http://" + UUID.randomUUID().toString());
        }
        return iris;
    }
}
