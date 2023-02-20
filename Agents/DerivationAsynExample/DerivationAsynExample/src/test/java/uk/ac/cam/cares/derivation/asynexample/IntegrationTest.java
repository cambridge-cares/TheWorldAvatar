package uk.ac.cam.cares.derivation.asynexample;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

import javax.servlet.ServletException;

import org.json.JSONObject;
import org.junit.Assert;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;
import org.junit.jupiter.api.Timeout;
import org.junit.jupiter.api.MethodOrderer.OrderAnnotation;
import org.testcontainers.containers.GenericContainer;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.junit.jupiter.Testcontainers;
import org.testcontainers.utility.DockerImageName;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.derivation.Derivation;
import uk.ac.cam.cares.jps.base.derivation.DerivationClient;
import uk.ac.cam.cares.jps.base.derivation.DerivationSparql;
import uk.ac.cam.cares.jps.base.derivation.StatusType;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

/**
 * These tests start a Docker container of blazegraph based on "ghcr.io/cambridge-cares/blazegraph:1.1.0"
 * Please refer to TheWorldAvatar/Agents/DerivationAsynExample/README.md for more details.
 * 
 * If one is developing in WSL2 and want to keep the container alive after the test (this will be useful when debugging if any test ran into exceptions),
 * then please follow the instruction from [1/5] to [5/5]
 * [1/5] - add the following line to file: ~/.testcontainers.properties
 * testcontainers.reuse.enable=true
 * [2/5] - comment out the line: @Testcontainers
 * [3/5] - comment out the line: @Container
 * [4/5] - uncomment out the line: .withReuse(true)
 * [5/5] - comment out the lines in method stopContainers: if (blazegraph.isRunning()) { blazegraph.stop(); }
 * 
 * @author Jiaru Bai (jb2197@cam.ac.uk)
 * 
 */
@Testcontainers
@TestMethodOrder(OrderAnnotation.class)
public class IntegrationTest extends TestCase {
    static JSONObject response;
    static RemoteStoreClient storeClient;
    static DerivationClient devClient;
    static DerivationSparql devSparql;
    static Method getTimestamp;
    static Method getStatusType;
    static SparqlClient sparqlClient;

    // agents
    static RNGAgent rngAgent;
    static MaxValueAgent maxValueAgent;
    static MinValueAgent minValueAgent;
    static DifferenceAgent differenceAgent;
    static DiffReverseAgent diffReverseAgent;
    static ExceptionThrowAgent exceptionThrowAgent;

    // timestamps
    static long currentTimestamp_rng_derivation;
    static long currentTimestamp_maxvalue_derivation;
    static long currentTimestamp_minvalue_derivation;
    static long currentTimestamp_difference_derivation;
    
    // note that the URLs in the properties file are the URLs when they are accessed from within the docker
    static String kgUrl;

    // the execution of test cases in sequential order (one will only start after the last one is finished) is achieved by:
    // (1) the IntegrationTest class extends TestCase;
    // (2) the sequential lock and override of the setUp and tearDown methods, see: https://stackoverflow.com/a/14609733;
    // (3) the @Order() annotation for each test method.
    // explanation: (3) ensures the test cases will start in order; (1) and (2) ensures only one test case run at a time
    Lock sequential = new ReentrantLock();

    @Override
    protected void setUp() throws Exception {
        super.setUp();
        sequential.lock();
    }

    @Override
    protected void tearDown() throws Exception {
        sequential.unlock();
        super.tearDown();
    }

    @Container
    private static GenericContainer<?> blazegraph;
    static {
        blazegraph = new GenericContainer<>(DockerImageName.parse("ghcr.io/cambridge-cares/blazegraph:1.1.0"))
            // .withReuse(true)
            .withExposedPorts(8080); // the port is set as 8080 to match with the value set in the docker image
    }

    @BeforeAll
    public static void initialise()
            throws NoSuchMethodException, SecurityException {
        // create the container in a clean state
        try {
            blazegraph.start();
        } catch (Exception e) {
            throw new JPSRuntimeException("DerivationAsynExampleIntegrationTest: Docker container startup failed. Please try running tests again");
        }

        // initialise all variables to be used
        kgUrl = "http://" + blazegraph.getHost() + ":" + blazegraph.getFirstMappedPort() + "/blazegraph/namespace/kb/sparql";
        System.out.println(kgUrl);
        Config.initProperties();
        storeClient = new RemoteStoreClient(kgUrl, kgUrl);
        devClient = new DerivationClient(storeClient, Config.derivationInstanceBaseURL);
        devSparql = new DerivationSparql(storeClient, Config.derivationInstanceBaseURL);
        sparqlClient = new SparqlClient(storeClient);
        getTimestamp = devSparql.getClass().getDeclaredMethod("getTimestamp", String.class);
        getTimestamp.setAccessible(true);
        getStatusType = devSparql.getClass().getDeclaredMethod("getStatusType", String.class);
        getStatusType.setAccessible(true);

        try {
            // wait for the blazegraph to be ready
            TimeUnit.SECONDS.sleep(10);
        } catch (InterruptedException e) {
            e.printStackTrace();
        }

        // the response is a JSON object containing the IRIs of the initialised instances, refer to InitialiseInstances for the keys
        InitialiseInstances initialisation = new InitialiseInstances();
        response = initialisation.initialise6(sparqlClient, devClient);

        // create the instance of the asyn agents, init() method will be called later in the test case
        rngAgent = new RNGAgent(storeClient, Config.derivationInstanceBaseURL);
        maxValueAgent = new MaxValueAgent(storeClient, Config.derivationInstanceBaseURL);
        minValueAgent = new MinValueAgent(storeClient, Config.derivationInstanceBaseURL);
        differenceAgent = new DifferenceAgent(storeClient, Config.derivationInstanceBaseURL);
        diffReverseAgent = new DiffReverseAgent(storeClient, Config.derivationInstanceBaseURL);
        exceptionThrowAgent = new ExceptionThrowAgent(storeClient, Config.derivationInstanceBaseURL);
    }

    @AfterAll
    public static void stopContainers() {
        // destroy all asyn agents after all tests
        rngAgent.destroy();
        maxValueAgent.destroy();
        minValueAgent.destroy();
        differenceAgent.destroy();
        diffReverseAgent.destroy();
        exceptionThrowAgent.destroy();

        // close containers after all tests
        if (blazegraph.isRunning()) {
            blazegraph.stop();
        }
    }

    @Test
    @Order(1)
    public void testInitialiseInstances() throws IllegalAccessException, IllegalArgumentException, InvocationTargetException {
        // get IRIs of initialise instances, the keys are located in the servlet InitialiseInstances
        // instances
        String upperlimit_instance = response.getString("UpperLimit instance");
        String lowerlimit_instance = response.getString("LowerLimit instance");
        String numofpoints_instance = response.getString("NumberOfPoints instance");
        
        // get the timestamp added to the instances
        long upperlimit_instance_timestamp = (long) getTimestamp.invoke(devSparql, upperlimit_instance);
        long lowerlimit_instance_timestamp = (long) getTimestamp.invoke(devSparql, lowerlimit_instance);
        long numofpoints_instance_timestamp = (long) getTimestamp.invoke(devSparql, numofpoints_instance);
        
        // test if timestamps are added correctly
        Assert.assertTrue(upperlimit_instance_timestamp > 0);
        Assert.assertTrue(lowerlimit_instance_timestamp > 0);
        Assert.assertTrue(numofpoints_instance_timestamp > 0);

        // test if only one instance was created for each type of pure inputs
        Assert.assertEquals(upperlimit_instance, sparqlClient.getUpperLimitIRI());
        Assert.assertEquals(lowerlimit_instance, sparqlClient.getLowerLimitIRI());
        Assert.assertEquals(numofpoints_instance, sparqlClient.getNumberOfPointsIRI());
        
        // test if the pure inputs are initiliased with predefined value
        Assert.assertEquals(20, sparqlClient.getValue(upperlimit_instance));
        Assert.assertEquals(3, sparqlClient.getValue(lowerlimit_instance));
        Assert.assertEquals(6, sparqlClient.getValue(numofpoints_instance));

        // get IRIs of initialise instances, the keys are located in the servlet InitialiseInstances
        // derivations
        String difference_derivation = response.getString("Difference Derivation");
        String maxvalue_derivation = response.getString("MaxValue Derivation");
        String minvalue_derivation = response.getString("MinValue Derivation");
        String rng_derivation = response.getString("RandomNumberGeneration Derivation");
        
        // get the timestamp added to the derivations
        currentTimestamp_difference_derivation = (long) getTimestamp.invoke(devSparql, difference_derivation);
        currentTimestamp_maxvalue_derivation = (long) getTimestamp.invoke(devSparql, maxvalue_derivation);
        currentTimestamp_minvalue_derivation = (long) getTimestamp.invoke(devSparql, minvalue_derivation);
        currentTimestamp_rng_derivation = (long) getTimestamp.invoke(devSparql, rng_derivation);
        
        // test if timestamps are added correctly
        Assert.assertEquals(currentTimestamp_difference_derivation, 0);
        Assert.assertEquals(currentTimestamp_maxvalue_derivation, 0);
        Assert.assertEquals(currentTimestamp_minvalue_derivation, 0);
        Assert.assertEquals(currentTimestamp_rng_derivation, 0);

        // test that NO instance should be created for each type of derived quantities
        Assert.assertTrue(sparqlClient.getListOfRandomPointsIRI().isEmpty());
        Assert.assertTrue(sparqlClient.getMaxValueIRI().isEmpty());
        Assert.assertTrue(sparqlClient.getMinValueIRI().isEmpty());
        Assert.assertTrue(sparqlClient.getDifferenceIRI().isEmpty());

        // test if all derivations were marked as Requested
        Assert.assertEquals(StatusType.REQUESTED, (StatusType) getStatusType.invoke(devSparql, difference_derivation));
        Assert.assertEquals(StatusType.REQUESTED, (StatusType) getStatusType.invoke(devSparql, maxvalue_derivation));
        Assert.assertEquals(StatusType.REQUESTED, (StatusType) getStatusType.invoke(devSparql, minvalue_derivation));
        Assert.assertEquals(StatusType.REQUESTED, (StatusType) getStatusType.invoke(devSparql, rng_derivation));
    }

    @Test
    @Order(2)
    public void testInitialiseAgents() throws ServletException {
        // now initialise all agents
        // except for the diffReverseAgent, which will be used individually in testMultipleAsyncDerivations
        rngAgent.init();
        maxValueAgent.init();
        minValueAgent.init();
        differenceAgent.init();
    }

    @Test
    @Timeout(value = 180, unit = TimeUnit.SECONDS)
    @Order(3)
    public void testRNGDerivation() throws IllegalAccessException, IllegalArgumentException, InvocationTargetException, InterruptedException {
        String rng_derivation = response.getString("RandomNumberGeneration Derivation");
        
        // once timestamp updated, the iri of listofrandompoints should be different from previous value
        currentTimestamp_rng_derivation = (long) getTimestamp.invoke(devSparql, rng_derivation);
        while (currentTimestamp_rng_derivation == 0) {
            TimeUnit.SECONDS.sleep(30);
            currentTimestamp_rng_derivation = (long) getTimestamp.invoke(devSparql, rng_derivation);
        }
        // wait arbitrary amount of time so that the cleaning up is finished
        TimeUnit.SECONDS.sleep(5);
        // test if it contains correct number of points
        Assert.assertEquals(sparqlClient.getValue(sparqlClient.getNumberOfPointsIRI()), sparqlClient.getAmountOfPointsInList());
        Assert.assertEquals(sparqlClient.getValue(sparqlClient.getNumberOfPointsIRI()),
                sparqlClient.getAmountOfPointsInKG());
    }

    @Test
    @Timeout(value = 180, unit = TimeUnit.SECONDS)
    @Order(4)
    public void testMaxValueDerivation() throws IllegalAccessException, IllegalArgumentException, InvocationTargetException, InterruptedException {
        String maxvalue_derivation = response.getString("MaxValue Derivation");

        // once timestamp updated, the iri of maxvalue should be different from previous one
        currentTimestamp_maxvalue_derivation = (long) getTimestamp.invoke(devSparql, maxvalue_derivation);
        while (currentTimestamp_maxvalue_derivation == 0) {
            TimeUnit.SECONDS.sleep(30);
            currentTimestamp_maxvalue_derivation = (long) getTimestamp.invoke(devSparql, maxvalue_derivation);
        }
        // wait arbitrary amount of time so that the cleaning up is finished
        TimeUnit.SECONDS.sleep(5);
        String maxvalue_instance = sparqlClient.getMaxValueIRI();
        // test if the value is the same as the max value
        Assert.assertEquals(sparqlClient.getExtremeValueInList(sparqlClient.getListOfRandomPointsIRI(), true), sparqlClient.getValue(maxvalue_instance));
    }

    @Test
    @Timeout(value = 180, unit = TimeUnit.SECONDS)
    @Order(5)
    public void testMinValueDerivation() throws IllegalAccessException, IllegalArgumentException, InvocationTargetException, InterruptedException {
        String minvalue_derivation = response.getString("MinValue Derivation");
        // once timestamp updated, the iri of maxvalue should be different from previous one
        currentTimestamp_minvalue_derivation = (long) getTimestamp.invoke(devSparql, minvalue_derivation);
        while (currentTimestamp_minvalue_derivation == 0) {
            TimeUnit.SECONDS.sleep(30);
            currentTimestamp_minvalue_derivation = (long) getTimestamp.invoke(devSparql, minvalue_derivation);
        }
        // wait arbitrary amount of time so that the cleaning up is finished
        TimeUnit.SECONDS.sleep(5);
        String minvalue_instance = sparqlClient.getMinValueIRI();
        // test if the value is the same as the min value
        Assert.assertEquals(sparqlClient.getExtremeValueInList(sparqlClient.getListOfRandomPointsIRI(), false), sparqlClient.getValue(minvalue_instance));
    }

    @Test
    @Timeout(value = 180, unit = TimeUnit.SECONDS)
    @Order(6)
    public void testDifferenceDerivation() throws IllegalAccessException, IllegalArgumentException, InvocationTargetException, InterruptedException {
        String difference_derivation = response.getString("Difference Derivation");
        // once timestamp updated, the iri of difference should be different from the previous one, it should have the value same as the max - min value
        currentTimestamp_difference_derivation = (long) getTimestamp.invoke(devSparql, difference_derivation);
        while (currentTimestamp_difference_derivation == 0) {
            TimeUnit.SECONDS.sleep(30);
            currentTimestamp_difference_derivation = (long) getTimestamp.invoke(devSparql, difference_derivation);
        }
        // wait arbitrary amount of time so that the cleaning up is finished
        TimeUnit.SECONDS.sleep(5);
        // test if the value is the same as the difference value
        int difference = sparqlClient.getValue(sparqlClient.getMaxValueIRI()) - sparqlClient.getValue(sparqlClient.getMinValueIRI());
        Assert.assertEquals(difference, sparqlClient.getValue(sparqlClient.getDifferenceIRI()));
    }

    @Test
    @Order(7)
    public void testInputAgent() throws IllegalAccessException, IllegalArgumentException, InvocationTargetException {
        // get information about old NumberOfPoints instance
        String numOfPoint_old = sparqlClient.getNumberOfPointsIRI();
        int numOfPoints_val_old = sparqlClient.getValue(numOfPoint_old);
        long numOfPoints_timestamp_old = (long) getTimestamp.invoke(devSparql, numOfPoint_old);
        // invoke InputAgent
        InputAgent inputAgent = new InputAgent();
        inputAgent.updateNumberOfPoints(sparqlClient, devClient);

        // get information about new NumberOfPoints instance
        String numOfPoint_new = sparqlClient.getNumberOfPointsIRI();
        int numOfPoints_val_new = sparqlClient.getValue(numOfPoint_new);
        long numOfPoints_timestamp_new = (long) getTimestamp.invoke(devSparql, numOfPoint_new);
        // test if InputAgent increased the value
        Assert.assertEquals(numOfPoints_val_old + 1, numOfPoints_val_new);
        // test if InputAgent modified the timestamp
        Assert.assertTrue(numOfPoints_timestamp_new > numOfPoints_timestamp_old);
    }

    @Test
    @Timeout(value = 300, unit = TimeUnit.SECONDS)
    @Order(8)
    public void testUpdateDerivations() throws IllegalAccessException, IllegalArgumentException, InvocationTargetException, InterruptedException {
        // get old timestamp of difference derivation
        String difference_derivation = response.getString("Difference Derivation");
        long difference_derivation_timestamp = (long) getTimestamp.invoke(devSparql, difference_derivation);
        // get information about old instance of difference
        String difference_instance_old = sparqlClient.getDifferenceIRI();

        // invoke update derivation again
        UpdateDerivations updateDerivations = new UpdateDerivations();
        updateDerivations.updateDerivations(sparqlClient, devClient);

        // once timestamp of difference derivation updated, the iri of difference should be different from the previous one, it should have the value same as the max - min value
        currentTimestamp_difference_derivation = (long) getTimestamp.invoke(devSparql, difference_derivation);
        while (currentTimestamp_difference_derivation <= difference_derivation_timestamp) {
            TimeUnit.SECONDS.sleep(20);
            currentTimestamp_difference_derivation = (long) getTimestamp.invoke(devSparql, difference_derivation);
        }
        // wait arbitrary amount of time so that the cleaning up is finished
        TimeUnit.SECONDS.sleep(5);
        String difference_instance_new = sparqlClient.getDifferenceIRI();
        Assert.assertNotEquals(difference_instance_old, difference_instance_new);
        // test if the value is the same as the difference value
        int difference = sparqlClient.getValue(sparqlClient.getMaxValueIRI()) - sparqlClient.getValue(sparqlClient.getMinValueIRI());
        Assert.assertEquals(difference, sparqlClient.getValue(sparqlClient.getDifferenceIRI()));
    }

    @Test
    @Timeout(value = 300, unit = TimeUnit.SECONDS)
    @Order(9)
    public void testMultipleAsyncDerivations() throws InterruptedException {
        String maxvalue_instance = sparqlClient.getMaxValueIRI();
        String minvalue_instance = sparqlClient.getMinValueIRI();

        // create two derivations, and call monitorAsyncDerivations with the periodicalTimescale of (Config.delayAgentDiffReverse - 1)
        // so that this makes sure that only one derivation will be updated after the call
        String diff_dev_1 = devClient.createAsyncDerivationForNewInfo(Config.agentIriDiffReverse, Arrays.asList(maxvalue_instance, minvalue_instance));
        String diff_dev_2 = devClient.createAsyncDerivationForNewInfo(Config.agentIriDiffReverse, Arrays.asList(maxvalue_instance, minvalue_instance));
        diffReverseAgent.monitorAsyncDerivations(Config.agentIriDiffReverse, Config.delayAgentDiffReverse - 1);
        // now check only one derivation is "Finished"
        Map<String, StatusType> diffReverseDerivations = devClient.getDerivationsAndStatusType(Config.agentIriDiffReverse);
        Assert.assertEquals(2, diffReverseDerivations.size());
        Assert.assertEquals(1, countNumberOfDerivationsGivenStatusType(diffReverseDerivations, StatusType.FINISHED));
        Assert.assertEquals(0, countNumberOfDerivationsGivenStatusType(diffReverseDerivations, StatusType.NOSTATUS));
        Assert.assertEquals(1, countNumberOfDerivationsGivenStatusType(diffReverseDerivations, StatusType.REQUESTED));

        // call monitorAsyncDerivations again
        diffReverseAgent.monitorAsyncDerivations(Config.agentIriDiffReverse, Config.delayAgentDiffReverse - 1);
        // now check that one derivation should be up-to-date and the other one is "Finished"
        diffReverseDerivations = devClient.getDerivationsAndStatusType(Config.agentIriDiffReverse);
        Assert.assertEquals(2, diffReverseDerivations.size());
        Assert.assertEquals(1, countNumberOfDerivationsGivenStatusType(diffReverseDerivations, StatusType.FINISHED));
        Assert.assertEquals(1, countNumberOfDerivationsGivenStatusType(diffReverseDerivations, StatusType.NOSTATUS));
        Assert.assertEquals(0, countNumberOfDerivationsGivenStatusType(diffReverseDerivations, StatusType.REQUESTED));

        // call monitorAsyncDerivations again
        diffReverseAgent.monitorAsyncDerivations(Config.agentIriDiffReverse, Config.delayAgentDiffReverse - 1);
        // now check that both derivations should be up-to-date
        diffReverseDerivations = devClient.getDerivationsAndStatusType(Config.agentIriDiffReverse);
        Assert.assertEquals(2, diffReverseDerivations.size());
        Assert.assertEquals(2, countNumberOfDerivationsGivenStatusType(diffReverseDerivations, StatusType.NOSTATUS));

        // create three more derivations, and call monitorAsyncDerivations with the periodicalTimescale of (8 * Config.delayAgentDiffReverse)
        // so that this makes sure that all derivations will be updated after the call
        String diff_dev_3 = devClient.createAsyncDerivationForNewInfo(Config.agentIriDiffReverse, Arrays.asList(maxvalue_instance, minvalue_instance));
        String diff_dev_4 = devClient.createAsyncDerivationForNewInfo(Config.agentIriDiffReverse, Arrays.asList(maxvalue_instance, minvalue_instance));
        String diff_dev_5 = devClient.createAsyncDerivationForNewInfo(Config.agentIriDiffReverse, Arrays.asList(maxvalue_instance, minvalue_instance));
        diffReverseAgent.monitorAsyncDerivations(Config.agentIriDiffReverse, 8 * Config.delayAgentDiffReverse);
        // now check that all derivations should be up-to-date
        diffReverseDerivations = devClient.getDerivationsAndStatusType(Config.agentIriDiffReverse);
        Assert.assertEquals(5, diffReverseDerivations.size());
        Assert.assertEquals(5, countNumberOfDerivationsGivenStatusType(diffReverseDerivations, StatusType.NOSTATUS));

        // also all values should be the same and they are the difference of min and max
        Map<String, Integer> diffReverseValues = sparqlClient.getDiffReverseValues();
        Assert.assertEquals(5, diffReverseValues.size());
        int difference = sparqlClient.getValue(sparqlClient.getDifferenceIRI());
        diffReverseValues.values().stream().forEach(val -> Assert.assertEquals(0, val + difference));
    }

    @Test
    @Timeout(value = 180, unit = TimeUnit.SECONDS)
    @Order(10)
    public void testErrorStatus() throws ServletException, InterruptedException {
        // first initialise exceptionThrowAgent
        exceptionThrowAgent.init();
        // initialise all triples for exception throw test
        InitialiseInstances initialiseExceptionThrow = new InitialiseInstances();
        JSONObject exceptionThrowResponse = initialiseExceptionThrow.initialiseExceptionThrow(sparqlClient, devClient);
        String inputPlaceholderExceptionThrowIri = exceptionThrowResponse.getString(InitialiseInstances.input_placeholder_exc_throw_key);
        // create three derivations and wait for the status to be changed to Error
        String exceptionThrowDerivation1 = devClient.createAsyncDerivationForNewInfo(Config.agentIriExceptionThrow, Arrays.asList(inputPlaceholderExceptionThrowIri));
        String exceptionThrowDerivation2 = devClient.createAsyncDerivationForNewInfo(Config.agentIriExceptionThrow, Arrays.asList(inputPlaceholderExceptionThrowIri));
        String exceptionThrowDerivation3 = devClient.createAsyncDerivationForNewInfo(Config.agentIriExceptionThrow, Arrays.asList(inputPlaceholderExceptionThrowIri));
        // wait for init delay and five periods, which should be sufficient for agent to iterate through all derivations
        TimeUnit.SECONDS.sleep(Config.initDelayAgentExceptionThrow + 5 * Config.periodAgentExceptionThrow);
        // if the amount of derivations in Error status matches the amount of total derivations got marked up
        // then it implies the agent was able to catch the exception and proceed to next derivation without getting stuck
        Map<String, StatusType> excThrowDerivations = devClient.getDerivationsAndStatusType(Config.agentIriExceptionThrow);
        Assert.assertEquals(3, excThrowDerivations.size());
        Assert.assertEquals(3, countNumberOfDerivationsGivenStatusType(excThrowDerivations, StatusType.ERROR));
        // also all of the error message recorded in rdfs:comment should have the error message defined in the ExceptionThrowAgent
        List<Derivation> errDerivations = devClient.getDerivationsInErrorStatus(Config.agentIriExceptionThrow);
        Assert.assertEquals(3, errDerivations.size());
        errDerivations.stream().forEach(d -> {
            Assert.assertTrue(d.getErrMsg().contains(ExceptionThrowAgent.EXCEPTION_MESSAGE));
        });
    }

    public int countNumberOfDerivationsGivenStatusType(Map<String, StatusType> derivationsAndStatusType, StatusType statusType) {
        return (int) derivationsAndStatusType.values().stream().filter(status -> status.equals(statusType)).count();
    }
}
