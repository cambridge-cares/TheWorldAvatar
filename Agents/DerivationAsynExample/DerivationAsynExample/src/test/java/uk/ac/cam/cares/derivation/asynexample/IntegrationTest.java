package uk.ac.cam.cares.derivation.asynexample;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
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
import uk.ac.cam.cares.jps.base.derivation.DerivationClient;
import uk.ac.cam.cares.jps.base.derivation.DerivationSparql;
import uk.ac.cam.cares.jps.base.derivation.StatusType;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

/**
 * These tests start a Docker container of blazegraph based on "docker.cmclinnovations.com/blazegraph_for_tests:1.0.0"
 * Please refer to TheWorldAvatar/Agents/DerivationAsynExample/README.md for more details.
 * For information regarding the Docker registry, see: https://github.com/cambridge-cares/TheWorldAvatar/wiki/Docker%3A-Image-registry
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

    // NOTE: requires access to the docker.cmclinnovations.com registry from the machine the test is run on.
    // For more information regarding the registry, see: https://github.com/cambridge-cares/TheWorldAvatar/wiki/Docker%3A-Image-registry
    @Container
    private static GenericContainer<?> blazegraph = new GenericContainer<>(DockerImageName.parse("docker.cmclinnovations.com/blazegraph_for_tests:1.0.0"))
                                                        .withExposedPorts(9999); // the port is set as 9999 to match with the value set in the docker image

    @BeforeAll
    public static void initialise() throws NoSuchMethodException, SecurityException {
        // create the container in a clean state
        try {
            blazegraph.start();
        } catch (Exception e) {
            throw new JPSRuntimeException("DerivationAsynExampleIntegrationTest: Docker container startup failed. Please try running tests again");
        }

        // initialise all variables to be used
        kgUrl = "http://" + blazegraph.getHost() + ":" + blazegraph.getFirstMappedPort() + "/blazegraph/namespace/kb/sparql";
        Config.initProperties();
        storeClient = new RemoteStoreClient(kgUrl, kgUrl);
        devClient = new DerivationClient(storeClient, Config.derivationInstanceBaseURL);
        devSparql = new DerivationSparql(storeClient, Config.derivationInstanceBaseURL);
        sparqlClient = new SparqlClient(storeClient);
        getTimestamp = devSparql.getClass().getDeclaredMethod("getTimestamp", String.class);
        getTimestamp.setAccessible(true);
        getStatusType = devSparql.getClass().getDeclaredMethod("getStatusType", String.class);
        getStatusType.setAccessible(true);

        // the response is a JSON object containing the IRIs of the initialised instances, refer to InitialiseInstances for the keys
        InitialiseInstances initialisation = new InitialiseInstances();
        response = initialisation.initialise(sparqlClient, devClient);

        // create the instance of the asyn agents, init() method will be called later in the test case
        rngAgent = new RNGAgent(storeClient, Config.derivationInstanceBaseURL);
        maxValueAgent = new MaxValueAgent(storeClient, Config.derivationInstanceBaseURL);
        minValueAgent = new MinValueAgent(storeClient, Config.derivationInstanceBaseURL);
        differenceAgent = new DifferenceAgent(storeClient, Config.derivationInstanceBaseURL);
    }

    @AfterAll
    public static void stopContainers() {
        // destroy all asyn agents after all tests
        rngAgent.destroy();
        maxValueAgent.destroy();
        minValueAgent.destroy();
        differenceAgent.destroy();

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

        // get IRIs of initialise instances, the keys are located in the servlet InitialiseInstances
        // instances
        String listofrandompoints_instance = response.getString("ListOfRandomPoints instance");
        String maxvalue_instance = response.getString("MaxValue instance");
        String minvalue_instance = response.getString("MinValue instance");
        String difference_instance = response.getString("Difference instance");
        
        // test if only one instance was created for each type of derived quantities
        Assert.assertEquals(listofrandompoints_instance, sparqlClient.getListOfRandomPointsIRI());
        Assert.assertEquals(maxvalue_instance, sparqlClient.getMaxValueIRI());
        Assert.assertEquals(minvalue_instance, sparqlClient.getMinValueIRI());
        Assert.assertEquals(difference_instance, sparqlClient.getDifferenceIRI());
        
        // test if the derived quantities are initiliased with predefined value
        Assert.assertEquals(0, sparqlClient.getValue(maxvalue_instance));
        Assert.assertEquals(0, sparqlClient.getValue(minvalue_instance));
        Assert.assertEquals(0, sparqlClient.getValue(difference_instance));
    }

    @Test
    @Order(2)
    public void testUpdateDerivations() throws IllegalAccessException, IllegalArgumentException, InvocationTargetException {
        // request updating derivations
        UpdateDerivations updateDerivations = new UpdateDerivations();
        updateDerivations.updateDerivations(sparqlClient, devClient);

        // get IRIs of initialise instances, the keys are located in the servlet InitialiseInstances
        // derivations
        String difference_derivation = response.getString("Difference Derivation");
        String maxvalue_derivation = response.getString("MaxValue Derivation");
        String minvalue_derivation = response.getString("MinValue Derivation");
        String rng_derivation = response.getString("RandomNumberGeneration Derivation");

        // test if all derivations were marked as PendingUpdate
//        Assert.assertEquals(StatusType.PENDINGUPDATE, sparqlClient.getDifferenceIRI());
        
        Assert.assertEquals(StatusType.PENDINGUPDATE, (StatusType) getStatusType.invoke(devSparql, difference_derivation));
        Assert.assertEquals(StatusType.PENDINGUPDATE, (StatusType) getStatusType.invoke(devSparql, maxvalue_derivation));
        Assert.assertEquals(StatusType.PENDINGUPDATE, (StatusType) getStatusType.invoke(devSparql, minvalue_derivation));
        Assert.assertEquals(StatusType.PENDINGUPDATE, (StatusType) getStatusType.invoke(devSparql, rng_derivation));
    }

    @Test
    @Order(3)
    public void testInitialiseAgents() throws ServletException {
        // now initialise all agents
        rngAgent.init();
        maxValueAgent.init();
        minValueAgent.init();
        differenceAgent.init();
    }

    @Test
    @Timeout(value = 180, unit = TimeUnit.SECONDS)
    @Order(4)
    public void testRNGDerivation() throws IllegalAccessException, IllegalArgumentException, InvocationTargetException, InterruptedException {
        String rng_derivation = response.getString("RandomNumberGeneration Derivation");
        
        // once timestamp updated, the iri of listofrandompoints should be different from previous value
        currentTimestamp_rng_derivation = (long) getTimestamp.invoke(devSparql, rng_derivation);
        while (currentTimestamp_rng_derivation == 0) {
            TimeUnit.SECONDS.sleep(30);
            currentTimestamp_rng_derivation = (long) getTimestamp.invoke(devSparql, rng_derivation);
        }
        Assert.assertNotEquals(response.getString("ListOfRandomPoints instance"), sparqlClient.getListOfRandomPointsIRI());
        // test if it contains correct number of points
        Assert.assertEquals(sparqlClient.getValue(sparqlClient.getNumberOfPointsIRI()), sparqlClient.getAmountOfPointsInList());
    }

    @Test
    @Timeout(value = 180, unit = TimeUnit.SECONDS)
    @Order(5)
    public void testMaxValueDerivation() throws IllegalAccessException, IllegalArgumentException, InvocationTargetException, InterruptedException {
        String maxvalue_derivation = response.getString("MaxValue Derivation");

        // once timestamp updated, the iri of maxvalue should be different from previous one
        currentTimestamp_maxvalue_derivation = (long) getTimestamp.invoke(devSparql, maxvalue_derivation);
        while (currentTimestamp_maxvalue_derivation == 0) {
            TimeUnit.SECONDS.sleep(30);
            currentTimestamp_maxvalue_derivation = (long) getTimestamp.invoke(devSparql, maxvalue_derivation);
        }
        String maxvalue_instance = sparqlClient.getMaxValueIRI();
        Assert.assertNotEquals(response.getString("MaxValue instance"), maxvalue_instance);
        // test if the value is the same as the max value
        Assert.assertEquals(sparqlClient.getExtremeValueInList(sparqlClient.getListOfRandomPointsIRI(), true), sparqlClient.getValue(maxvalue_instance));
    }

    @Test
    @Timeout(value = 180, unit = TimeUnit.SECONDS)
    @Order(6)
    public void testMinValueDerivation() throws IllegalAccessException, IllegalArgumentException, InvocationTargetException, InterruptedException {
        String minvalue_derivation = response.getString("MinValue Derivation");
        // once timestamp updated, the iri of maxvalue should be different from previous one
        currentTimestamp_minvalue_derivation = (long) getTimestamp.invoke(devSparql, minvalue_derivation);
        while (currentTimestamp_minvalue_derivation == 0) {
            TimeUnit.SECONDS.sleep(30);
            currentTimestamp_minvalue_derivation = (long) getTimestamp.invoke(devSparql, minvalue_derivation);
        }
        String minvalue_instance = sparqlClient.getMinValueIRI();
        Assert.assertNotEquals(response.getString("MinValue instance"), minvalue_instance);
        // test if the value is the same as the min value
        Assert.assertEquals(sparqlClient.getExtremeValueInList(sparqlClient.getListOfRandomPointsIRI(), false), sparqlClient.getValue(minvalue_instance));
    }

    @Test
    @Timeout(value = 180, unit = TimeUnit.SECONDS)
    @Order(7)
    public void testDifferenceDerivation() throws IllegalAccessException, IllegalArgumentException, InvocationTargetException, InterruptedException {
        String difference_derivation = response.getString("Difference Derivation");
        // once timestamp updated, the iri of difference should be different from the previous one, it should have the value same as the max - min value
        currentTimestamp_difference_derivation = (long) getTimestamp.invoke(devSparql, difference_derivation);
        while (currentTimestamp_difference_derivation == 0) {
            TimeUnit.SECONDS.sleep(30);
            currentTimestamp_difference_derivation = (long) getTimestamp.invoke(devSparql, difference_derivation);
        }
        String difference_instance = sparqlClient.getDifferenceIRI();
        Assert.assertNotEquals(response.getString("Difference instance"), difference_instance);
        // test if the value is the same as the difference value
        int difference = sparqlClient.getValue(sparqlClient.getMaxValueIRI()) - sparqlClient.getValue(sparqlClient.getMinValueIRI());
        Assert.assertEquals(difference, sparqlClient.getValue(sparqlClient.getDifferenceIRI()));
    }

    @Test
    @Order(8)
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
    @Order(9)
    public void testUpdateDerivationsAgain() throws IllegalAccessException, IllegalArgumentException, InvocationTargetException, InterruptedException {
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
            TimeUnit.SECONDS.sleep(100);
            currentTimestamp_difference_derivation = (long) getTimestamp.invoke(devSparql, difference_derivation);
        }
        String difference_instance_new = sparqlClient.getDifferenceIRI();
        Assert.assertNotEquals(difference_instance_old, difference_instance_new);
        // test if the value is the same as the difference value
        int difference = sparqlClient.getValue(sparqlClient.getMaxValueIRI()) - sparqlClient.getValue(sparqlClient.getMinValueIRI());
        Assert.assertEquals(difference, sparqlClient.getValue(sparqlClient.getDifferenceIRI()));
    }
}
