package uk.ac.cam.cares.derivation.asynexample;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.concurrent.TimeUnit;

import org.json.JSONObject;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.TestMethodOrder;
import org.junit.jupiter.api.Timeout;
import org.junit.jupiter.api.MethodOrderer.OrderAnnotation;

import uk.ac.cam.cares.jps.base.agent.AsynAgent;
import uk.ac.cam.cares.jps.base.derivation.DerivationSparql;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

/**
 * These tests require the docker stack to be up and running.
 * Please refer to TheWorldAvatar/Agents/DerivationAsynExample/README.md for more details.
 * @author Jiaru Bai (jb2197@cam.ac.uk)
 */
@TestMethodOrder(OrderAnnotation.class)
public class IntegrationTest {
    JSONObject response;
    RemoteStoreClient storeClient;
    DerivationSparql devSparql;
    Method getTimestamp;
    Method isPendingUpdate;
    SparqlClient sparqlClient;
    
    // note that the URLs in the properties file are the URLs when they are accessed from within the docker
    String kgurl = "http://localhost:8889/blazegraph/namespace/kb/sparql";
    String agentDeployURL = "http://localhost:58085/DerivationAsynExample";

    @Before
    public void initialise() throws NoSuchMethodException, SecurityException {
        // the response is a JSON object containing the IRIs of the initialised instances, refer to InitialiseInstances for the key
        response = new JSONObject(AgentCaller.executeGet(agentDeployURL + InitialiseInstances.API_PATTERN));
        Config.initProperties();
        storeClient = new RemoteStoreClient(kgurl, kgurl, Config.kgUser, Config.kgPassword);
        devSparql = new DerivationSparql(storeClient, Config.derivationInstanceBaseURL);
        getTimestamp = devSparql.getClass().getDeclaredMethod("getTimestamp", String.class);
        getTimestamp.setAccessible(true);
        isPendingUpdate = devSparql.getClass().getDeclaredMethod("isPendingUpdate", String.class);
        isPendingUpdate.setAccessible(true);
    }

    @Test
    @Order(1)
    public void testPureInputs() throws IllegalAccessException, IllegalArgumentException, InvocationTargetException {
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
    }

    @Test
    @Order(2)
    public void testDerivationTimestamp() throws IllegalAccessException, IllegalArgumentException, InvocationTargetException {
        // get IRIs of initialise instances, the keys are located in the servlet InitialiseInstances
        // derivations
        String difference_derivation = response.getString("Difference Derivation");
        String maxvalue_derivation = response.getString("MaxValue Derivation");
        String minvalue_derivation = response.getString("MinValue Derivation");
        String rng_derivation = response.getString("RandomNumberGeneration Derivation");
        
        // get the timestamp added to the derivations
        long difference_derivation_timestamp = (long) getTimestamp.invoke(devSparql, difference_derivation);
        long maxvalue_derivation_timestamp = (long) getTimestamp.invoke(devSparql, maxvalue_derivation);
        long minvalue_derivation_timestamp = (long) getTimestamp.invoke(devSparql, minvalue_derivation);
        long rng_derivation_timestamp = (long) getTimestamp.invoke(devSparql, rng_derivation);
        
        // test if timestamps are added correctly
        Assert.assertEquals(difference_derivation_timestamp, 0);
        Assert.assertEquals(maxvalue_derivation_timestamp, 0);
        Assert.assertEquals(minvalue_derivation_timestamp, 0);
        Assert.assertEquals(rng_derivation_timestamp, 0);
    }

    @Test
    @Order(3)
    public void testDerivedQuantities() {
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
    @Order(4)
    public void testUpdateDerivations() throws IllegalAccessException, IllegalArgumentException, InvocationTargetException {
        AgentCaller.executeGet(agentDeployURL + UpdateDerivations.API_PATTERN);
        String difference_derivation = response.getString("Difference Derivation");
        String maxvalue_derivation = response.getString("MaxValue Derivation");
        String minvalue_derivation = response.getString("MinValue Derivation");
        String rng_derivation = response.getString("RandomNumberGeneration Derivation");

        // test if all derivations were marked as PendingUpdate
        Assert.assertTrue((boolean) isPendingUpdate.invoke(devSparql, difference_derivation));
        Assert.assertTrue((boolean) isPendingUpdate.invoke(devSparql, maxvalue_derivation));
        Assert.assertTrue((boolean) isPendingUpdate.invoke(devSparql, minvalue_derivation));
        Assert.assertTrue((boolean) isPendingUpdate.invoke(devSparql, rng_derivation));
    }

    @Test
    @Order(5)
    public void testInitialiseAgents() {
        JSONObject res = new JSONObject(AgentCaller.executeGet(agentDeployURL + RNGAgent.API_PATTERN));
        Assert.assertTrue(res.getString("status") == AsynAgent.msg);

        res = new JSONObject(AgentCaller.executeGet(agentDeployURL + MaxValueAgent.API_PATTERN));
        Assert.assertTrue(res.getString("status") == AsynAgent.msg);

        res = new JSONObject(AgentCaller.executeGet(agentDeployURL + MaxValueAgent.API_PATTERN));
        Assert.assertTrue(res.getString("status") == AsynAgent.msg);

        res = new JSONObject(AgentCaller.executeGet(agentDeployURL + DifferenceAgent.API_PATTERN));
        Assert.assertTrue(res.getString("status") == AsynAgent.msg);
    }

    @Test
    @Timeout(value = 180, unit = TimeUnit.SECONDS)
    @Order(6)
    public void testRNGDerivation() throws IllegalAccessException, IllegalArgumentException, InvocationTargetException {
        String rng_derivation = response.getString("RandomNumberGeneration Derivation");
        
        // once timestamp updated, the iri of listofrandompoints should be different from previous value
        while ((long) getTimestamp.invoke(devSparql, rng_derivation) == 0) {
        }
        Assert.assertNotEquals(response.getString("ListOfRandomPoints instance"), sparqlClient.getListOfRandomPointsIRI());
        // test if it contains correct number of points
        Assert.assertEquals(sparqlClient.getValue(sparqlClient.getNumberOfPointsIRI()), sparqlClient.getAmountOfPointsInList());
    }

    @Test
    @Timeout(value = 180, unit = TimeUnit.SECONDS)
    @Order(7)
    public void testMaxValueDerivation() throws IllegalAccessException, IllegalArgumentException, InvocationTargetException {
        String maxvalue_derivation = response.getString("MaxValue Derivation");

        // once timestamp updated, the iri of maxvalue should be different from previous one
        while ((long) getTimestamp.invoke(devSparql, maxvalue_derivation) == 0) {
        }
        String maxvalue_instance = sparqlClient.getMaxValueIRI();
        Assert.assertNotEquals(response.getString("MaxValue instance"), maxvalue_instance);
        // test if the value is the same as the max value
        Assert.assertEquals(sparqlClient.getExtremeValueInList(sparqlClient.getListOfRandomPointsIRI(), true), sparqlClient.getValue(maxvalue_instance));
    }

    @Test
    @Timeout(value = 180, unit = TimeUnit.SECONDS)
    @Order(8)
    public void testMinValueDerivation() throws IllegalAccessException, IllegalArgumentException, InvocationTargetException {
        String minvalue_derivation = response.getString("MinValue Derivation");
        // once timestamp updated, the iri of maxvalue should be different from previous one
        while ((long) getTimestamp.invoke(devSparql, minvalue_derivation) == 0) {
        }
        String minvalue_instance = sparqlClient.getMinValueIRI();
        Assert.assertNotEquals(response.getString("MinValue instance"), minvalue_instance);
        // test if the value is the same as the min value
        Assert.assertEquals(sparqlClient.getExtremeValueInList(sparqlClient.getListOfRandomPointsIRI(), false), sparqlClient.getValue(minvalue_instance));
    }

    @Test
    @Timeout(value = 180, unit = TimeUnit.SECONDS)
    @Order(9)
    public void testDifferenceDerivation() throws IllegalAccessException, IllegalArgumentException, InvocationTargetException {
        String difference_derivation = response.getString("Difference Derivation");
        // once timestamp updated, the iri of difference should be different from the previous one, it should have the value same as the max - min value
        while ((long) getTimestamp.invoke(devSparql, difference_derivation) == 0) {
        }
        String difference_instance = sparqlClient.getDifferenceIRI();
        Assert.assertNotEquals(response.getString("Difference instance"), difference_instance);
        // test if the value is the same as the difference value
        int difference = sparqlClient.getValue(sparqlClient.getMaxValueIRI()) - sparqlClient.getValue(sparqlClient.getMinValueIRI());
        Assert.assertEquals(difference, sparqlClient.getValue(sparqlClient.getDifferenceIRI()));
    }

    @Test
    @Order(10)
    public void testInputAgent() throws IllegalAccessException, IllegalArgumentException, InvocationTargetException {
        // get information about old NumberOfPoints instance
        String numOfPoint_old = sparqlClient.getNumberOfPointsIRI();
        int numOfPoints_val_old = sparqlClient.getValue(numOfPoint_old);
        long numOfPoints_timestamp_old = (long) getTimestamp.invoke(devSparql, numOfPoint_old);
        // invoke InputAgent
        AgentCaller.executeGet(agentDeployURL + InputAgent.API_PATTERN);

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
    @Order(11)
    public void testUpdateDerivationsAgain() throws IllegalAccessException, IllegalArgumentException, InvocationTargetException {
        // get old timestamp of difference derivation
        String difference_derivation = response.getString("Difference Derivation");
        long difference_derivation_timestamp = (long) getTimestamp.invoke(devSparql, difference_derivation);
        // get information about old instance of difference
        String difference_instance_old = sparqlClient.getDifferenceIRI();

        // invoke update derivation again
        AgentCaller.executeGet(agentDeployURL + UpdateDerivations.API_PATTERN);

        // once timestamp of difference derivation updated, the iri of difference should be different from the previous one, it should have the value same as the max - min value
        while ((long) getTimestamp.invoke(devSparql, difference_derivation) <= difference_derivation_timestamp) {
        }
        String difference_instance_new = sparqlClient.getDifferenceIRI();
        Assert.assertNotEquals(difference_instance_old, difference_instance_new);
        // test if the value is the same as the difference value
        int difference = sparqlClient.getValue(sparqlClient.getMaxValueIRI()) - sparqlClient.getValue(sparqlClient.getMinValueIRI());
        Assert.assertEquals(difference, sparqlClient.getValue(sparqlClient.getDifferenceIRI()));
    }
}
