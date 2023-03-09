package uk.ac.cam.cares.derivation.asynexample;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

import org.json.JSONObject;
import org.junit.After;
import org.junit.Assert;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;
import org.junit.jupiter.api.Timeout;
import org.junit.jupiter.api.MethodOrderer.OrderAnnotation;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.derivation.Derivation;
import uk.ac.cam.cares.jps.base.derivation.DerivationClient;
import uk.ac.cam.cares.jps.base.derivation.DerivationSparql;
import uk.ac.cam.cares.jps.base.derivation.StatusType;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

/**
 * These tests require the docker stack defined in
 * TheWorldAvatar/Agents/DerivationAsynExample/docker-compose.yml to be up and
 * running
 * Please refer to TheWorldAvatar/Agents/DerivationAsynExample/README.md for
 * more details.
 * 
 * @author Jiaru Bai (jb2197@cam.ac.uk)
 * 
 */
@TestMethodOrder(OrderAnnotation.class)
public class DockerIntegrationTest extends TestCase {
	// static JSONObject response;
	static RemoteStoreClient storeClient;
	static DerivationClient devClient;
	static DerivationSparql devSparql;
	static Method getTimestamp;
	static Method getStatusType;
	static SparqlClient sparqlClient;
	static String host = "http://localhost:58085/DerivationAsynExample";
	static String kgUrl = "http://localhost:8889/blazegraph/namespace/kb/sparql";

	// the execution of test cases in sequential order (one will only start after
	// the last one is finished) is achieved by:
	// (1) the IntegrationTest class extends TestCase;
	// (2) the sequential lock and override of the setUp and tearDown methods, see:
	// https://stackoverflow.com/a/14609733;
	// (3) the @Order() annotation for each test method.
	// explanation: (3) ensures the test cases will start in order; (1) and (2)
	// ensures only one test case run at a time
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

	@BeforeAll
	public static void initialise() throws NoSuchMethodException, SecurityException {
		Config.initProperties();
		// Config.sparqlEndpoint is the endpoint that used within the docker container
		// here we need to initialise RemoteStoreClient with the url outside docker
		storeClient = new RemoteStoreClient(kgUrl, kgUrl, Config.kgUser, Config.kgPassword);
		devClient = new DerivationClient(storeClient, Config.derivationInstanceBaseURL);
		devSparql = new DerivationSparql(storeClient, Config.derivationInstanceBaseURL);
		sparqlClient = new SparqlClient(storeClient);
		getTimestamp = devSparql.getClass().getDeclaredMethod("getTimestamp", String.class);
		getTimestamp.setAccessible(true);
		getStatusType = devSparql.getClass().getDeclaredMethod("getStatusType", String.class);
		getStatusType.setAccessible(true);
	}

	@After
	public static void pulse() throws InterruptedException {
		TimeUnit.SECONDS.sleep(2);
	}

	@Test
	@Order(1)
	public void testAllSync()
			throws IllegalAccessException, IllegalArgumentException, InvocationTargetException, InterruptedException {
		//////////////////////////////////
		// CASE 1 - All Derivation Sync //
		//////////////////////////////////

		// stage 1: initialise triples for derivation DAG
		JSONObject response = new JSONObject(AgentCaller.executeGet(host + InitialiseInstances.API_PATTERN_1));
		executeAndTestInitialiseInstances(response, true, true, true, true);

		// stage 2: call InputAgent to make all derivations outdated again
		executeAndTestInputAgent();

		// stage 3: call UpdateDerivations
		executeAndTestUpdateDerivations(response);
	}

	@Test
	@Timeout(value = 180, unit = TimeUnit.SECONDS)
	@Order(2)
	public void testDiffAsync()
			throws IllegalAccessException, IllegalArgumentException, InvocationTargetException, InterruptedException {
		//////////////////////////////////////////
		// CASE 2 - Difference Derivation Async //
		//////////////////////////////////////////

		// stage 1: initialise triples for derivation DAG
		JSONObject response = new JSONObject(AgentCaller.executeGet(host + InitialiseInstances.API_PATTERN_2));
		executeAndTestInitialiseInstances(response, true, true, true, false);

		// stage 2: call InputAgent to make all derivations outdated again
		executeAndTestInputAgent();

		// stage 3: call UpdateDerivations
		executeAndTestUpdateDerivations(response);
	}

	@Test
	@Timeout(value = 180, unit = TimeUnit.SECONDS)
	@Order(3)
	public void testMaxDiffAsync()
			throws IllegalAccessException, IllegalArgumentException, InvocationTargetException, InterruptedException {
		///////////////////////////////////////////////////
		// CASE 3 - MaxValue/Difference Derivation Async //
		///////////////////////////////////////////////////

		// stage 1: initialise triples for derivation DAG
		JSONObject response = new JSONObject(AgentCaller.executeGet(host + InitialiseInstances.API_PATTERN_3));
		executeAndTestInitialiseInstances(response, true, false, true, false);

		// stage 2: call InputAgent to make all derivations outdated again
		executeAndTestInputAgent();

		// stage 3: call UpdateDerivations
		executeAndTestUpdateDerivations(response);
	}

	@Test
	@Timeout(value = 180, unit = TimeUnit.SECONDS)
	@Order(4)
	public void testMinDiffAsync()
			throws IllegalAccessException, IllegalArgumentException, InvocationTargetException, InterruptedException {
		///////////////////////////////////////////////////
		// CASE 4 - MinValue/Difference Derivation Async //
		///////////////////////////////////////////////////

		// stage 1: initialise triples for derivation DAG
		JSONObject response = new JSONObject(AgentCaller.executeGet(host + InitialiseInstances.API_PATTERN_4));
		executeAndTestInitialiseInstances(response, true, true, false, false);

		// stage 2: call InputAgent to make all derivations outdated again
		executeAndTestInputAgent();

		// stage 3: call UpdateDerivations
		executeAndTestUpdateDerivations(response);
	}

	@Test
	@Timeout(value = 180, unit = TimeUnit.SECONDS)
	@Order(5)
	public void testAllAsync()
			throws IllegalAccessException, IllegalArgumentException, InvocationTargetException, InterruptedException {
		///////////////////////////////////
		// CASE 6 - All Derivation Async //
		///////////////////////////////////

		// stage 1: initialise triples for derivation DAG
		JSONObject response = new JSONObject(AgentCaller.executeGet(host + InitialiseInstances.API_PATTERN_6));
		executeAndTestInitialiseInstances(response, false, false, false, false);

		// stage 2: call InputAgent to make all derivations outdated again
		executeAndTestInputAgent();

		// stage 3: call UpdateDerivations
		executeAndTestUpdateDerivations(response);
	}

	// NOTE testMaxMinDiffAsync can now pass with the changes made to only do SPARQL
	// update when reconnecting new derived IRIs at the DerivationAgent side if the
	// derivation is still outdated, although the calculation within HTTP requests
	// will still be executed for each concurrent HTTP request, the knowledge graph
	// will only be modified once, therefore, this avoid the concurrent HTTP request
	// issue as now the correct outputs will be presented in the knowledge graph and
	// the derivation DAGs (for more details, please refer to
	// https://github.com/cambridge-cares/TheWorldAvatar/issues/184)
	@Test
	@Timeout(value = 180, unit = TimeUnit.SECONDS)
	@Order(6)
	public void testMaxMinDiffAsync()
			throws IllegalAccessException, IllegalArgumentException, InvocationTargetException, InterruptedException {
		////////////////////////////////////////////////////////////
		// CASE 5 - MaxValue/MinValue/Difference Derivation Async //
		////////////////////////////////////////////////////////////

		// stage 1: initialise triples for derivation DAG
		JSONObject response = new JSONObject(AgentCaller.executeGet(host + InitialiseInstances.API_PATTERN_5));
		executeAndTestInitialiseInstances(response, true, false, false, false);

		// stage 2: call InputAgent to make all derivations outdated again
		executeAndTestInputAgent();

		// stage 3: call UpdateDerivations
		executeAndTestUpdateDerivations(response);
	}

	@Test
	@Timeout(value = 180, unit = TimeUnit.SECONDS)
	@Order(7)
	public void testMultipleAsyncDerivations() throws InterruptedException {
		// create five DifferenceReverseDerivation
		String maxvalue_instance = sparqlClient.getMaxValueIRI();
        String minvalue_instance = sparqlClient.getMinValueIRI();
		String difference_instance = sparqlClient.getDifferenceIRI();
		String diff_dev_1 = devClient.createAsyncDerivationForNewInfo(Config.agentIriDiffReverse, Arrays.asList(maxvalue_instance, minvalue_instance));
        String diff_dev_2 = devClient.createAsyncDerivationForNewInfo(Config.agentIriDiffReverse, Arrays.asList(maxvalue_instance, minvalue_instance));
		String diff_dev_3 = devClient.createAsyncDerivationForNewInfo(Config.agentIriDiffReverse, Arrays.asList(maxvalue_instance, minvalue_instance));
        String diff_dev_4 = devClient.createAsyncDerivationForNewInfo(Config.agentIriDiffReverse, Arrays.asList(maxvalue_instance, minvalue_instance));
        String diff_dev_5 = devClient.createAsyncDerivationForNewInfo(Config.agentIriDiffReverse, Arrays.asList(maxvalue_instance, minvalue_instance));
        // wait for all derivations to be completed
		TimeUnit.SECONDS.sleep(6 * Config.periodAgentDiffReverse);
		Map<String, StatusType> diffReverseDerivations = devClient.getDerivationsAndStatusType(Config.agentIriDiffReverse);
        Assert.assertEquals(5, diffReverseDerivations.size());
		// NOTE below check is likely to fail if the agent is not thread-safe
		// as the same derivation is likely to be processed by multiple threads
		// therefore, produce more DiffReverse instances than the amount of derivations created
		// (as observed in several testing runs before the changes made to the uuidLock)
        Assert.assertEquals(5, countNumberOfDerivationsGivenStatusType(diffReverseDerivations, StatusType.NOSTATUS));
		// also all values should be the same and they are the difference of min and max
		Map<String, Integer> diffReverseValues = sparqlClient.getDiffReverseValues();
        Assert.assertEquals(5, diffReverseValues.size());
        int difference = sparqlClient.getValue(difference_instance);
        diffReverseValues.values().stream().forEach(val -> Assert.assertEquals(0, val + difference));
	}

	@Test
	@Timeout(value = 180, unit = TimeUnit.SECONDS)
	@Order(8)
	public void testErrorStatus() throws InterruptedException {
		// initialise all triples for exception throw test
		JSONObject response = new JSONObject(AgentCaller.executeGet(host + InitialiseInstances.API_PATTERN_EXC_THROW));
		String inputPlaceholderExceptionThrowIri = response.getString(InitialiseInstances.input_placeholder_exc_throw_key);
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

	////////////////////////////////////////////////////////////
	// Below are utility functions to reduce code-duplication //
	////////////////////////////////////////////////////////////

	void assertAllInfoUpdateToDate(JSONObject response, boolean beforeInvokeInputAgent)
			throws IllegalAccessException, IllegalArgumentException, InvocationTargetException {
		// get pure inputs instance IRIs
		String upperlimit_instance = sparqlClient.getUpperLimitIRI();
		String lowerlimit_instance = sparqlClient.getLowerLimitIRI();
		String number_of_points_instance = sparqlClient.getNumberOfPointsIRI();

		// test pure input UpperLimit and LowerLimit still remain the same instance IRI
		Assert.assertEquals(response.getString(InitialiseInstances.upper_limit_instance_key),
				upperlimit_instance);
		Assert.assertEquals(response.getString(InitialiseInstances.lower_limit_instance_key),
				lowerlimit_instance);

		// test if the value for upperlimit and lowerlimit are still initialised value
		Assert.assertEquals(InitialiseInstances.upper_limit_value, sparqlClient.getValue(upperlimit_instance));
		Assert.assertEquals(InitialiseInstances.lower_limit_value, sparqlClient.getValue(lowerlimit_instance));

		// test IRI and value of NumberOfPoints if it's before invoking the InputAgent
		if (beforeInvokeInputAgent) {
			Assert.assertEquals(response.getString(InitialiseInstances.num_of_pts_instance_key),
					number_of_points_instance);
			Assert.assertEquals(InitialiseInstances.number_of_points, sparqlClient.getValue(number_of_points_instance));
		}

		// get all timestamp
		long timestamp_upper_limit_instance = (long) getTimestamp.invoke(devSparql, upperlimit_instance);
		long timestamp_lower_limit_instance = (long) getTimestamp.invoke(devSparql, lowerlimit_instance);
		long timestamp_num_of_point_instance = (long) getTimestamp.invoke(devSparql, number_of_points_instance);
		long timestamp_difference_derivation = (long) getTimestamp.invoke(devSparql,
				response.getString(InitialiseInstances.diff_dev_key));
		long timestamp_maxvalue_derivation = (long) getTimestamp.invoke(devSparql,
				response.getString(InitialiseInstances.max_dev_key));
		long timestamp_minvalue_derivation = (long) getTimestamp.invoke(devSparql,
				response.getString(InitialiseInstances.min_dev_key));
		long timestamp_rng_derivation = (long) getTimestamp.invoke(devSparql,
				response.getString(InitialiseInstances.rng_dev_key));

		// test if all the timestamp of derivations are up-to-date
		Assert.assertTrue(timestamp_upper_limit_instance > 0);
		Assert.assertTrue(timestamp_lower_limit_instance > 0);
		Assert.assertTrue(timestamp_num_of_point_instance > 0);
		Assert.assertTrue(timestamp_rng_derivation >= timestamp_upper_limit_instance);
		Assert.assertTrue(timestamp_rng_derivation >= timestamp_lower_limit_instance);
		Assert.assertTrue(timestamp_rng_derivation >= timestamp_num_of_point_instance);
		Assert.assertTrue(timestamp_maxvalue_derivation >= timestamp_rng_derivation);
		Assert.assertTrue(timestamp_minvalue_derivation >= timestamp_rng_derivation);
		Assert.assertTrue(timestamp_difference_derivation >= timestamp_maxvalue_derivation);
		Assert.assertTrue(timestamp_difference_derivation >= timestamp_minvalue_derivation);

		// test if all values in the KG are correct
		// test if it contains correct number of points in the derivation DAG
		Assert.assertEquals(sparqlClient.getValue(sparqlClient.getNumberOfPointsIRI()),
				sparqlClient.getAmountOfPointsInList());
		// test if no duplicate information written to KG in the situation of concurrent
		// HTTP request
		Assert.assertEquals(sparqlClient.getValue(sparqlClient.getNumberOfPointsIRI()),
				sparqlClient.getAmountOfPointsInKG());
		// test if the value is the same as the max value
		Assert.assertEquals(sparqlClient.getExtremeValueInList(sparqlClient.getListOfRandomPointsIRI(), true),
				sparqlClient.getValue(sparqlClient.getMaxValueIRI()));
		// test if the value is the same as the min value
		Assert.assertEquals(sparqlClient.getExtremeValueInList(sparqlClient.getListOfRandomPointsIRI(), false),
				sparqlClient.getValue(sparqlClient.getMinValueIRI()));
		// test if the value is the same as the difference value
		int difference = sparqlClient.getValue(sparqlClient.getMaxValueIRI())
				- sparqlClient.getValue(sparqlClient.getMinValueIRI());
		Assert.assertEquals(difference, sparqlClient.getValue(sparqlClient.getDifferenceIRI()));
	}

	void executeAndTestInitialiseInstances(JSONObject response, boolean rng, boolean max, boolean min, boolean diff)
			throws IllegalAccessException, IllegalArgumentException, InvocationTargetException, InterruptedException {
		// for each created instance, test that only one instance should be created if
		// it is sync derivation, otherwise, no instance should be created
		if (rng) {
			String listofrandompoints_instance = response.getString(InitialiseInstances.list_rand_pts_instance_key);
			Assert.assertEquals(listofrandompoints_instance, sparqlClient.getListOfRandomPointsIRI());
		} else {
			Assert.assertTrue(!response.has(InitialiseInstances.list_rand_pts_instance_key));
		}

		if (max) {
			String maxvalue_instance = response.getString(InitialiseInstances.maxvalue_instance_key);
			Assert.assertEquals(maxvalue_instance, sparqlClient.getMaxValueIRI());
		} else {
			Assert.assertTrue(!response.has(InitialiseInstances.maxvalue_instance_key));
		}

		if (min) {
			String minvalue_instance = response.getString(InitialiseInstances.minvalue_instance_key);
			Assert.assertEquals(minvalue_instance, sparqlClient.getMinValueIRI());
		} else {
			Assert.assertTrue(!response.has(InitialiseInstances.minvalue_instance_key));
		}

		if (diff) {
			String difference_instance = response.getString(InitialiseInstances.difference_instance_key);
			Assert.assertEquals(difference_instance, sparqlClient.getDifferenceIRI());
		} else {
			Assert.assertTrue(!response.has(InitialiseInstances.difference_instance_key));
		}

		// get IRI of difference derivation
		String difference_derivation = response.getString(InitialiseInstances.diff_dev_key);

		// once timestamp of difference derivation updated, all information in the KG
		// should also be up-to-date
		long currentTimestamp_difference_derivation = (long) getTimestamp.invoke(devSparql, difference_derivation);
		while (currentTimestamp_difference_derivation == 0) {
			TimeUnit.SECONDS.sleep(8);
			currentTimestamp_difference_derivation = (long) getTimestamp.invoke(devSparql, difference_derivation);
		}

		// wait arbitrary amount of time so that the cleaning up is finished
		TimeUnit.SECONDS.sleep(3);

		// now all information should be up-to-date
		assertAllInfoUpdateToDate(response, true);
	}

	void executeAndTestInputAgent() throws IllegalAccessException, IllegalArgumentException, InvocationTargetException {
		// save information about the old NumberOfPoints instance
		String numOfPoint_old = sparqlClient.getNumberOfPointsIRI();
		int numOfPoints_val_old = sparqlClient.getValue(numOfPoint_old);
		long numOfPoints_timestamp_old = (long) getTimestamp.invoke(devSparql, numOfPoint_old);

		// invoke InputAgent via HTTP request
		AgentCaller.executeGet(host + InputAgent.API_PATTERN);

		// get information about new NumberOfPoints instance
		String numOfPoint_new = sparqlClient.getNumberOfPointsIRI();
		int numOfPoints_val_new = sparqlClient.getValue(numOfPoint_new);
		long numOfPoints_timestamp_new = (long) getTimestamp.invoke(devSparql, numOfPoint_new);

		// test if InputAgent increased the value
		Assert.assertEquals(numOfPoints_val_old + 1, numOfPoints_val_new);
		// test if InputAgent modified the timestamp
		Assert.assertTrue(numOfPoints_timestamp_new > numOfPoints_timestamp_old);
	}

	void executeAndTestUpdateDerivations(JSONObject response)
			throws IllegalAccessException, IllegalArgumentException, InvocationTargetException, InterruptedException {
		// get IRI of difference derivation
		String difference_derivation = response.getString(InitialiseInstances.diff_dev_key);
		// get old timestamp of difference derivation
		long difference_derivation_timestamp = (long) getTimestamp.invoke(devSparql, difference_derivation);
		// get information about old instance of difference
		String difference_instance_old = sparqlClient.getDifferenceIRI();

		// invoke UpdateDerivations via HTTP request
		AgentCaller.executeGet(host + UpdateDerivations.API_PATTERN);

		// once timestamp of difference derivation updated, the iri of difference should
		// be different from the previous one, all information in the KG should also be
		// up-to-date
		long currentTimestamp_difference_derivation = (long) getTimestamp.invoke(devSparql, difference_derivation);
		while (currentTimestamp_difference_derivation <= difference_derivation_timestamp) {
			TimeUnit.SECONDS.sleep(20);
			currentTimestamp_difference_derivation = (long) getTimestamp.invoke(devSparql, difference_derivation);
		}

		// wait arbitrary amount of time so that the cleaning up is finished
		TimeUnit.SECONDS.sleep(3);
		String difference_instance_new = sparqlClient.getDifferenceIRI();
		Assert.assertNotEquals(difference_instance_old, difference_instance_new);

		// now all information in the KG should be up-to-date
		assertAllInfoUpdateToDate(response, false);
	}

	public int countNumberOfDerivationsGivenStatusType(Map<String, StatusType> derivationsAndStatusType, StatusType statusType) {
        return (int) derivationsAndStatusType.values().stream().filter(status -> status.equals(statusType)).count();
    }
}
