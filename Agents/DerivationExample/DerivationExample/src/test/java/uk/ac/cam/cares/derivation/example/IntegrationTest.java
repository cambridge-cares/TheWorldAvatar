package uk.ac.cam.cares.derivation.example;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.time.Instant;
import java.util.Arrays;

import org.eclipse.rdf4j.sparqlbuilder.constraint.SparqlAggregate;
import org.json.JSONArray;
import org.json.JSONObject;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import uk.ac.cam.cares.jps.base.derivation.DerivationClient;
import uk.ac.cam.cares.jps.base.derivation.DerivationOutputs;
import uk.ac.cam.cares.jps.base.derivation.DerivationSparql;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.interfaces.StoreClientInterface;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;

/**
 * These tests require the docker stack to be up and running
 * please refer to TheWorldAvatar/Agents/DerivationExample/README.md for more
 * details
 * 
 * @author Kok Foong Lee
 * @author Jiaru Bai
 *
 */
public class IntegrationTest {
	JSONObject initResponse;

	// note that the URLs in the properties file are the URLs when they are accessed from within the docker 
	String kgurl = "http://localhost:8889/blazegraph/namespace/kb/sparql";
	String rdburl = "jdbc:postgresql://localhost:7432/postgres";

	@Before
	public void initialise() {
		// the response is a JSON object containing the IRIs of the initialise instances, refer to InitialiseInstances for the keys
		initResponse = new JSONObject(AgentCaller.executeGet("http://localhost:8081/DerivationExample/InitialiseInstances"));
	}

	@Test
	public void testInputAgent() {
		// obtain IRI of the input that is initialised
		String input = initResponse.getString(InitialiseInstances.input_key);
		Config.initProperties();
		RemoteStoreClient storeClient = new RemoteStoreClient(kgurl, kgurl, Config.kguser, Config.kgpassword);
		TimeSeriesClient<Instant> tsClient = new TimeSeriesClient<Instant>(storeClient, Instant.class, rdburl, Config.dbuser, Config.dbpassword);

		TimeSeries<Instant> ts1 = tsClient.getTimeSeries(Arrays.asList(input));

		// the input agent will add a row to the time series table
		AgentCaller.executeGet("http://localhost:8081/DerivationExample/InputAgent");

		TimeSeries<Instant> ts2 = tsClient.getTimeSeries(Arrays.asList(input));

		Assert.assertTrue(ts2.getTimes().size() > ts1.getTimes().size());
	}

	/**
	 * assumes that if the derivation time stamps are updated, it is functioning as intended
	 * derivations are initialised with timestamp = 0
	 * @throws SecurityException 
	 * @throws NoSuchMethodException 
	 * @throws InvocationTargetException 
	 * @throws IllegalArgumentException 
	 * @throws IllegalAccessException 
	 */
	@Test
	public void testUpdateDerivations() throws NoSuchMethodException, SecurityException, IllegalAccessException, IllegalArgumentException, InvocationTargetException {
		Config.initProperties();
		RemoteStoreClient storeClient = new RemoteStoreClient(kgurl, kgurl, Config.kguser, Config.kgpassword);

		// get IRIs of initialise instances, the keys are located in the servlet InitialiseInstances
		String derived_diff = initResponse.getString(InitialiseInstances.diff_dev_key);
		String derived_average = initResponse.getString(InitialiseInstances.avg_dev_key);

		DerivationSparql devClient = new DerivationSparql(storeClient, InitialiseInstances.derivationInstanceBaseURL);

		Method getTimestamp = devClient.getClass().getDeclaredMethod("getTimestamp", String.class);
		getTimestamp.setAccessible(true);

		long oldtimestamp_diff = (long) getTimestamp.invoke(devClient, derived_diff);
		long oldtimestamp_average = (long) getTimestamp.invoke(devClient, derived_average);

		AgentCaller.executeGet("http://localhost:8081/DerivationExample/UpdateDerivations");

		long newtimestamp_diff = (long) getTimestamp.invoke(devClient, derived_diff);
		long newtimestamp_average = (long) getTimestamp.invoke(devClient, derived_average);

		Assert.assertTrue(newtimestamp_diff > oldtimestamp_diff);
		Assert.assertTrue(newtimestamp_average > oldtimestamp_average);
	}

	@Test
	public void testMinValueAgent() {
		String input = initResponse.getString(InitialiseInstances.input_key);
		JSONObject request = new JSONObject()
				.put(DerivationClient.SYNC_NEW_INFO_FLAG, false) // this is added to indicate call for update
				.put(DerivationClient.AGENT_INPUT_KEY,
						new JSONObject().put(SparqlClient.getRdfTypeString(SparqlClient.InputData), input))
				.put(DerivationClient.BELONGSTO_KEY,
						new JSONObject().put(initResponse.getString(InitialiseInstances.min_key),
								SparqlClient.getRdfTypeString(SparqlClient.MinValue)))
				.put(DerivationClient.DERIVATION_KEY, initResponse.getString(InitialiseInstances.min_dev_key))
				.put(DerivationClient.DERIVATION_TYPE_KEY, DerivationSparql.ONTODERIVATION_DERIVATION)
				.put(DerivationClient.DOWNSTREAMDERIVATION_KEY, new JSONObject().put(
						initResponse.getString(InitialiseInstances.min_key),
						new JSONArray(Arrays.asList(initResponse.getString(InitialiseInstances.diff_dev_key)))));

		String response = AgentCaller.executeGetWithURLAndJSON("http://localhost:8081/DerivationExample/MinValueAgent", request.toString());
		JSONObject responseJson = new JSONObject(response);
		Assert.assertTrue(responseJson.has(DerivationOutputs.RETRIEVED_INPUTS_TIMESTAMP_KEY));
	}

	@Test
	public void testMaxValueAgent() {
		String input = initResponse.getString(InitialiseInstances.input_key);
		JSONObject request = new JSONObject()
				.put(DerivationClient.SYNC_NEW_INFO_FLAG, false) // this is added to indicate call for update
				.put(DerivationClient.AGENT_INPUT_KEY,
						new JSONObject().put(SparqlClient.getRdfTypeString(SparqlClient.InputData), input))
				.put(DerivationClient.BELONGSTO_KEY,
						new JSONObject().put(initResponse.getString(InitialiseInstances.max_key),
								SparqlClient.getRdfTypeString(SparqlClient.MaxValue)))
				.put(DerivationClient.DERIVATION_KEY, initResponse.getString(InitialiseInstances.max_dev_key))
				.put(DerivationClient.DERIVATION_TYPE_KEY, DerivationSparql.ONTODERIVATION_DERIVATION)
				.put(DerivationClient.DOWNSTREAMDERIVATION_KEY, new JSONObject().put(
						initResponse.getString(InitialiseInstances.max_key),
						new JSONArray(Arrays.asList(initResponse.getString(InitialiseInstances.diff_dev_key)))));

		String response = AgentCaller.executeGetWithURLAndJSON("http://localhost:8081/DerivationExample/MaxValueAgent", request.toString());
		JSONObject responseJson = new JSONObject(response);
		Assert.assertTrue(responseJson.getLong(DerivationOutputs.RETRIEVED_INPUTS_TIMESTAMP_KEY) > 0);
	}

	@Test
	public void testAverageAgent() {
		String input = initResponse.getString(InitialiseInstances.input_key);
		String average = initResponse.getString(InitialiseInstances.avg_key);

		Config.initProperties();
		RemoteStoreClient storeClient = new RemoteStoreClient(kgurl, kgurl, Config.kguser, Config.kgpassword);
		TimeSeriesClient<Instant> tsClient = new TimeSeriesClient<Instant>(storeClient, Instant.class, rdburl, Config.dbuser, Config.dbpassword);

		TimeSeries<Instant> ts1 = tsClient.getTimeSeries(Arrays.asList(average));

		// now call agent to update the table containing averages
		JSONObject request = new JSONObject()
				.put(DerivationClient.SYNC_NEW_INFO_FLAG, false) // this is added to indicate call for update
				.put(DerivationClient.AGENT_INPUT_KEY,
						new JSONObject().put(SparqlClient.getRdfTypeString(SparqlClient.InputData), input))
				.put(DerivationClient.BELONGSTO_KEY,
						new JSONObject().put(initResponse.getString(InitialiseInstances.avg_key),
								SparqlClient.getRdfTypeString(SparqlClient.Average)))
				.put(DerivationClient.DERIVATION_KEY, initResponse.getString(InitialiseInstances.avg_dev_key))
				.put(DerivationClient.DERIVATION_TYPE_KEY, DerivationSparql.ONTODERIVATION_DERIVATIONWITHTIMESERIES)
				.put(DerivationClient.DOWNSTREAMDERIVATION_KEY, new JSONObject());
		String response = AgentCaller.executeGetWithURLAndJSON("http://localhost:8081/DerivationExample/AverageAgent", request.toString());

		TimeSeries<Instant> ts2 = tsClient.getTimeSeries(Arrays.asList(average));

		Assert.assertTrue(ts2.getTimes().size() > ts1.getTimes().size());
		JSONObject responseJson = new JSONObject(response);
		Assert.assertTrue(responseJson.getLong(DerivationOutputs.RETRIEVED_INPUTS_TIMESTAMP_KEY) > 0);
	}

	@Test
	public void testDifferenceAgent() {
		String min = initResponse.getString(InitialiseInstances.min_key);
		String max = initResponse.getString(InitialiseInstances.max_key);

		JSONObject request = new JSONObject()
				.put(DerivationClient.SYNC_NEW_INFO_FLAG, false) // this is added to indicate call for update
				.put(DerivationClient.AGENT_INPUT_KEY, new JSONObject()
						.put(SparqlClient.getRdfTypeString(SparqlClient.MinValue), min)
						.put(SparqlClient.getRdfTypeString(SparqlClient.MaxValue), max))
				.put(DerivationClient.BELONGSTO_KEY,
						new JSONObject().put(initResponse.getString(InitialiseInstances.diff_key),
								SparqlClient.getRdfTypeString(SparqlClient.Difference)))
				.put(DerivationClient.DERIVATION_KEY, initResponse.getString(InitialiseInstances.diff_dev_key))
				.put(DerivationClient.DERIVATION_TYPE_KEY, DerivationSparql.ONTODERIVATION_DERIVATION)
				.put(DerivationClient.DOWNSTREAMDERIVATION_KEY, new JSONObject());
		String response = AgentCaller.executeGetWithURLAndJSON("http://localhost:8081/DerivationExample/DifferenceAgent", request.toString());
		JSONObject responseJson = new JSONObject(response);

		// NOTE following the updated design of SPARQL udpate with sub query, the
		// DifferenceAgent will NOT update the knowledge graph in this situation as all
		// three DifferenceDerivation/MaxValueDerivation/MinValueDerivation will be
		// initialised with timestamp 0, thus by just looking at the timestamp, the
		// DifferenceDerivation is "up-to-date", the SPARQL update will thus not be
		// executed, therefore nothing changes and the timestamp of DifferenceDerivation
		// remains 0
		Assert.assertEquals(0, responseJson.getLong(DerivationOutputs.RETRIEVED_INPUTS_TIMESTAMP_KEY));

		// now if we change the timestamp of the MaxValueDerivation then fire the
		// request again, it should be working as expected
		RemoteStoreClient storeClient = new RemoteStoreClient(kgurl, kgurl, Config.kguser, Config.kgpassword);
		DerivationClient devClient = new DerivationClient(storeClient, InitialiseInstances.derivationInstanceBaseURL);
		devClient.updateTimestamp(initResponse.getString(InitialiseInstances.max_dev_key));
		response = AgentCaller.executeGetWithURLAndJSON("http://localhost:8081/DerivationExample/DifferenceAgent",
				request.toString());
		responseJson = new JSONObject(response);
		Assert.assertTrue(responseJson.getLong(DerivationOutputs.RETRIEVED_INPUTS_TIMESTAMP_KEY) > 0);
	}
}
