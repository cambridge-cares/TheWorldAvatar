package uk.ac.cam.cares.derivation.example;

import java.time.Instant;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Random;

import javax.servlet.annotation.WebServlet;

import org.json.JSONObject;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.derivation.DerivationClient;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;


@WebServlet(urlPatterns = {"/InitialiseInstances"})
public class InitialiseInstances extends JPSAgent{
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	// this URL is based on the docker image, make sure the URL is accessible from the derived quantity's calling entity
	private static String baseURL = "http://derivationexample:8080/DerivationExample";

	private static String minvalue_agent_iri = SparqlClient.namespace + "minvalue_agent";
	private static String minvalue_agent_url = baseURL + MinValueAgent.URL_MINVALUE;


	public static final String derivationInstanceBaseURL = "http://derivationexample.com/triplestore/repository/";

	public static final String input_key = "input";
	public static final String min_key = "min value";
	public static final String min_dev_key = "derivation of min value";
	private static final Logger LOGGER = LogManager.getLogger(InitialiseInstances.class);

	@Override
	public JSONObject processRequestParameters(JSONObject requestParams) {
		Config.initProperties();
		RemoteStoreClient storeClient = new RemoteStoreClient(Config.kgurl,Config.kgurl,Config.kguser,Config.kgpassword);
		SparqlClient sparqlClient = new SparqlClient(storeClient);
		DerivationClient devClient = new DerivationClient(storeClient, derivationInstanceBaseURL);

		LOGGER.info("Initialising new instances, all existing instances will get deleted");

		TimeSeriesClient<Instant> tsClient = new TimeSeriesClient<Instant>(storeClient, Instant.class, Config.dburl, Config.dbuser, Config.dbpassword);
		tsClient.deleteAll();
		sparqlClient.clearKG();

		// record the IRIs of the created instances to link them later
		String input = sparqlClient.createInputData();
		// attach timestamp to input
		devClient.addTimeInstance(input);
		// the timestamp added using addTimeInstance is 0, this will ensure that the input is current
		devClient.updateTimestamps(Arrays.asList(input));
		createInputTimeSeries(input, tsClient);
		LOGGER.info("Created input <" + input + ">");
		InstancesDatabase.Input = input;

		// register ontoagent instances in triple store
		String inputDataRdfType = SparqlClient.getRdfTypeString(SparqlClient.InputData);
		String minValueRdfType = SparqlClient.getRdfTypeString(SparqlClient.MinValue);
		String scalarValueRdfType = SparqlClient.getRdfTypeString(SparqlClient.ScalarValue);
		devClient.createOntoAgentInstance(minvalue_agent_iri, minvalue_agent_url,
			Arrays.asList(inputDataRdfType), Arrays.asList(minValueRdfType, scalarValueRdfType));

		String min_property = sparqlClient.createMinValue();
		String min_value = sparqlClient.addValueInstance(min_property, 0);
		LOGGER.info("Created min value <" + min_property + ">");


		// create 3 standard derived quantities
		String derived_minvalue = devClient.createDerivation(Arrays.asList(min_property,min_value), minvalue_agent_iri, Arrays.asList(input));
		LOGGER.info("Created derived quantity for min value <" + derived_minvalue + ">");




		JSONObject response = new JSONObject();
		response.put(input_key, input);
		response.put(min_key, min_property);
		response.put(min_dev_key, derived_minvalue);

		return response;
	}

	private static void createInputTimeSeries(String input_iri, TimeSeriesClient<Instant> tsClient) {
		tsClient.initTimeSeries(Arrays.asList(input_iri), Arrays.asList(Integer.class), null);

		// create a new time series object with random numbers
		Random rand = new Random();
		List<Instant> time_column = Arrays.asList(Instant.now());
		List<List<?>> values = new ArrayList<>();
		List<Integer> value_column = Arrays.asList(rand.nextInt());
		values.add(value_column);
		TimeSeries<Instant> ts = new TimeSeries<Instant>(time_column, Arrays.asList(input_iri), values);

		tsClient.addTimeSeriesData(ts);
	}
}
