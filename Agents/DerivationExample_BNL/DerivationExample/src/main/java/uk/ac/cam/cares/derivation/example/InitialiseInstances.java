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
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;


@WebServlet(urlPatterns = {"/InitialiseInstances"})
public class InitialiseInstances extends JPSAgent{

	private static final long serialVersionUID = 1L;

	private static final Logger LOGGER = LogManager.getLogger(InitialiseInstances.class);

	private static String baseURL = "http://derivationexamplebnl:8080/DerivationExample";

	private static String sumvalue_agent_iri = SparqlClient.namespace + "sumvalue_agent";
	private static String sumvalue_agent_url = baseURL + SumValueAgent.URL_SUMVALUE;

	public static final String derivationInstanceBaseURL = "http://derivationexample.com/triplestore/repository/";

	public static final String input_key = "input";
	public static final String sum_key = "sum value";
	public static final String sum_dev_key = "derivation of sum value";


	@Override
		public JSONObject processRequestParameters (JSONObject requestParams)
		{


			Config.initProperties();
			RemoteStoreClient storeClient = new RemoteStoreClient(Config.kgurl, Config.kgurl, Config.kguser, Config.kgpassword);
			SparqlClient sparqlClient = new SparqlClient(storeClient);
			DerivationClient devClient = new DerivationClient(storeClient,derivationInstanceBaseURL);


			LOGGER.info("Initialising new instances");

			TimeSeriesClient<Instant> timeSeriesClient = new TimeSeriesClient<Instant>(storeClient, Instant.class, Config.dburl, Config.dbuser, Config.dbpassword);
			timeSeriesClient.deleteAll();
			sparqlClient.clearKG();

			LOGGER.info("Instances deleted");

			// record the IRIs of the created instances to link them later
			String input = sparqlClient.createInputData();


			// attach timestamp to input
			devClient.addTimeInstance(input);
			// the timestamp added using addTimeInstance is 0, this will ensure that the input is current
			devClient.updateTimestamps(Arrays.asList(input));
			createInputTimeSeries(input, timeSeriesClient);
			LOGGER.info("Created input <" + input + ">");
			InstancesDatabase.Input = input;



			// register ontoagent instances in triple store
			String inputDataRdfType = SparqlClient.getRdfTypeString(SparqlClient.InputData);
			String sumValueRdfType = SparqlClient.getRdfTypeString(SparqlClient.SumValue);
			String scalarValueRdfType = SparqlClient.getRdfTypeString(SparqlClient.ScalarValue);


			//Create ontoagent instances
			devClient.createOntoAgentInstance(sumvalue_agent_iri, sumvalue_agent_url,
					Arrays.asList(inputDataRdfType), Arrays.asList(sumValueRdfType, scalarValueRdfType));



			//Create sumvalue_iri
			String sumvalue_property = sparqlClient.createSumValue();
			//Initialise sumvalue to be 0
			String sum_value = sparqlClient.addValueInstance(sumvalue_property, 0);
			LOGGER.info("Created sum value <" + sumvalue_property + ">");



			// create standard derived quantities
			String derived_sumvalue = devClient.createDerivation(Arrays.asList(sumvalue_property,sum_value), sumvalue_agent_iri, Arrays.asList(input));
			LOGGER.info("Created derived quantity for min value <" + derived_sumvalue + ">");


			JSONObject response = new JSONObject();
			response.put(input_key, input);
			response.put(sum_key, sumvalue_property);
			response.put(sum_dev_key, derived_sumvalue);
			return requestParams;
        }

		private static void createInputTimeSeries(String input_iri, TimeSeriesClient<Instant> tsClient) {
			tsClient.initTimeSeries(Arrays.asList(input_iri), Arrays.asList(Integer.class), null);

			// create a new time series object with random numbers
			Random rand = new Random();
			List<Instant> time_column = Arrays.asList(Instant.now());
			List<List<?>> values = new ArrayList<>();
			//Generate a random number between 1-1000
			List<Integer> value_column = Arrays.asList(rand.nextInt(1000) + 1);
			values.add(value_column);
			TimeSeries<Instant> ts = new TimeSeries<Instant>(time_column, Arrays.asList(input_iri), values);

			tsClient.addTimeSeriesData(ts);
		}

	
}