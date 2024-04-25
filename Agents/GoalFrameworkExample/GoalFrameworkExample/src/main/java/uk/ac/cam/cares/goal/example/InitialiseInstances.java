package uk.ac.cam.cares.goal.example;

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

//GoalFramework import statements
import uk.ac.cam.cares.goal.framework.*;

@WebServlet(urlPatterns = {"/InitialiseInstances"})
public class InitialiseInstances extends JPSAgent{

	private static final long serialVersionUID = 1L;

	private static final Logger LOGGER = LogManager.getLogger(InitialiseInstances.class);

	private static String baseURL = "http://goalframeworkexample:8080/GoalFrameworkExample";

	private static String binemptying_agent_iri = SparqlClient.namespace + "binemptying_agent";

	private static String binemptying_agent_url = baseURL + BinEmptyingAgent.URL_BINEMPTYINGAGENT;

	public static final String goalInstanceBaseURL = "http://goalframeworkexample.com/triplestore/repository/";

	public static final String goal_key = "goal";

	@Override
		public JSONObject processRequestParameters (JSONObject requestParams)
		{
			Config.initProperties();
			RemoteStoreClient storeClient = new RemoteStoreClient(Config.kgurl, Config.kgurl, Config.kguser, Config.kgpassword);
			SparqlClient sparqlClient = new SparqlClient(storeClient);
			GoalClient goalClient = new GoalClient(storeClient,goalInstanceBaseURL);
			DerivationClient devClient  = new DerivationClient(storeClient,goalInstanceBaseURL);
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
			String inputDataRdfType = SparqlClient.getRdfTypeString(SparqlClient.Weight);
			String truckRdfType = SparqlClient.getRdfTypeString(SparqlClient.Truck);
			String goalRangeType = SparqlClient.getRdfTypeString(SparqlClient.GoalRange);
			String desiredStateType = SparqlClient.getRdfTypeString(SparqlClient.Weight);

			//Create goalRange_iri
			String goalRange_iri = sparqlClient.createGoalRangeIRI();
			//Create goalCondition
			sparqlClient.createRangeCondition(goalRange_iri,"10000","0");

			//Create ontoagent instances for Derivations
			devClient.createOntoAgentInstance(binemptying_agent_iri, binemptying_agent_url, Arrays.asList(goalRangeType,input), Arrays.asList(desiredStateType));

			//create derivation
			//Create sumvalue_iri
			String desiredValue_property = sparqlClient.createDesiredValue();
			//Initialise sumvalue to be 0
			String desiredValue = sparqlClient.addValueInstance(desiredValue_property, 50);

			//create goal
			String goal_binemptyingagent = goalClient.createGoalForNewInfo(binemptying_agent_iri, binemptying_agent_url, goalRange_iri, input, desiredValue_property);


			String derived_desiredValue = devClient.createDerivation(Arrays.asList(desiredValue_property,desiredValue), binemptying_agent_iri, Arrays.asList(input,goalRange_iri));





			JSONObject response = new JSONObject();
			response.put(goal_key,goal_binemptyingagent);
			return response;
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