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

	public static String baseURL = "http://goalframeworkexample:8080/GoalFrameworkExample";

	private static String binemptying_agent_iri = SparqlClient.namespace + "binemptying_agent";

	private static String binemptying_agent_url = baseURL + BinEmptyingAgent.URL_BINEMPTYINGAGENT;

	private static String truckemptying_agent_iri = SparqlClient.namespace + "truckemptying_agent";

	private static String truckemptying_agent_url = baseURL + TruckEmptyingAgent.URL_TRUCKEMPTYINGAGENT;

	public static final String goalInstanceBaseURL = "http://goalframeworkexample.com/triplestore/repository/";

	public static final String bin_goal_key = "bin_goal";
	public static final String truck_goal_key = "truck_goal";

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


			/**
			 * Create Goal and Derivation for Bins
			 */

			// record the IRIs of the created instances to link them later
			String input_iri = sparqlClient.createInputData(sparqlClient.BinInput);

			// attach timestamp to input_iri
			devClient.addTimeInstance(input_iri);

			// the timestamp added using addTimeInstance is 0, this will ensure that the input_iri is current
			devClient.updateTimestamps(Arrays.asList(input_iri));
			createInputTimeSeries(input_iri, timeSeriesClient);
			LOGGER.info("Created input_iri <" + input_iri + ">");
			InstancesDatabase.Input = input_iri;

			// register ontoagent instances in triple store
			String weightRdfType = SparqlClient.getRdfTypeString(SparqlClient.Weight);
			String binInputRdfType = SparqlClient.getRdfTypeString(SparqlClient.BinInput);
			String truckInputRdfType = SparqlClient.getRdfTypeString(SparqlClient.TruckInput);
			String goalRangeType = SparqlClient.getRdfTypeString(SparqlClient.GoalRange);
			String desiredStateType = SparqlClient.getRdfTypeString(SparqlClient.Weight);

			//Create goalRange_iri
			String goalRange_iri = sparqlClient.createGoalRangeIRI();
			//Create goalCondition
			sparqlClient.createRangeCondition(goalRange_iri,"100","0");

			//Create ontoagent instances for Derivations
			devClient.createOntoAgentInstance(binemptying_agent_iri, binemptying_agent_url, Arrays.asList(goalRangeType, binInputRdfType,weightRdfType), Arrays.asList(desiredStateType));

			//create derivation
			//Create sumvalue_iri
			String desiredValue_property = sparqlClient.createDesiredValue();
			//Initialise sumvalue to be 0
			String desiredValue = sparqlClient.addValueInstance(desiredValue_property, 50);

			//create goal
			String goal_binemptyingagent = goalClient.createGoalForNewInfo(binemptying_agent_iri, binemptying_agent_url, goalRange_iri, input_iri, desiredValue_property);

			//create derivation
			String derived_desiredValue = devClient.createDerivation(Arrays.asList(desiredValue_property,desiredValue), binemptying_agent_iri, Arrays.asList(input_iri,goalRange_iri));


			/**
			 * Create goal and derivation for truck
			 */

			// record the IRIs of the created instances to link them later
			String inputTruck_iri = sparqlClient.createInputData(sparqlClient.TruckInput);

			// attach timestamp to inputTruck_iri
			devClient.addTimeInstance(inputTruck_iri);

			// the timestamp added using addTimeInstance is 0, this will ensure that the inputTruck_iri is current
			devClient.updateTimestamps(Arrays.asList(inputTruck_iri));
			createInputTimeSeries(inputTruck_iri, timeSeriesClient);
			LOGGER.info("Created inputTruck_iri <" + inputTruck_iri + ">");
			InstancesDatabase.InputTruck = inputTruck_iri;

			//Create goalRange_iri
			String truck_goalRange_iri = sparqlClient.createGoalRangeIRI();
			//Create goalCondition
			sparqlClient.createRangeCondition(truck_goalRange_iri,"500","0");

			//Create ontoagent instances for Derivations
			devClient.createOntoAgentInstance(truckemptying_agent_iri, truckemptying_agent_url, Arrays.asList(goalRangeType,truckInputRdfType,weightRdfType), Arrays.asList(desiredStateType));

			//create derivation
			//Create sumvalue_iri
			String truck_desiredValue_property = sparqlClient.createDesiredValue();
			//Initialise sumvalue to be 0
			String truck_desiredValue = sparqlClient.addValueInstance(truck_desiredValue_property, 20);

			//create goal
			String goal_truckemptyingagent = goalClient.createGoalForNewInfo(truckemptying_agent_iri, truckemptying_agent_url, truck_goalRange_iri, inputTruck_iri, truck_desiredValue_property);

			//create derivation
			String derived_truck_desiredValue = devClient.createDerivation(Arrays.asList(truck_desiredValue_property,truck_desiredValue), truckemptying_agent_iri, Arrays.asList(inputTruck_iri,truck_goalRange_iri));


			JSONObject response = new JSONObject();
			response.put(bin_goal_key,goal_binemptyingagent);
			response.put(truck_goal_key,goal_truckemptyingagent);
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