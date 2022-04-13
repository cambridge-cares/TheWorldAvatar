package uk.ac.cam.cares.derivation.asynexample;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Random;

import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.derivation.DerivationClient;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

/**
 * This InitialiseInstances agent initialises the knowledge graph and creates the chain of derivations.
 * @author Jiaru Bai (jb2197@cam.ac.uk)
 *
 */
@WebServlet(urlPatterns = { InitialiseInstances.API_PATTERN_1, InitialiseInstances.API_PATTERN_2,
		InitialiseInstances.API_PATTERN_3, InitialiseInstances.API_PATTERN_4, InitialiseInstances.API_PATTERN_5,
		InitialiseInstances.API_PATTERN_6 })
public class InitialiseInstances extends JPSAgent {

	private static final long serialVersionUID = 1L;

	private static final Logger LOGGER = LogManager.getLogger(InitialiseInstances.class);

	static final String API_PATTERN_1 = "/InitialiseInstances_1";
	static final String API_PATTERN_2 = "/InitialiseInstances_2";
	static final String API_PATTERN_3 = "/InitialiseInstances_3";
	static final String API_PATTERN_4 = "/InitialiseInstances_4";
	static final String API_PATTERN_5 = "/InitialiseInstances_5";
	static final String API_PATTERN_6 = "/InitialiseInstances_6";

	private final int upper_limit_value = 20;
	private final int lower_limit_value = 3;
	private final int number_of_points = 6;

	private static final String upper_limit_instance_key = "UpperLimit instance";
	private static final String lower_limit_instance_key = "LowerLimit instance";
	private static final String num_of_pts_instance_key = "NumberOfPoints instance";
	private static final String list_rand_pts_instance_key = "ListOfRandomPoints instance";
	private static final String maxvalue_instance_key = "MaxValue instance";
	private static final String minvalue_instance_key = "MinValue instance";
	private static final String difference_instance_key = "Difference instance";
	private static final String rng_dev_key = "RandomNumberGeneration Derivation";
	private static final String max_dev_key = "MaxValue Derivation";
	private static final String min_dev_key = "MinValue Derivation";
	private static final String diff_dev_key = "Difference Derivation";

	private static final String PATTERN_NOT_SUPPORTED_KEY = "Pattern not supported";

	@Override
	public JSONObject processRequestParameters(JSONObject requestParams, HttpServletRequest request) {
		Config.initProperties();
		RemoteStoreClient storeClient = new RemoteStoreClient(Config.sparqlEndpoint, Config.sparqlEndpoint, Config.kgUser, Config.kgPassword);
		SparqlClient sparqlClient = new SparqlClient(storeClient);
		DerivationClient devClient = new DerivationClient(storeClient, Config.derivationInstanceBaseURL);
		
		JSONObject response = new JSONObject();
		String path = request.getServletPath();
		switch (path) {
			case API_PATTERN_1:
				response = initialise1(sparqlClient, devClient);
				break;
			case API_PATTERN_2:
				response = initialise2(sparqlClient, devClient);
				break;
			case API_PATTERN_3:
				response = initialise3(sparqlClient, devClient);
				break;
			case API_PATTERN_4:
				response = initialise4(sparqlClient, devClient);
				break;
			case API_PATTERN_5:
				response = initialise5(sparqlClient, devClient);
				break;
			case API_PATTERN_6:
				response = initialise6(sparqlClient, devClient);
				break;
			default:
				response.put(PATTERN_NOT_SUPPORTED_KEY, "Servlet pattern NOT supported.");
		}

		// check all connections between the derived quantities
		// as the validate method traverse down, checking difference derivation checks
		// all other derivations in the chain
		// TODO update validateDerivations for NEW_INFO mode
		// LOGGER.info("Validating derivations: " + rng_dev + ", " + max_dev + ", " +
		// min_dev + ", and " + diff_dev);
		// try {
		// devClient.validateDerivations();
		// LOGGER.info("Validated chain of derivations successfully");
		// } catch (Exception e) {
		// LOGGER.error("Validation failure for chain of derivations: " +
		// e.getMessage());
		// throw new JPSRuntimeException(e);
		// }

		// invoke all asynchronous agents so that they can be initialised
		AgentCaller.executeGet(Config.agentHttpUrlRNG);
		AgentCaller.executeGet(Config.agentHttpUrlMaxValue);
		AgentCaller.executeGet(Config.agentHttpUrlMinValue);
		AgentCaller.executeGet(Config.agentHttpUrlDifference);

		return response;
	}

	JSONObject initialise1(SparqlClient sparqlClient, DerivationClient devClient) {
		return basicInitialisation(sparqlClient, devClient, true, true, true, true);
	}

	JSONObject initialise2(SparqlClient sparqlClient, DerivationClient devClient) {
		return basicInitialisation(sparqlClient, devClient, true, true, true, false);
	}

	JSONObject initialise3(SparqlClient sparqlClient, DerivationClient devClient) {
		return basicInitialisation(sparqlClient, devClient, true, false, true, false);
	}

	JSONObject initialise4(SparqlClient sparqlClient, DerivationClient devClient) {
		return basicInitialisation(sparqlClient, devClient, true, true, false, false);
	}

	JSONObject initialise5(SparqlClient sparqlClient, DerivationClient devClient) {
		return basicInitialisation(sparqlClient, devClient, true, false, false, false);
	}

	JSONObject initialise6(SparqlClient sparqlClient, DerivationClient devClient) {
		return basicInitialisation(sparqlClient, devClient, false, false, false, false);
	}

	JSONObject basicInitialisation(SparqlClient sparqlClient, DerivationClient devClient, boolean listPt, boolean max,
			boolean min, boolean diff) {
		JSONObject response = new JSONObject();

		// clear KG when initialising
		LOGGER.info("Initialising new instances, all existing instances will get deleted");
		sparqlClient.clearKG();

		// get the IRIs
		String ul_rdf_type = SparqlClient.getRdfTypeString(SparqlClient.UpperLimit);
		String ll_rdf_type = SparqlClient.getRdfTypeString(SparqlClient.LowerLimit);
		String np_rdf_type = SparqlClient.getRdfTypeString(SparqlClient.NumberOfPoints);
		String lp_rdf_type = SparqlClient.getRdfTypeString(SparqlClient.ListOfRandomPoints);
		String maxv_rdf_type = SparqlClient.getRdfTypeString(SparqlClient.MaxValue);
		String minv_rdf_type = SparqlClient.getRdfTypeString(SparqlClient.MinValue);
		String diff_rdf_type = SparqlClient.getRdfTypeString(SparqlClient.Difference);

		// create ontoagent instances
		sparqlClient.createOntoAgentInstance(Config.agentIriRNG, Config.agentHttpUrlRNG,
				Arrays.asList(ul_rdf_type, ll_rdf_type, np_rdf_type), Arrays.asList(lp_rdf_type));
		sparqlClient.createOntoAgentInstance(Config.agentIriMaxValue, Config.agentHttpUrlMaxValue,
				Arrays.asList(lp_rdf_type), Arrays.asList(maxv_rdf_type));
		sparqlClient.createOntoAgentInstance(Config.agentIriMinValue, Config.agentHttpUrlMinValue,
				Arrays.asList(lp_rdf_type), Arrays.asList(minv_rdf_type));
		sparqlClient.createOntoAgentInstance(Config.agentIriDifference, Config.agentHttpUrlDifference,
				Arrays.asList(maxv_rdf_type, minv_rdf_type), Arrays.asList(diff_rdf_type));

		// create upperlimit, lowerlimit, numberofpoints
		String upperLimit = sparqlClient.createUpperLimit();
		String ul_value = sparqlClient.addValueInstance(upperLimit, upper_limit_value);
		devClient.addTimeInstance(upperLimit);
		devClient.updateTimestamp(upperLimit);
		LOGGER.info("Created UpperLimit instance <" + upperLimit + ">");
		response.put(upper_limit_instance_key, upperLimit);

		String lowerLimit = sparqlClient.createLowerLimit();
		String ll_value = sparqlClient.addValueInstance(lowerLimit, lower_limit_value);
		devClient.addTimeInstance(lowerLimit);
		devClient.updateTimestamp(lowerLimit);
		LOGGER.info("Created LowerLimit instance <" + lowerLimit + ">");
		response.put(lower_limit_instance_key, lowerLimit);

		String numOfPoints = sparqlClient.createNumberOfPoints();
		String np_value = sparqlClient.addValueInstance(numOfPoints, number_of_points);
		devClient.addTimeInstance(numOfPoints);
		devClient.updateTimestamp(numOfPoints);
		LOGGER.info("Created NumberOfPoints instance <" + numOfPoints + ">");
		response.put(num_of_pts_instance_key, numOfPoints);

		List<String> pureInputs = Arrays.asList(upperLimit, lowerLimit, numOfPoints);
		List<String> maxDevInputs = new ArrayList<>();
		List<String> minDevInputs = new ArrayList<>();
		List<String> diffDevInputs = new ArrayList<>();
		if (listPt) {
			// create listofrandompoints, points
			List<Integer> listOfRandomPointsValue = new ArrayList<Integer>();
			Random rand = new Random();
			for (int i = 0; i < number_of_points; i++) {
				listOfRandomPointsValue.add(rand.nextInt(upper_limit_value - lower_limit_value) + lower_limit_value);
			}
			String listOfRandomPoints = sparqlClient.createListOfRandomPoints(listOfRandomPointsValue);
			LOGGER.info("Created ListOfRandomPoints instance <" + listOfRandomPoints + ">");
			response.put(list_rand_pts_instance_key, listOfRandomPoints);
			maxDevInputs.add(listOfRandomPoints);
			minDevInputs.add(listOfRandomPoints);

			String rng_dev = devClient.createDerivation(Arrays.asList(listOfRandomPoints), Config.agentIriRNG,
					pureInputs);
			devClient.updateTimestamp(rng_dev);
			response.put(rng_dev_key, rng_dev);

			// create maxvalue, minvalue
			if (max) {
				String maxValue = sparqlClient.createMaxValue();
				String maxval = sparqlClient.addValueInstance(maxValue, Collections.max(listOfRandomPointsValue));
				LOGGER.info("Created MaxValue instance <" + maxValue + ">");
				response.put(maxvalue_instance_key, maxValue);
				diffDevInputs.add(maxValue);

				String max_dev = devClient.createDerivation(Arrays.asList(maxValue), Config.agentIriMaxValue,
						maxDevInputs);
				devClient.updateTimestamp(max_dev);
				response.put(max_dev_key, max_dev);
			} else {
				String max_dev = devClient.createAsyncDerivationForNewInfo(Config.agentIriMaxValue,
						maxDevInputs);
				response.put(max_dev_key, max_dev);
				diffDevInputs.add(max_dev);
			}

			if (min) {
				String minValue = sparqlClient.createMinValue();
				String minval = sparqlClient.addValueInstance(minValue, Collections.min(listOfRandomPointsValue));
				LOGGER.info("Created MinValue instance <" + minValue + ">");
				response.put(minvalue_instance_key, minValue);
				diffDevInputs.add(minValue);

				String min_dev = devClient.createDerivation(Arrays.asList(minValue), Config.agentIriMinValue,
						minDevInputs);
				devClient.updateTimestamp(min_dev);
				response.put(min_dev_key, min_dev);
			} else {
				String min_dev = devClient.createAsyncDerivationForNewInfo(Config.agentIriMinValue,
						minDevInputs);
				response.put(min_dev_key, min_dev);
				diffDevInputs.add(min_dev);
			}

			if (diff) {
				// create difference
				String difference = sparqlClient.createDifference();
				String diffval = sparqlClient.addValueInstance(difference,
						Collections.max(listOfRandomPointsValue) - Collections.min(listOfRandomPointsValue));
				LOGGER.info("Created Difference instance <" + difference + ">");
				response.put(difference_instance_key, difference);

				String diff_dev = devClient.createDerivation(Arrays.asList(difference), Config.agentIriDifference,
						Arrays.asList(response.getString(maxvalue_instance_key),
								response.getString(minvalue_instance_key)));
				devClient.updateTimestamp(diff_dev);
				response.put(diff_dev_key, diff_dev);
			} else {
				String diff_dev = devClient.createAsyncDerivationForNewInfo(Config.agentIriDifference, diffDevInputs);
				response.put(diff_dev_key, diff_dev);
			}
		} else {
			String rng_dev = devClient.createAsyncDerivationForNewInfo(Config.agentIriRNG, pureInputs);
			String max_dev = devClient.createAsyncDerivationForNewInfo(Config.agentIriMaxValue,
					Arrays.asList(rng_dev));
			String min_dev = devClient.createAsyncDerivationForNewInfo(Config.agentIriMinValue,
					Arrays.asList(rng_dev));
			String diff_dev = devClient.createAsyncDerivationForNewInfo(Config.agentIriDifference,
					Arrays.asList(max_dev, min_dev));

			response.put(rng_dev_key, rng_dev);
			response.put(max_dev_key, max_dev);
			response.put(min_dev_key, min_dev);
			response.put(diff_dev_key, diff_dev);
		}

		return response;
	}
}
