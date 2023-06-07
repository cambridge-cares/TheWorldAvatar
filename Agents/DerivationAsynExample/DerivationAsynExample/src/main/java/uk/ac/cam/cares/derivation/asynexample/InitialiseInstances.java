package uk.ac.cam.cares.derivation.asynexample;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONArray;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.derivation.Derivation;
import uk.ac.cam.cares.jps.base.derivation.DerivationClient;
import uk.ac.cam.cares.jps.base.derivation.DerivationSparql;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

/**
 * This InitialiseInstances agent initialises the knowledge graph and creates the chain of derivations.
 * @author Jiaru Bai (jb2197@cam.ac.uk)
 *
 */
@WebServlet(urlPatterns = { InitialiseInstances.API_PATTERN_1, InitialiseInstances.API_PATTERN_2,
		InitialiseInstances.API_PATTERN_3, InitialiseInstances.API_PATTERN_4, InitialiseInstances.API_PATTERN_5,
		InitialiseInstances.API_PATTERN_6, InitialiseInstances.API_PATTERN_EXC_THROW })
public class InitialiseInstances extends JPSAgent {

	private static final long serialVersionUID = 1L;

	private static final Logger LOGGER = LogManager.getLogger(InitialiseInstances.class);

	static final String API_PATTERN_1 = "/InitialiseInstances_1";
	static final String API_PATTERN_2 = "/InitialiseInstances_2";
	static final String API_PATTERN_3 = "/InitialiseInstances_3";
	static final String API_PATTERN_4 = "/InitialiseInstances_4";
	static final String API_PATTERN_5 = "/InitialiseInstances_5";
	static final String API_PATTERN_6 = "/InitialiseInstances_6";
	static final String API_PATTERN_EXC_THROW = "/InitialiseInstances_ExceptionThrow";

	public static final int upper_limit_value = 20;
	public static final int lower_limit_value = 3;
	public static final int number_of_points = 0;

	public static final String upper_limit_instance_key = "UpperLimit instance";
	public static final String lower_limit_instance_key = "LowerLimit instance";
	public static final String num_of_pts_instance_key = "NumberOfPoints instance";
	public static final String pt_instances_key = "Point instances";
	public static final String maxvalue_instance_key = "MaxValue instance";
	public static final String minvalue_instance_key = "MinValue instance";
	public static final String difference_instance_key = "Difference instance";
	public static final String rng_dev_key = "RandomNumberGeneration Derivation";
	public static final String max_dev_key = "MaxValue Derivation";
	public static final String min_dev_key = "MinValue Derivation";
	public static final String diff_dev_key = "Difference Derivation";
	public static final String input_placeholder_exc_throw_key = "InputPlaceholderExceptionThrow instance";
	public static final String output_placeholder_exc_throw_key = "OutputPlaceholderExceptionThrow instance";

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
			case API_PATTERN_EXC_THROW:
				response = initialiseExceptionThrow(sparqlClient, devClient);
				break;
			default:
				response.put(PATTERN_NOT_SUPPORTED_KEY, "Servlet pattern NOT supported.");
		}

		// check all connections between all derivations
		// the method validateDerivations() validates all derivations in the KG
		if (!path.contentEquals(API_PATTERN_EXC_THROW)) {
			// only log if it's not API_PATTERN_EXC_THROW
			LOGGER.info("Validating derivations: " + response.getString(rng_dev_key) + ", " + response.getString(max_dev_key)
					+ ", " + response.getString(min_dev_key) + ", and " + response.getString(diff_dev_key));
		}

		try {
			devClient.validateDerivations();
			LOGGER.info("Validated chain of derivations successfully");
		} catch (Exception e) {
			LOGGER.error("Validation failure for chain of derivations: " +
					e.getMessage());
			throw new JPSRuntimeException(e);
		}

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

	JSONObject initialiseExceptionThrow(SparqlClient sparqlClient, DerivationClient devClient) {
		JSONObject response = new JSONObject();

		// clear KG when initialising
		LOGGER.info("Initialising new instances, all existing instances will get deleted");
		sparqlClient.clearKG();

		// get the IRIs
		String inputPlaceholderRdfType = SparqlClient.getRdfTypeString(SparqlClient.InputPlaceholderExceptionThrow);
		String outputPlaceholderRdfType = SparqlClient.getRdfTypeString(SparqlClient.OutputPlaceholderExceptionThrow);

		// create ontoagent instances
		devClient.createOntoAgentInstance(Config.agentIriExceptionThrow, Config.agentHttpUrlExceptionThrow,
				Arrays.asList(inputPlaceholderRdfType), Arrays.asList(outputPlaceholderRdfType));

		// create upperlimit, lowerlimit, numberofpoints
		String inputPlaceholder = sparqlClient.createInputPlaceholderExceptionThrow();
		LOGGER.info("Created InputPlaceholderExceptionThrow instance <" + inputPlaceholder + ">");
		response.put(input_placeholder_exc_throw_key, inputPlaceholder);

		return response;
	}

	JSONObject basicInitialisation(SparqlClient sparqlClient, DerivationClient devClient, boolean listPt, boolean max,
			boolean min, boolean diff) {
		JSONObject response = new JSONObject();

		// clear KG when initialising
		LOGGER.info("Initialising new instances, all existing instances will get deleted");
		sparqlClient.clearKG();

		// get the IRIs
		String ulRdfType = SparqlClient.getRdfTypeString(SparqlClient.UpperLimit);
		String llRdfType = SparqlClient.getRdfTypeString(SparqlClient.LowerLimit);
		String npRdfType = SparqlClient.getRdfTypeString(SparqlClient.NumberOfPoints);
		String ptRdfType = SparqlClient.getRdfTypeString(SparqlClient.Point);
		String maxvRdfType = SparqlClient.getRdfTypeString(SparqlClient.MaxValue);
		String minvRdfType = SparqlClient.getRdfTypeString(SparqlClient.MinValue);
		String diffRdfType = SparqlClient.getRdfTypeString(SparqlClient.Difference);
		String diffReverseRdfType = SparqlClient.getRdfTypeString(SparqlClient.DifferenceReverse);

		// create ontoagent instances
		devClient.createOntoAgentInstance(Config.agentIriRNG, Config.agentHttpUrlRNG,
				Arrays.asList(ulRdfType, llRdfType, npRdfType), Arrays.asList(ptRdfType));
		devClient.createOntoAgentInstance(Config.agentIriMaxValue, Config.agentHttpUrlMaxValue,
				Arrays.asList(ptRdfType), Arrays.asList(maxvRdfType));
		devClient.createOntoAgentInstance(Config.agentIriMinValue, Config.agentHttpUrlMinValue,
				Arrays.asList(ptRdfType), Arrays.asList(minvRdfType));
		devClient.createOntoAgentInstance(Config.agentIriDifference, Config.agentHttpUrlDifference,
				Arrays.asList(maxvRdfType, minvRdfType), Arrays.asList(diffRdfType));
		devClient.createOntoAgentInstance(Config.agentIriDiffReverse, Config.agentHttpUrlDiffReverse,
				Arrays.asList(maxvRdfType, minvRdfType), Arrays.asList(diffReverseRdfType));

		// create upperlimit, lowerlimit, numberofpoints
		String upperLimit = sparqlClient.createUpperLimit();
		String ul_value = sparqlClient.addValueInstance(upperLimit, upper_limit_value);
		LOGGER.info("Created UpperLimit instance <" + upperLimit + ">");
		response.put(upper_limit_instance_key, upperLimit);

		String lowerLimit = sparqlClient.createLowerLimit();
		String ll_value = sparqlClient.addValueInstance(lowerLimit, lower_limit_value);
		LOGGER.info("Created LowerLimit instance <" + lowerLimit + ">");
		response.put(lower_limit_instance_key, lowerLimit);

		String numOfPoints = sparqlClient.createNumberOfPoints();
		String np_value = sparqlClient.addValueInstance(numOfPoints, number_of_points);
		LOGGER.info("Created NumberOfPoints instance <" + numOfPoints + ">");
		response.put(num_of_pts_instance_key, numOfPoints);

		List<String> pureInputs = Arrays.asList(upperLimit, lowerLimit, numOfPoints);
		List<String> maxDevInputs = new ArrayList<>();
		List<String> minDevInputs = new ArrayList<>();
		List<String> diffDevInputs = new ArrayList<>();
		if (listPt) {
			// create listofrandompoints via rng derivation
			Derivation rng_derivation = devClient.createSyncDerivationForNewInfo(Config.agentIriRNG, pureInputs,
					DerivationSparql.ONTODERIVATION_DERIVATION);
			String rng_dev = rng_derivation.getIri();
			LOGGER.info("Created RNG derivation <" + rng_dev + ">");
			response.put(rng_dev_key, rng_dev);

			List<String> ptIRIs = rng_derivation
					.getBelongsToIris(SparqlClient.getRdfTypeString(SparqlClient.Point));
			LOGGER.info("Created list of random Point instances: " + ptIRIs);
			response.put(pt_instances_key, new JSONArray(ptIRIs));

			maxDevInputs.addAll(ptIRIs);
			minDevInputs.addAll(ptIRIs);

			// create maxvalue, minvalue via maxvalue and minvalue derivation
			if (max) {
				Derivation max_derivation = devClient.createSyncDerivationForNewInfo(Config.agentIriMaxValue,
						maxDevInputs, DerivationSparql.ONTODERIVATION_DERIVATION);
				String max_dev = max_derivation.getIri();
				LOGGER.info("Created MaxValue derivation <" + max_dev + ">");
				response.put(max_dev_key, max_dev);

				List<String> maxValue_iris = max_derivation
						.getBelongsToIris(SparqlClient.getRdfTypeString(SparqlClient.MaxValue));
				if (maxValue_iris.size() != 1) {
					throw new IllegalStateException("Expected 1 MaxValue instance, got " + maxValue_iris.size());
				}
				String maxValue = maxValue_iris.get(0);
				LOGGER.info("Created MaxValue instance <" + maxValue + ">");
				response.put(maxvalue_instance_key, maxValue);

				diffDevInputs.add(maxValue);
			} else {
				String max_dev = devClient.createAsyncDerivationForNewInfo(Config.agentIriMaxValue,
						maxDevInputs);
				response.put(max_dev_key, max_dev);
				diffDevInputs.add(max_dev);
			}

			if (min) {
				Derivation min_derivation = devClient.createSyncDerivationForNewInfo(Config.agentIriMinValue,
						minDevInputs, DerivationSparql.ONTODERIVATION_DERIVATION);
				String min_dev = min_derivation.getIri();
				LOGGER.info("Created MinValue derivation <" + min_dev + ">");
				response.put(min_dev_key, min_dev);

				List<String> minValue_iris = min_derivation
						.getBelongsToIris(SparqlClient.getRdfTypeString(SparqlClient.MinValue));
				if (minValue_iris.size() != 1) {
					throw new IllegalStateException("Expected 1 MinValue instance, got " + minValue_iris.size());
				}
				String minValue = minValue_iris.get(0);
				LOGGER.info("Created MinValue instance <" + minValue + ">");
				response.put(minvalue_instance_key, minValue);

				diffDevInputs.add(minValue);
			} else {
				String min_dev = devClient.createAsyncDerivationForNewInfo(Config.agentIriMinValue,
						minDevInputs);
				response.put(min_dev_key, min_dev);
				diffDevInputs.add(min_dev);
			}

			if (diff) {
				// create difference via difference derivation
				Derivation diff_derivation = devClient.createSyncDerivationForNewInfo(Config.agentIriDifference,
						diffDevInputs, DerivationSparql.ONTODERIVATION_DERIVATION);
				String diff_dev = diff_derivation.getIri();
				LOGGER.info("Created Difference derivation <" + diff_dev + ">");
				response.put(diff_dev_key, diff_dev);

				List<String> difference_iris = diff_derivation
						.getBelongsToIris(SparqlClient.getRdfTypeString(SparqlClient.Difference));
				if (difference_iris.size() != 1) {
					throw new IllegalStateException("Expected 1 Difference instance, got "
							+ difference_iris.size());
				}
				String difference = difference_iris.get(0);
				LOGGER.info("Created Difference instance <" + difference + ">");
				response.put(difference_instance_key, difference);

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
