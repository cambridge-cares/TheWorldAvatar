package uk.ac.cam.cares.jps.base.agent;

import java.time.Instant;
import java.io.IOException;
import java.util.List;
import java.util.HashMap;
import java.util.Map;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.ws.rs.BadRequestException;

import org.apache.http.client.methods.HttpGet;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.derivation.DerivationClient;
import uk.ac.cam.cares.jps.base.derivation.DerivationInputs;
import uk.ac.cam.cares.jps.base.derivation.DerivationOutputs;
import uk.ac.cam.cares.jps.base.derivation.DerivationSparql;
import uk.ac.cam.cares.jps.base.derivation.StatusType;
import uk.ac.cam.cares.jps.base.interfaces.DerivationAgentInterface;
import uk.ac.cam.cares.jps.base.interfaces.StoreClientInterface;

/**
 * This class is a template to be extended when developing agent with derivation
 * functionality.
 * 
 * @author Jiaru Bai (jb2197@cam.ac.uk)
 *
 */
public class DerivationAgent extends JPSAgent implements DerivationAgentInterface {
	/**
	 * Logger for stdout and stderr.
	 */
	private static final Logger LOGGER = LogManager.getLogger(DerivationAgent.class);

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	StoreClientInterface storeClient;
	DerivationClient devClient;
	public static final String EMPTY_REQUEST_MSG = "An empty request received by DerivationAgent.";

	/**
	 * Provide the default constructor to enable agent initialisation.
	 */
	public DerivationAgent() {
		LOGGER.info("A new DerivationAgent has been initialised.");
	}

	public DerivationAgent(StoreClientInterface storeClient, String derivationInstanceBaseURL) {
		this.storeClient = storeClient;
		this.devClient = new DerivationClient(storeClient, derivationInstanceBaseURL);
	}

	// @Override
	// protected void doGet(HttpServletRequest req, HttpServletResponse resp) throws
	// ServletException, IOException {
	// System.out.println(req.toString());
	// JSONObject requestJson = processHttpServletRequest(req);
	// if (validateInput(requestJson)) {
	// // serialises DerivationInputs objects from JSONObject
	// DerivationInputs inputs = new DerivationInputs(requestJson);
	// long timestamp = Instant.now().getEpochSecond();
	// // apply agent logic to convert inputs to outputs
	// DerivationOutputs outputs = processRequestParameters(inputs);
	// // set timestamp before the outputs were returned
	// outputs.setRetrievedInputsAt(timestamp);
	// // deserialises DerivationOutputs objects to JSONObject as HTTP response
	// resp.getWriter().write(outputs.toJson().toString());
	// }
	// }

	// private JSONObject processHttpServletRequest(HttpServletRequest req) {
	// if (req.getMethod().equals(HttpGet.METHOD_NAME)) {
	// return new JSONObject(req.getParameter(DerivationClient.AGENT_INPUT_KEY));
	// }
	// return new JSONObject();
	// }

	@Override
	public JSONObject processRequestParameters(JSONObject requestParams) {
		JSONObject res = new JSONObject();
		if (validateInput(requestParams)) {
			// serialises DerivationInputs objects from JSONObject
			DerivationInputs inputs = new DerivationInputs(
				requestParams.getJSONObject(DerivationClient.AGENT_INPUT_KEY));
			long timestamp = Instant.now().getEpochSecond();
			// apply agent logic to convert inputs to outputs
			DerivationOutputs outputs = processRequestParameters(inputs);
			// set timestamp before the outputs were returned
			outputs.setRetrievedInputsAt(timestamp);
			// deserialises DerivationOutputs objects to JSONObject as HTTP response
			res.put(DerivationClient.AGENT_OUTPUT_KEY, outputs.toJson());
			res.put(DerivationOutputs.RETRIEVED_INPUTS_TIMESTAMP_KEY, outputs.getRetrievedInputsAt());
		} else {
			res.put(DerivationClient.AGENT_OUTPUT_KEY, EMPTY_REQUEST_MSG);
		}
		return res;
	}

	@Override
	public DerivationOutputs processRequestParameters(DerivationInputs derivationInputs) {
		// TODO developer needs to overwrite this function
		return null;
	}

	@Override
	public boolean validateInput(JSONObject requestParams) throws BadRequestException {
		// TODO developer needs to overwrite this function for customised validation
		if (requestParams.isEmpty()) {
			LOGGER.warn("RequestParams are empty, throwing BadRequestException...");
			throw new BadRequestException();
		}
		if (!requestParams.has(DerivationClient.AGENT_INPUT_KEY)) {
			LOGGER.info(this.getClass().toString()+" agent received an empty request...");
			return false;
		}
		return true;
	}

	/**
	 * Monitor the derivation that isDerivedUsing the agentIRI.
	 * 
	 * @param agentIRI
	 */
	@Override
	public void monitorDerivation(String agentIRI) {
		// function getDerivationsAndStatusType ONLY consider the async derivation
		// sync derivations <isDerivedUsing> agentIRI will be handled as HTTP requests
		Map<String, StatusType> derivationsAndStatusType = devClient.getDerivationsAndStatusType(agentIRI);
		if (derivationsAndStatusType.isEmpty()) {
			LOGGER.info("Currently, no derivation <isDerivedUsing> <" + agentIRI + ">.");
		} else {
			LOGGER.info("A list of derivations that <isDerivedUsing> <" + agentIRI + "> are retrieved: "
					+ derivationsAndStatusType.toString() + ".");
		}

		// iterate over each derivation that the agent is monitoring and make decisions
		// based on its status
		derivationsAndStatusType.forEach((derivation, statusType) -> {
			LOGGER.info("Derivation <" + derivation + "> has status type: " + statusType + ".");
			switch (statusType) {
				case REQUESTED:
					Map<String, List<String>> immediateUpstreamDerivationToUpdate = devClient
							.checkImmediateUpstreamDerivation(derivation);
					if (immediateUpstreamDerivationToUpdate
							.containsKey(DerivationSparql.ONTODERIVATION_DERIVATIONASYN)) {
						// if any of upstream async derivations are still outdated, skip
						LOGGER.info("Derivation <" + derivation
								+ "> has a list of immediate upstream derivations to be updated: "
								+ immediateUpstreamDerivationToUpdate.toString());
					} else {
						// here implies all the immediate upstream async derivations are up-to-date
						// request update if any of upstream sync derivations are outdated
						List<String> syncDerivationsToUpdate = devClient
								.groupSyncDerivationsToUpdate(immediateUpstreamDerivationToUpdate);
						if (!syncDerivationsToUpdate.isEmpty()) {
							devClient.updatePureSyncDerivations(syncDerivationsToUpdate);
						}
						// check again to make sure that NO upstream async derivations need update
						// this is needed when updating all upstream sync derivation takes long time
						if (devClient.checkImmediateUpstreamDerivation(derivation).isEmpty()) {
							// assume the checking is really fast so all sync ones are up-to-date
							// retrieve the agent inputs that mapped to their rdf:type
							JSONObject agentInputs = devClient.retrieveAgentInputIRIs(derivation, agentIRI);
							LOGGER.info("Agent <" + agentIRI + "> retrieved inputs of derivation <" + derivation + ">: "
									+ agentInputs.toString() + ".");
							LOGGER.info("Derivation <" + derivation + "> is now in progress.");
							// serialise JSONObject retrieved from KG to instance of DerivationInputs
							DerivationInputs derivationInputs = new DerivationInputs(
									agentInputs.getJSONObject(DerivationClient.AGENT_INPUT_KEY));
							// perform the conversion from DerivationInputs to DerivationOutputs
							DerivationOutputs derivationOutputs = processRequestParameters(derivationInputs);
							// deserialise the derivationOutputs to a list of String of new derived IRI
							List<String> newDerivedIRI = derivationOutputs.getNewDerivedIRI();
							LOGGER.info("Derivation <" + derivation + "> has new generated derived IRI: "
									+ newDerivedIRI.toString() + ".");
							// update the status records in KG when the job is completed
							devClient.updateStatusAtJobCompletion(derivation, newDerivedIRI);
							LOGGER.info("Derivation <" + derivation + "> is now finished, to be cleaned up.");
						}
					}
					break;
				case INPROGRESS:
					// the current design just passes when the derivation is "InProgress"
					break;
				case FINISHED:
					// clean up the derivation at "Finished" status
					devClient.cleanUpFinishedDerivationUpdate(derivation);
					LOGGER.info("Derivation <" + derivation + "> is now cleand up.");
					break;
				case NOSTATUS:
					break;
			}
		});
	}

	public static void main(String[] args) {
		Map<String, List<String>> map = new HashMap<>();
		System.out.println(map.containsKey("arg0"));
		System.out.println(map);
		System.out.println(map.isEmpty());
		if (map.containsKey("arg0")) {
			System.out.println("yes");
		} else {
			System.out.println("no");
		}
	}
}
