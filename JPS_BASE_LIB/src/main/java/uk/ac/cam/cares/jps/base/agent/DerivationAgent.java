package uk.ac.cam.cares.jps.base.agent;

import java.time.Instant;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import javax.ws.rs.BadRequestException;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.TriplePattern;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.config.JPSConstants;
import uk.ac.cam.cares.jps.base.derivation.Derivation;
import uk.ac.cam.cares.jps.base.derivation.DerivationClient;
import uk.ac.cam.cares.jps.base.derivation.DerivationInputs;
import uk.ac.cam.cares.jps.base.derivation.DerivationOutputs;
import uk.ac.cam.cares.jps.base.derivation.DerivationSparql;
import uk.ac.cam.cares.jps.base.derivation.StatusType;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
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

	public StoreClientInterface storeClient;
	// NOTE devClient this is made public so that developer extend DerivationAgent
	// can initialise this variable in init() function, otherwise it will throw
	// errors when executing this.devClient.reconnectNewDerivedIRIs when dealing
	// with synchronous derivations
	public DerivationClient devClient;

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

	@Override
	public JSONObject processRequestParameters(JSONObject requestParams) {
		JSONObject res = new JSONObject();
		if (validateInput(requestParams)) {
			// serialises DerivationInputs objects from JSONObject
			DerivationInputs inputs = new DerivationInputs(
				requestParams.getJSONObject(DerivationClient.AGENT_INPUT_KEY));

			// initialise DerivationOutputs, also set up information
			String derivationIRI = requestParams.getString(DerivationClient.DERIVATION_KEY);
			String derivationType = requestParams.getString(DerivationClient.DERIVATION_TYPE_KEY);
			DerivationOutputs outputs = new DerivationOutputs();
			outputs.setThisDerivation(derivationIRI);
			outputs.setRetrievedInputsAt(Instant.now().getEpochSecond());
			outputs.setOldEntitiesMap(requestParams.getJSONObject(DerivationClient.BELONGSTO_KEY));
			outputs.setOldEntitiesDownstreamDerivationMap(
					requestParams.getJSONObject(DerivationClient.DOWNSTREAMDERIVATION_KEY));

			// apply agent logic to convert inputs to outputs
			processRequestParameters(inputs, outputs);

			Derivation derivation = new Derivation(derivationIRI, derivationType);
			if (!derivation.isDerivationAsyn() && !derivation.isDerivationWithTimeSeries()) {
				// construct and fire SPARQL update given DerivationOutputs objects, if normal
				// derivation
				// NOTE this makes sure that the new generated instances/triples will
				// ONLY be written to knowledge graph if the target derivation is till outdated
				// at the point of executing SPARQL update, i.e. this solves concurrent request
				// issue as detailed in
				// https://github.com/cambridge-cares/TheWorldAvatar/issues/184
				String sparqlUpdate = this.devClient.reconnectNewDerivedIRIs(outputs.getOutputTriples(),
						outputs.getNewEntitiesDownstreamDerivationMap(), outputs.getThisDerivation(),
						outputs.getRetrievedInputsAt());
				LOGGER.info("Derivation Agent (URL: <" + requestParams.getString(JPSConstants.REQUESTURL)
						+ ">) attempted to update derivation DAG in the knowledge graph with: " + sparqlUpdate
						+ " Whether the update was successfully executed depends on if the target derivation is still outdated at the SPARQL update execution.");
			}

			// if all agent operations executed as expected, return retrievedInputsAt
			res.put(DerivationOutputs.RETRIEVED_INPUTS_TIMESTAMP_KEY, outputs.getRetrievedInputsAt());
		} else {
			res.put(DerivationClient.AGENT_OUTPUT_KEY, EMPTY_REQUEST_MSG);
		}
		return res;
	}

	@Override
	public void processRequestParameters(DerivationInputs derivationInputs, DerivationOutputs derivationOutputs) {
		// TODO developer needs to overwrite this function
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
		} else {
			if (!requestParams.has(DerivationClient.DERIVATION_KEY)) {
				String msg = this.getClass().toString()
						+ " agent received a request that doesn't have derivationIRI...";
				LOGGER.error(msg);
				throw new JPSRuntimeException(msg);
			}
			if (!requestParams.has(DerivationClient.BELONGSTO_KEY)) {
				String msg = this.getClass().toString()
						+ " agent received a request that doesn't have information about old outputs...";
				LOGGER.error(msg);
				throw new JPSRuntimeException(msg);
			}
			if (!requestParams.has(DerivationClient.DOWNSTREAMDERIVATION_KEY)) {
				String msg = this.getClass().toString()
						+ " agent received a request that doesn't have information about downstream derivation...";
				LOGGER.error(msg);
				throw new JPSRuntimeException(msg);
			}
		}
		return true;
	}

	/**
	 * Monitor the asynchronous derivation that isDerivedUsing the agentIRI.
	 * 
	 * @param agentIRI
	 */
	@Override
	public void monitorAsyncDerivations(String agentIRI) {
		// function getDerivationsAndStatusType ONLY consider the async derivation
		// sync derivations <isDerivedUsing> agentIRI will be handled as HTTP requests
		Map<String, StatusType> derivationsAndStatusType = devClient.getDerivationsAndStatusType(agentIRI);
		if (derivationsAndStatusType.isEmpty()) {
			LOGGER.info("Currently, no asynchronous derivation <isDerivedUsing> <" + agentIRI + ">.");
		} else {
			LOGGER.info("A list of asynchronous derivations that <isDerivedUsing> <" + agentIRI + "> are retrieved: "
					+ derivationsAndStatusType.toString() + ".");
		}

		// iterate over each derivation that the agent is monitoring and make decisions
		// based on its status
		derivationsAndStatusType.forEach((derivation, statusType) -> {
			LOGGER.info("Asynchronous derivation <" + derivation + "> has status type: " + statusType + ".");
			switch (statusType) {
				case REQUESTED:
					Map<String, List<String>> immediateUpstreamDerivationToUpdate = devClient
							.checkImmediateUpstreamDerivation(derivation);
					if (immediateUpstreamDerivationToUpdate
							.containsKey(DerivationSparql.ONTODERIVATION_DERIVATIONASYN)) {
						// if any of upstream async derivations are still outdated, skip
						LOGGER.info("Asynchronous derivation <" + derivation
								+ "> has a list of immediate upstream asynchronous derivations to be updated: "
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
							LOGGER.info("Agent <" + agentIRI + "> retrieved inputs of asynchronous derivation <"
									+ derivation + ">: " + agentInputs.toString() + ".");
							LOGGER.info("Asynchronous derivation <" + derivation + "> is now in progress.");
							// serialise JSONObject retrieved from KG to instance of DerivationInputs
							DerivationInputs derivationInputs = new DerivationInputs(
									agentInputs.getJSONObject(DerivationClient.AGENT_INPUT_KEY));
							DerivationOutputs derivationOutputs = new DerivationOutputs();
							// perform the conversion from DerivationInputs to DerivationOutputs
							processRequestParameters(derivationInputs, derivationOutputs);
							// deserialise the derivationOutputs to a list of String of new derived IRI
							List<String> newDerivedIRI = derivationOutputs.getNewDerivedIRI();
							List<TriplePattern> newTriples = derivationOutputs.getOutputTriples();
							// update the status records in KG when the job is completed, also writes all
							// new triples to KG
							devClient.updateStatusAtJobCompletion(derivation, newDerivedIRI, newTriples);
							LOGGER.info("Asynchronous derivation <" + derivation + "> has new generated derived IRI: "
									+ newDerivedIRI.toString() + ".");
							LOGGER.info("Asynchronous derivation <" + derivation + "> has all new generated triples: "
									+ newTriples.stream().map(t -> t.getQueryString()).collect(Collectors.toList()));
							LOGGER.info(
									"Asynchronous derivation <" + derivation + "> is now finished, to be cleaned up.");
						}
					}
					break;
				case INPROGRESS:
					// the current design just passes when the derivation is "InProgress"
					break;
				case FINISHED:
					// clean up the derivation at "Finished" status
					devClient.cleanUpFinishedDerivationUpdate(derivation);
					LOGGER.info("Asynchronous derivation <" + derivation + "> is now cleand up.");
					break;
				case NOSTATUS:
					break;
			}
		});
	}
}
