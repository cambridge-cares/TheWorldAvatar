package uk.ac.cam.cares.jps.base.agent;

import java.time.Instant;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;

import javax.ws.rs.BadRequestException;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.TriplePattern;
import org.json.JSONObject;

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
	public static final String DERIVATION_CLIENT_NOT_INITIALISED = "DerivationClient is not initialised yet. You may want to instantiate DerivationAgent with DerivationAgent(StoreClientInterface, String).";

	/**
	 * Provide the default constructor to enable agent initialisation.
	 * This constructor is required to enable servlet create the very first instance of DerivationAgent before calling init() during initialisation.
	 */
	public DerivationAgent() {
		LOGGER.info("A new DerivationAgent has been initialised.");
	}

	/**
	 * This constructor initialises DerivationClient which will be used to handle both sync and async derivations.
	 * @param storeClient
	 * @param derivationInstanceBaseURL
	 */
	public DerivationAgent(StoreClientInterface storeClient, String derivationInstanceBaseURL) {
		this.storeClient = storeClient;
		this.devClient = new DerivationClient(storeClient, derivationInstanceBaseURL);
	}

	@Override
	public JSONObject processRequestParameters(JSONObject requestParams) {
		// check if this.devClient is initialised properly, this will throw meaningful exception
		checkIfDerivationClientInitialised();
		JSONObject res = new JSONObject();
		if (validateInput(requestParams)) {
			// serialises DerivationInputs objects from JSONObject
			DerivationInputs inputs = new DerivationInputs(
				requestParams.getJSONObject(DerivationClient.AGENT_INPUT_KEY));
			LOGGER.info("Received derivation request parameters: " + requestParams);

			// retrieve necessary information
			String derivationIRI = requestParams.getString(DerivationClient.DERIVATION_KEY);
			String derivationType = requestParams.getString(DerivationClient.DERIVATION_TYPE_KEY);
			Boolean syncNewInfoFlag = requestParams.getBoolean(DerivationClient.SYNC_NEW_INFO_FLAG);

			// initialise DerivationOutputs, also set up information
			DerivationOutputs outputs = new DerivationOutputs();
			outputs.setThisDerivation(derivationIRI);
			outputs.setRetrievedInputsAt(Instant.now().getEpochSecond());
			if (!syncNewInfoFlag) {
				outputs.setOldEntitiesMap(requestParams.getJSONObject(DerivationClient.BELONGSTO_KEY));
				outputs.setOldEntitiesDownstreamDerivationMap(
						requestParams.getJSONObject(DerivationClient.DOWNSTREAMDERIVATION_KEY));
			}

			// apply agent logic to convert inputs to outputs
			processRequestParameters(derivationIRI, inputs, outputs);

			// return response if this sync derivation is generated for new info
			if (syncNewInfoFlag) {
				String agentServiceIRI = requestParams.getString(DerivationClient.AGENT_IRI_KEY);
				this.devClient.writeSyncDerivationNewInfo(outputs.getOutputTriples(),
						outputs.getNewDerivedIRI(), agentServiceIRI, inputs.getAllIris(),
						derivationIRI, derivationType, outputs.getRetrievedInputsAt());
				res.put(DerivationOutputs.RETRIEVED_INPUTS_TIMESTAMP_KEY,
						outputs.getRetrievedInputsAt());
				res.put(DerivationClient.AGENT_OUTPUT_KEY,
						outputs.getNewEntitiesJsonMap());
				return res;
			}

			// only enters below if the computation was not for new information (new
			// instances)
			Derivation derivation = new Derivation(derivationIRI, derivationType);
			if (!derivation.isDerivationAsyn() && !derivation.isDerivationWithTimeSeries()) {
				// construct and fire SPARQL update given DerivationOutputs objects, if normal
				// derivation
				// NOTE this makes sure that the new generated instances/triples will
				// ONLY be written to knowledge graph if the target derivation is till outdated
				// at the point of executing SPARQL update, i.e. this solves concurrent request
				// issue as detailed in
				// https://github.com/cambridge-cares/TheWorldAvatar/issues/184
				boolean triplesChangedForSure = this.devClient.reconnectNewDerivedIRIs(outputs.getOutputTriples(),
						outputs.getNewEntitiesDownstreamDerivationMap(), outputs.getThisDerivation(),
						outputs.getRetrievedInputsAt());

				// for normal Derivation, we need to return both timestamp and the new derived
				if (triplesChangedForSure) {
					// if we know the triples are changed for sure, we return the triples
					// computed by this agent
					res.put(DerivationOutputs.RETRIEVED_INPUTS_TIMESTAMP_KEY,
							outputs.getRetrievedInputsAt());
					res.put(DerivationClient.AGENT_OUTPUT_KEY,
							outputs.getNewEntitiesJsonMap());
					LOGGER.info("Derivation update is done in the knowledge graph, returned response: " + res);
				} else {
					// if we are not certain, query the knowledge graph to get the accurate
					// information
					Derivation updated = this.devClient.getDerivation(derivationIRI);
					res.put(DerivationOutputs.RETRIEVED_INPUTS_TIMESTAMP_KEY, updated.getTimestamp());
					res.put(DerivationClient.AGENT_OUTPUT_KEY, updated.getBelongsToMap());
					LOGGER.info(
							"Unable to determine if the SPARQL update mutated triples, returned latest information in knowledge graph: "
									+ res);
				}
			} else {
				// for DerivationWithTimeSeries, we just need to return retrievedInputsAt
				res.put(DerivationOutputs.RETRIEVED_INPUTS_TIMESTAMP_KEY, outputs.getRetrievedInputsAt());
				LOGGER.info("DerivationWithTimeSeries update is done, returned response: " + res);
			}
		} else {
			res.put(DerivationClient.AGENT_OUTPUT_KEY, EMPTY_REQUEST_MSG);
		}
		return res;
	}

	@Override
	public void processRequestParameters(String derivationIRI, DerivationInputs derivationInputs, DerivationOutputs derivationOutputs) {
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
			if (requestParams.getBoolean(DerivationClient.SYNC_NEW_INFO_FLAG)) {
				if (!requestParams.has(DerivationClient.AGENT_IRI_KEY)) {
					String msg = this.getClass().toString()
							+ " agent received a request for sync new information that doesn't have information about agent IRI...";
					LOGGER.error(msg);
					throw new JPSRuntimeException(msg);
				}
			} else {
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
		}
		return true;
	}

	/**
	 * Monitor the asynchronous derivation that isDerivedUsing the agentIRI.
	 * 
	 * @param agentIRI
	 */
	@Override
	public void monitorAsyncDerivations(String agentIRI, long periodicalTimescaleInSecond) {
		// check if this.devClient is initialised properly, this will throw meaningful exception
		checkIfDerivationClientInitialised();
		// NOTE two things used to control the loop:
		// 1. breakOutTime - the time when the next run of monitorAsyncDerivations is scheduled
		long breakOutTime = System.currentTimeMillis() + TimeUnit.SECONDS.toMillis(periodicalTimescaleInSecond);
		// 2. queryAgain - flag to indicate if any of the derivation was Requested and consequently processed
		// thus we need to query the knowledge graph again to make sure all status are up-to-date (in case other thread made changes to the knowledge graph)
		boolean queryAgain = false;
		// process all derivations that are derived using this agent for at least once
		// then the loop will only be proceed again if the time is not up AND the queryAgain if true
		do {
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
			// NOTE a for loop is used here instead of Stream.forEach as we need to break out the iteration
			for (String derivation : derivationsAndStatusType.keySet()) {
				StatusType statusType = derivationsAndStatusType.get(derivation);
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
							// set flag to false to skips this "Requested" derivation until next time
							// this is to avoid the agent flooding the KG with queries of the status over a short period of time
							queryAgain = false;
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
								// if another agent thread is updating the same derivation concurrently
								// and successed before this thread, then this method will return false
								boolean progressToJob = devClient.updateStatusBeforeSetupJob(derivation);
								// only progress to job if the status is updated successfully
								// otherwise, the other thread will handle the job
								if (progressToJob) {
									LOGGER.info("Agent <" + agentIRI + "> retrieved inputs of asynchronous derivation <"
											+ derivation + ">: " + agentInputs.toString() + ".");
									LOGGER.info("Asynchronous derivation <" + derivation + "> is now in progress.");
									// serialise JSONObject retrieved from KG to instance of DerivationInputs
									DerivationInputs derivationInputs = new DerivationInputs(
											agentInputs.getJSONObject(DerivationClient.AGENT_INPUT_KEY));
									DerivationOutputs derivationOutputs = new DerivationOutputs();
									// perform the conversion from DerivationInputs to DerivationOutputs
									processRequestParameters(derivation, derivationInputs, derivationOutputs);
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
								} else {
									LOGGER.info("Asynchronous derivation <" + derivation
											+ "> is already in progress by another agent thread.");
								}
							}
							// set flag to true as either (1) the agent has been process this derivation for some time
							// and status of other derivations in KG might have changed by other processes during this time
							// or (2) the derivation is processed by another agent therefore needs a record update
							queryAgain = true;
						}
						break;
					case INPROGRESS:
						// the current design just passes when the derivation is "InProgress"
						// the queryAgain flag is set as false to let agent carry on to next derivation in the list
						queryAgain = false;
						break;
					case FINISHED:
						// clean up the derivation at "Finished" status
						devClient.cleanUpFinishedDerivationUpdate(derivation);
						LOGGER.info("Asynchronous derivation <" + derivation + "> is now cleand up.");
						// set flag to false as this cleaning up process is fast and no need to query again
						queryAgain = false;
						break;
					case NOSTATUS:
						// no need to queryAgain as the derivation is considered as up-to-date
						queryAgain = false;
						break;
				}
				// break out the for loop and query again the list of derivations and their status
				if (queryAgain) {
					break;
				}
			};
		} while (System.currentTimeMillis() < breakOutTime && queryAgain); // process until the time is up and if have not gone through all derivations
	}

	public void checkIfDerivationClientInitialised() {
		if (Objects.isNull(this.devClient)) {
			throw new JPSRuntimeException(DERIVATION_CLIENT_NOT_INITIALISED);
		}
	}
}
