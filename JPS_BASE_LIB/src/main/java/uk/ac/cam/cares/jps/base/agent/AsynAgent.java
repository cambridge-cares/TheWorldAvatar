package uk.ac.cam.cares.jps.base.agent;

import java.util.List;
import java.util.Map;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.derivation.DerivationClient;
import uk.ac.cam.cares.jps.base.derivation.StatusType;
import uk.ac.cam.cares.jps.base.interfaces.AsynAgentInterface;
import uk.ac.cam.cares.jps.base.interfaces.StoreClientInterface;

/**
 * This class acts as a template to be extended when developing agent with asynchronous derivation functionality.
 * @author Jiaru Bai (jb2197@cam.ac.uk)
 *
 */
@Deprecated
public class AsynAgent extends JPSAgent implements AsynAgentInterface {
	
	/**
	 * LOGGER for outputs.
	 */
	private static final Logger LOGGER = LogManager.getLogger(AsynAgent.class);
	
	/**
    *
    */
	private static final long serialVersionUID = 1L;
	
	static final String msg = "This agent is an instance of AsynAgent, which monitors the derivation and should NOT be used as an HTTP servlet.";
	
	StoreClientInterface storeClient;
	DerivationClient devClient;
	
	/**
	 * Provide default constructor to enable agent initialisation.
	 */
	public AsynAgent() {
		LOGGER.info("A new AsynAgent has been initialised.");
	}
	
	/**
     * Initialise a new AsynAgent with StoreClientInterface and base URL for derivation instances.
     */
	public AsynAgent(StoreClientInterface storeClient, String derivationInstanceBaseURL) {
		this.storeClient = storeClient;
		this.devClient = new DerivationClient(storeClient, derivationInstanceBaseURL);
	}
	
	/**
	 * Display default response message when accidentally invoking agent via an HTTP request.
	 */
	@Override
	public JSONObject processRequestParameters(JSONObject requestParams) {
		
		JSONObject response = new JSONObject();
		response.put("status", AsynAgent.msg);
		
		return response;
	}
	
	/**
     * Monitor the derivation that isDerivedUsing the agentIRI.
     * @param agentIRI
     */
    public void monitorDerivation(String agentIRI) {
    	// TODO this SPARQL query does NOT consider synchronous derivations that isDerivedUsing given agentIRI
    	// TODO think about the situation where one agent monitors both synchronous and asynchronous derivations
    	Map<String, StatusType> derivationsAndStatusType = devClient.getDerivationsAndStatusType(agentIRI);
		if (derivationsAndStatusType.isEmpty()) {
			LOGGER.info("Currently, no derivation <isDerivedUsing> <" + agentIRI + ">.");
		} else {
			LOGGER.info("A list of derivations that <isDerivedUsing> <"+ agentIRI +"> are retrieved: " + derivationsAndStatusType.toString() + ".");
		}

    	// iterate over each derivation that the agent is monitoring and make decisions based on its status
    	derivationsAndStatusType.forEach((derivation, statusType) -> {
			LOGGER.info("Derivation <" + derivation +"> has status type: " + statusType +".");
    		switch (statusType) {
			case REQUESTED:
					Map<String, List<String>> immediateUpstreamDerivationToUpdate = devClient
							.checkImmediateUpstreamDerivation(derivation);
				if (!immediateUpstreamDerivationToUpdate.isEmpty()) {
					LOGGER.info("Derivation <" + derivation + "> has a list of immediate upstream derivations to be updated: " + immediateUpstreamDerivationToUpdate.toString());
				} else {
					JSONObject agentInputs = devClient.retrieveAgentInputIRIs(derivation, agentIRI);
					LOGGER.info("Agent <" + agentIRI + "> retrieved inputs of derivation <" + derivation + ">: " + agentInputs.toString() + ".");
					LOGGER.info("Derivation <" + derivation + "> is now in progress.");
					List<String> newDerivedIRI = setupJob(agentInputs);
					LOGGER.info("Derivation <" + derivation + "> has new generated derived IRI: " + newDerivedIRI.toString() + ".");
					devClient.updateStatusAtJobCompletion(derivation, newDerivedIRI);
					LOGGER.info("Derivation <" + derivation + "> is now finished, to be cleaned up.");
				}
				break;
			case INPROGRESS:
				// at the moment the design is the agent just pass when it's detected as "InProgress"
				break;
			case FINISHED:
				devClient.cleanUpFinishedDerivationUpdate(derivation);
				LOGGER.info("Derivation <" + derivation + "> is now cleand up.");
				break;
			case NOSTATUS:
				break;
			}
    	});
    }

    /**
     * Set up the job based on the requestParams, it can either from the HTTP request, 
     * or the inputs from asynchronous derivation framework.
     * @param request
     */
	public List<String> setupJob(JSONObject requestParams) {
		return null;
	}
}
