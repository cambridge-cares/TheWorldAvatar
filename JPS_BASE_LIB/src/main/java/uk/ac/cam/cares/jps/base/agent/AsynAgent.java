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
    	
    	// iterate over each derivation that the agent is monitoring and make decisions based on its status
    	derivationsAndStatusType.forEach((derivation, statusType) -> {
    		switch (statusType) {
			case PENDINGUPDATE:
				devClient.checkAtPendingUpdate(derivation);
				break;
			case REQUESTED:
				JSONObject agentInputs = devClient.retrieveAgentInputs(derivation, agentIRI);
				devClient.markAsInProgress(derivation);
				List<String> newDerivedIRI = setupJob(agentInputs);
				devClient.updateStatusAtJobCompletion(derivation, newDerivedIRI);
				break;
			case INPROGRESS:
				// at the moment the design is the agent just pass when it's detected as "InProgress"
				break;
			case FINISHED:
				devClient.cleanUpFinishedDerivationUpdate(derivation);
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
