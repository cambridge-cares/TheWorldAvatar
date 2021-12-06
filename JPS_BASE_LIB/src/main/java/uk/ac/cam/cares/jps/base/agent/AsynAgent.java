package uk.ac.cam.cares.jps.base.agent;

import java.util.List;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.derivation.DerivationClient;
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
	
	public static final String msg = "This agent is an instance of AsynAgent, which monitors the derivation and should NOT be used as an HTTP servlet.";
	
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
    	List<String> listOfDerivation = devClient.getDerivations(agentIRI);
    	
    	for (String derivation : listOfDerivation) {
    		// check if the derivation is an instance of asynchronous derivation
    		if (devClient.isDerivedAsynchronous(derivation)) {
    			if (devClient.isPendingUpdate(derivation)) {
    				devClient.checkAtPendingUpdate(derivation);
    			} else if (devClient.isRequested(derivation)) {
    				JSONObject agentInputs = devClient.retrieveAgentInputs(derivation, agentIRI);
    				devClient.markAsInProgress(derivation);
    				List<String> newDerivedIRI = setupJob(agentInputs);
    				devClient.updateStatusAtJobCompletion(derivation, newDerivedIRI);
    			} else if (devClient.isInProgress(derivation)) {
    				// at the moment the design is the agent just pass when it's detected as "InProgress"
    			} else if (devClient.isFinished(derivation)) {
    				devClient.cleanUpFinishedDerivationUpdate(derivation);
    			}
    		} else {
    			// TODO ideally this should call the update or other functions in synchronous derivation function
    			LOGGER.info("Derivation instance <" + derivation + "> is not an asynchronous derivation.");
    		}
    	}
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
