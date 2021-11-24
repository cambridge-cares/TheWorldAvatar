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
   
	StoreClientInterface storeClient;
	DerivationClient devClient;
	
	/**
     * Initialise a new AsynAgent with StoreClientInterface.
     */
	public AsynAgent(StoreClientInterface storeClient) {
        this.storeClient = storeClient;
        this.devClient = new DerivationClient(storeClient);
    }
	
//	public void init() throws ServletException {
//		logger.info("\n---------- Sensitivity Analysis Agent has started ----------");
//		System.out.println("---------- Sensitivity Analysis Agent has started ----------");
//		ScheduledExecutorService executorService = Executors.newSingleThreadScheduledExecutor();
////		RemoteStoreClient storeClient = new RemoteStoreClient();
////    	DerivationClient devClient = new DerivationClient(storeClient);
//		AsynAgent asynAgent = new AsynAgent();
//		
//		executorService.scheduleAtFixedRate(() -> {
//			try {
//				asynAgent.monitorDerivation("http://example/agent_iri");
//			} catch (JPSRuntimeException e) 
//			{e.printStackTrace();
//			}
//		}, 120, 120, TimeUnit.SECONDS);
//		logger.info("\n---------- Derivation instances are being monitored  ----------");
//		System.out.println("---------- Derivation instances are being monitored  ----------");
//	}
	
	/**
     * Monitor the derivation that isDerivedUsing the agentIRI.
     * @param agentIRI
     */
    public void monitorDerivation(String agentIRI) {
    	List<String> listOfDerivation = devClient.getDerivations(agentIRI);
    	
    	for (String derivation : listOfDerivation) {
    		// check if the derivation is an instance of asynchronous derivation
    		if (devClient.isDerivedAsynchronous(derivation)) {
    			if (devClient.isRequested(derivation)) {
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
