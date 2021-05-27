package uk.ac.cam.cares.jps.base.derivedquantity;

import org.json.JSONArray;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.interfaces.KnowledgeBaseClientInterface;

public class DerivedQuantityClient {
	// defines the endpoint DerivedQuantityClient should act on
    KnowledgeBaseClientInterface kbClient;
    
    public DerivedQuantityClient(KnowledgeBaseClientInterface kbClient) {
    	this.kbClient = kbClient;
    }
    
    /**
     * Links the given derived quantity to its inputs and the agent used to derived it
     * @param derivedQuantityIRI
     * @param inputsIRI
     * @param agentIRI
     */
    public void initDerivedQuantity(String derivedQuantityIRI, String[] inputsIRI, String agentIRI, String agentURL) {
    	DerivedQuantitySparql.initDerivedQuantity(this.kbClient, derivedQuantityIRI, inputsIRI, agentIRI, agentURL);
    }
    
    /** 
     * ensures the queried quantity is up-to-date, then execute a query
     * @param derivedQuantityIRI
     * @param query
     * @return
     */
    public JSONArray queryDerivedQuantity(String derivedQuantityIRI, String query) {
    	updateInstance(derivedQuantityIRI);
    	this.kbClient.setQuery(query);
    	return this.kbClient.executeQuery();
    }
    
    /**
	 * makes sure the given instance is up-to-date by comparing its timestamp to all of its dependents
	 * @param kbClient
	 * @param derivedQuantity
	 */
	private void updateInstance(String instance) {
		String[] inputs = DerivedQuantitySparql.getInputs(this.kbClient, instance);
		
		for (int i = 0; i < inputs.length; i++) {
			updateInstance(inputs[i]);
		}
		
		if (isOutOfDate(instance, inputs)) {
			String agentURL = DerivedQuantitySparql.getAgentUrl(kbClient, instance);
			JSONObject requestParams = new JSONObject();
			for (int i = 0; i < inputs.length; i++) {
				String key = "key" + i; // keys might not be necessary in the future if we scrap JSON object and just pass a list of strings
				requestParams.put(key, inputs[i]);
			}
			AgentCaller.executeGetWithURLAndJSON(agentURL, requestParams.toString());
			
			// if no exception thrown, assume update is successful and update timestamp
			DerivedQuantitySparql.updateTimeStamp(kbClient, instance);
		}
	}
	
	/**
	 * compares the timestamps of quantities used to derived this instance
	 * returns true if any of its input is newer
	 * @param instance
	 * @return
	 */
	private boolean isOutOfDate(String instance, String[] inputs) {
	    boolean outOfDate = false;
	    long instanceTimestamp = DerivedQuantitySparql.getTimestamp(this.kbClient, instance);
	    
	    for (int i = 0; i < inputs.length; i++) {
	    	long inputTimestamp = DerivedQuantitySparql.getTimestamp(this.kbClient, inputs[i]);
	    	if (inputTimestamp > instanceTimestamp) {
	    		outOfDate = true;
	    		return outOfDate;
	    	}
	    }
	    return outOfDate;
	}
}
