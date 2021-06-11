package uk.ac.cam.cares.jps.base.derivedquantity;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
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
    public void initDerivedQuantity(String derivedQuantityIRI, String agentIRI, String agentURL, String... inputsIRI) {
    	DerivedQuantitySparql.initDerivedQuantity(this.kbClient, derivedQuantityIRI, agentIRI, agentURL, inputsIRI);
    }
    
    /**
	 * makes sure the given instance is up-to-date by comparing its timestamp to all of its dependents
	 * @param kbClient
	 * @param derivedQuantity
	 */
	public void updateInstance(String instance) {
		// keep track of quantities to avoid circular dependencies
		List<String> derivedList = new ArrayList<>(); 
		updateInstance(instance, derivedList);
	}
	
	/**
	 * throws an exception if there is a circular dependency
	 * @param instance
	 * @param derivedList
	 */
	private void updateInstance(String instance, List<String> derivedList) {
		String[] inputs = DerivedQuantitySparql.getInputs(this.kbClient, instance);
		
		for (int i = 0; i < inputs.length; i++) {
			if (!derivedList.contains(inputs[i])) {
				derivedList.add(instance);
				updateInstance(inputs[i], derivedList);
			} else {
				throw new JPSRuntimeException("DerivedQuantityClient: Circular dependency detected");
			}
		}
		
		// this is necessary because the inputs may be replaced by new instances
		inputs = DerivedQuantitySparql.getInputs(this.kbClient, instance);
		if (inputs.length > 0) {
			if (isOutOfDate(instance,inputs)) {
				String agentURL = DerivedQuantitySparql.getAgentUrl(kbClient, instance);
				JSONObject requestParams = new JSONObject();
				for (int i = 0; i < inputs.length; i++) {
					String key = "key" + i; // keys might not be necessary in the future if we scrap JSON object and just pass a list of strings
					requestParams.put(key, inputs[i]);
				}
				String response = AgentCaller.executeGetWithURLAndJSON(agentURL, requestParams.toString());
				String instanceClass = DerivedQuantitySparql.getInstanceClass(kbClient, instance);
				String newInstance = getNewIRI(response, instanceClass);
				
				if (newInstance == null) {
					throw new JPSRuntimeException("DerivedQuantityClient: No suitable output from " + agentURL);
				}
				
				// replace triples involving old IRI to preserve the link to the new instance
				DerivedQuantitySparql.replaceDerivedTriples(this.kbClient,instance,newInstance);
				DerivedQuantitySparql.updateTimeStamp(kbClient, newInstance);
			}
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
	
	/**
	 * return IRI within response that has the same class as the one it's replacing
	 * @param response
	 * @param instanceClass
	 * @return
	 */
	public String getNewIRI(String response, String instanceClass) {
		String newIRI = null;
		JSONObject json = new JSONObject(response);
		Iterator<String> keys = json.keys();
		while (keys.hasNext()) {
			newIRI = json.getString(keys.next());
			String newClass = DerivedQuantitySparql.getInstanceClass(kbClient, newIRI);
			if (newClass.equals(instanceClass)) {
				return newIRI;
			}
		}
		return newIRI;
	}
}
