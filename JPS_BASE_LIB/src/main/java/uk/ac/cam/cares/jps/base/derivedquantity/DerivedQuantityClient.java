package uk.ac.cam.cares.jps.base.derivedquantity;

import java.util.ArrayList;
import java.util.List;

import org.json.JSONArray;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.interfaces.KnowledgeBaseClientInterface;

public class DerivedQuantityClient {
	// input and output of agents need to be a JSONArray consisting a list of IRIs
	public static final String AGENT_INPUT_KEY = "input";
	public static final String AGENT_OUTPUT_KEY = "output";
	// defines the endpoint DerivedQuantityClient should act on
    KnowledgeBaseClientInterface kbClient;
    
    public DerivedQuantityClient(KnowledgeBaseClientInterface kbClient) {
    	this.kbClient = kbClient;
    }
    
    /**
     * Links the given derived quantity to its inputs and the agent used to derived it
     * Use this for instances that get replaced by agents
     * @param derivedQuantityIRI
     * @param inputsIRI
     * @param agentIRI
     */
    public void createDerivedQuantity(List<String> entities, String agentIRI, String agentURL, List<String> inputsIRI) {
    	DerivedQuantitySparql.createDerivedQuantity(this.kbClient, entities, agentIRI, agentURL, inputsIRI);
    }
    
    /**
     * use this if all the agent does to the instance is appending time series data, entity do not get replaced
     * @param entity
     * @param agentIRI
     * @param agentURL
     * @param inputsIRI
     */
    public void createDerivedQuantityWithTimeSeries(String entity, String agentIRI, String agentURL, List<String> inputsIRI) {
    	DerivedQuantitySparql.createDerivedQuantityWithTimeSeries(this.kbClient, entity, agentIRI, agentURL, inputsIRI);
    }
    
    /**
	 * makes sure the given instance is up-to-date by comparing its timestamp to all of its dependents
	 * the input, derivedIRI, should have an rdf:type DerivedQuantity or DerivedQuantityWithTimeSeries
	 * @param kbClient
	 * @param derivedIRI
	 */
	public void updateInstance(String derivedIRI) {
		// keep track of quantities to avoid circular dependencies
		List<String> derivedList = new ArrayList<>(); 
		updateInstance(derivedIRI, derivedList);
	}
	
	/**
	 * throws an exception if there is a circular dependency
	 * @param instance
	 * @param derivedList
	 */
	private void updateInstance(String instance, List<String> derivedList) {
		// this will query the direct inputs, as well as the derived instance of any of the inputs if the input is part of a derived instance
		List<String> inputsAndDerived = DerivedQuantitySparql.getInputsAndDerived(this.kbClient, instance);
		
		for (String s : inputsAndDerived) {
			if (!derivedList.contains(s)) {
				derivedList.add(instance);
				updateInstance(s, derivedList);
			} else {
				throw new JPSRuntimeException("DerivedQuantityClient: Circular dependency detected");
			}
		}

		// inputs required by the agent
		String[] inputs = DerivedQuantitySparql.getInputs(this.kbClient, instance);
		if (inputs.length > 0) {
			// at this point, "instance" is a derived instance for sure, any other instances will not go through this code
			// getInputs queries for <instance> <isDerivedFrom> ?x
			if (isOutOfDate(instance,inputs)) {
				// calling agent to create a new instance
				String agentURL = DerivedQuantitySparql.getAgentUrl(kbClient, instance);
				JSONObject requestParams = new JSONObject();
				JSONArray iris = new JSONArray();
				for (int i = 0; i < inputs.length; i++) {
					iris.put(inputs[i]);
				}
				requestParams.put(AGENT_INPUT_KEY, iris);
				String response = AgentCaller.executeGetWithURLAndJSON(agentURL, requestParams.toString());
				
				// if it is a derived quantity with time series, there will be no changes to the instances
				if (!DerivedQuantitySparql.isDerivedWithTimeSeries(this.kbClient, instance)) {
					// collect new instances created by agent
					JSONArray output = new JSONObject(response).getJSONArray(AGENT_OUTPUT_KEY);
					String[] newEntities = new String[output.length()];
					for (int i = 0; i < output.length(); i++) {
						newEntities[i] = output.getString(i);
					}

					// get all the other entities linked to the derived quantity, to be deleted and replaced with new entities
					// query for ?x <belongsTo> <instance>
					String[] entities = DerivedQuantitySparql.getDerivedEntities(kbClient, instance);
					
					// check if any of the old entities is an input for another derived quantity
					// query ?x <isDerivedFrom> <entity>, <entity> a ?y
					// where ?x = a derived instance, ?y = class of entity
					// index 0 = derivedIRIs list, index 1 = type IRI list
					List<List<String>> derivedAndType = DerivedQuantitySparql.getIsDerivedFromEntities(kbClient, entities);
					
					// delete old instances
					DerivedQuantitySparql.deleteInstances(kbClient, entities);
					if (derivedAndType.get(0).size() > 1) {
						// after deleting the old entity, we need to make sure that it remains linked to the appropriate derived instance
						String[] classOfNewEntities = DerivedQuantitySparql.getInstanceClass(kbClient, newEntities);
						
						// look for the entity with the same rdf:type that we need to reconnect
						List<String> oldDerivedList = derivedAndType.get(0);
						List<String> oldTypeList = derivedAndType.get(1);
				
						// for each instance in the old derived instance that is connected to another derived instance, reconnect it
						for (int i = 0; i < oldDerivedList.size(); i++) {
							// index in the new array with the matching type
							Integer matchingIndex = null;
							for (int j = 0; j < classOfNewEntities.length; i++) {
								if (classOfNewEntities[j].contentEquals(oldTypeList.get(i))) {
									if (matchingIndex != null) {
										throw new JPSRuntimeException("Duplicate type found within output, the DerivedQuantityClient does not support this");
									}
									matchingIndex = j;
								}
							}
							if (matchingIndex == null) {
								throw new JPSRuntimeException("Unable to find an instance with the same rdf:type to reconnect to " + oldDerivedList.get(i));
							}
						    // reconnect
							DerivedQuantitySparql.reconnectInputToDerived(kbClient, newEntities[matchingIndex], oldDerivedList.get(i));
						}
					}
				}
				// if there are no errors, assume update is successful
				DerivedQuantitySparql.updateTimeStamp(kbClient, instance);
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
}
