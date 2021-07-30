package uk.ac.cam.cares.jps.base.derivedquantity;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.json.JSONArray;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.interfaces.StoreClientInterface;

/**
 * this class acts as an interface to create and deal with derived quantities
 * certain functions could be called directly from DerivedQuantitySparql, e.g. createDerivedQuantity
 * this class lists the functions that are intended for users to use
 * @author Kok Foong Lee
 *
 */
public class DerivedQuantityClient {
	// input and output of agents need to be a JSONArray consisting a list of IRIs
	public static final String AGENT_INPUT_KEY = "agent_input";
	public static final String AGENT_OUTPUT_KEY = "agent_output";
	// defines the endpoint DerivedQuantityClient should act on
	StoreClientInterface kbClient;
	
	private static final Logger LOGGER = LoggerFactory.getLogger(DerivedQuantityClient.class);
    
    public DerivedQuantityClient(StoreClientInterface kbClient) {
    	this.kbClient = kbClient;
    }
    
    /**
     * This creates a new derived instance and adds the following statements
     * <entity> <belongsTo> <derived>, <derived> <isDerivedUsing> <agentIRI>, <agentIRI> <hasHttpUrl> <agentURL>, <derived> <isDerivedFrom> <inputsIRI>
     * Use this for instances that get replaced by agents
     * @param derivedQuantityIRI
     * @param inputsIRI
     * @param agentIRI
     */
    public String createDerivedQuantity(List<String> entities, String agentIRI, String agentURL, List<String> inputsIRI) {
    	return DerivedQuantitySparql.createDerivedQuantity(this.kbClient, entities, agentIRI, agentURL, inputsIRI);
    }
    
    /**
     * use this if all the agent does to the instance is appending time series data, entity do not get replaced
     * @param entity
     * @param agentIRI
     * @param agentURL
     * @param inputsIRI
     */
    public String createDerivedQuantityWithTimeSeries(String entity, String agentIRI, String agentURL, List<String> inputsIRI) {
    	return DerivedQuantitySparql.createDerivedQuantityWithTimeSeries(this.kbClient, entity, agentIRI, agentURL, inputsIRI);
    }
    
    /**
     * adds a timestamp to your input following the w3c standard for unix timestamp https://www.w3.org/TR/owl-time/
     * <entity> <hasTime> <time>, <time> <numericPosition> 123
     * @param entity
     */
    public void addTimeInstance(String entity) {
    	DerivedQuantitySparql.addTimeInstance(kbClient, entity);
    }
    
    /**
     * you may want to use this to update an input's timestamp, the DerivedQuantityClient does not deal with inputs directly
     */
    public void updateTimestamp(String entity) {
    	DerivedQuantitySparql.updateTimeStamp(kbClient, entity);
    }
    
    /**
	 * makes sure the given instance is up-to-date by comparing its timestamp to all of its dependents
	 * the input, derivedIRI, should have an rdf:type DerivedQuantity or DerivedQuantityWithTimeSeries
	 * @param kbClient
	 * @param derivedIRI
	 */
	public void updateInstance(String derivedIRI) {
		// keep track of quantities to avoid circular dependencies
		// String of the map is the derived/input IRI, Integer is the status
		// assignment of the status follows the depth-first search principle
		// a vertex is assigned status = 1 when first discovered
		// then assigned status = 2 after it has no inputs
		// there is a circular dependency when a derived quantity accesses an input with status=1, accessing an input
		// with status =2 is fine
		Map<String,Integer> vertexList = new HashMap<>();
		updateInstance(derivedIRI, vertexList);
	}
	
	/**
	 * This checks for any circular dependency and ensures that all the linked inputs have a suitable timestamp attached
	 * This does not check for everything, e.g. instances having appropriate rdf:types, and the agent design
	 * @param derived
	 * @return
	 */
	public boolean validateDerived(String derived) {
		boolean valid = true;
		// keep track of quantities to avoid circular dependencies
		Map<String,Integer> vertexList = new HashMap<>();
        
		try {
			validateDerived(derived, vertexList);
		} catch (Exception e) {
			LOGGER.error(e.getMessage());
			valid = false;
		}
		return valid;
	}
	
	/**
	 * All private functions below
	 */
	
	/**
	 * called by the public function updateInstance
	 * @param instance
	 * @param derivedList
	 */
	private void updateInstance(String instance, Map<String,Integer> vertexList) {
		// this will query the direct inputs, as well as the derived instance of any of the inputs if the input is part of a derived instance
		List<String> inputsAndDerived = DerivedQuantitySparql.getInputsAndDerived(this.kbClient, instance);
		
		if (!vertexList.containsKey(instance)) {
			vertexList.put(instance, 1);
		}
		
		for (String input : inputsAndDerived) {
			if (vertexList.containsKey(input)) {
				if (vertexList.get(input) == 1) { 
					LOGGER.error("DerivedQuantityClient: Circular dependency detected");
					throw new JPSRuntimeException("DerivedQuantityClient: Circular dependency detected");
				} else {
					updateInstance(input, vertexList);
				}
			} else {
				updateInstance(input, vertexList);
			}
		}
		
		// modify status to "finished"
		vertexList.replace(instance, 1, 2);

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
					
					// link new entities to derived instance, adding ?x <belongsTo> <instance>
					DerivedQuantitySparql.addNewEntitiesToDerived(kbClient, instance, newEntities);
					
					if (derivedAndType.get(0).size() > 0) {
						// after deleting the old entity, we need to make sure that it remains linked to the appropriate derived instance
						String[] classOfNewEntities = DerivedQuantitySparql.getInstanceClass(kbClient, newEntities);
						
						// look for the entity with the same rdf:type that we need to reconnect
						List<String> oldDerivedList = derivedAndType.get(0);
						List<String> oldTypeList = derivedAndType.get(1);
				
						// for each instance in the old derived instance that is connected to another derived instance, reconnect it
						for (int i = 0; i < oldDerivedList.size(); i++) {
							// index in the new array with the matching type
							Integer matchingIndex = null;
							for (int j = 0; j < classOfNewEntities.length; j++) {
								if (classOfNewEntities[j].contentEquals(oldTypeList.get(i))) {
									if (matchingIndex != null) {
										LOGGER.error("Duplicate type found within output, the DerivedQuantityClient does not support this");
										throw new JPSRuntimeException("Duplicate type found within output, the DerivedQuantityClient does not support this");
									}
									matchingIndex = j;
								}
							}
							if (matchingIndex == null) {
								LOGGER.error("Unable to find an instance with the same rdf:type to reconnect to " + oldDerivedList.get(i));
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
	 * called by the public function validateDerived
	 * @param instance
	 * @param derivedList
	 */
	private void validateDerived(String instance, Map<String,Integer> vertexList) {
		List<String> inputsAndDerived = DerivedQuantitySparql.getInputsAndDerived(this.kbClient, instance);
		if (!vertexList.containsKey(instance)) {
			vertexList.put(instance, 1);
		}
		
		for (String input : inputsAndDerived) {
			if (vertexList.containsKey(input)) {
				if (vertexList.get(input) == 1) { 
					LOGGER.error("DerivedQuantityClient: Circular dependency detected");
					throw new JPSRuntimeException("DerivedQuantityClient: Circular dependency detected");
				} else {
					updateInstance(input, vertexList);
				}
			} else {
				updateInstance(input, vertexList);
			}
		}
		
		// modify status to "finished"
		vertexList.replace(instance, 1, 2);
		
		// check that for each derived quantity, there is a timestamp to compare to
		String[] inputs = DerivedQuantitySparql.getInputs(this.kbClient, instance);
		if (inputs.length > 0) {
			// getTimestamp will throw an exception if there is no timestamp
			DerivedQuantitySparql.getTimestamp(kbClient, instance);
			for (String input : inputs) {
				DerivedQuantitySparql.getTimestamp(kbClient, input);
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
