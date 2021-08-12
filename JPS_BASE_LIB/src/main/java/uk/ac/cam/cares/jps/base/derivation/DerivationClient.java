package uk.ac.cam.cares.jps.base.derivation;

import java.util.List;
import org.json.JSONArray;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.jgrapht.graph.DefaultEdge;
import org.jgrapht.graph.DirectedAcyclicGraph;

import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.interfaces.StoreClientInterface;

/**
 * this class acts as an interface to create and deal with derived quantities
 * @author Kok Foong Lee
 *
 */
public class DerivationClient {
	// input and output of agents need to be a JSONArray consisting a list of IRIs with the do
	public static final String AGENT_INPUT_KEY = "agent_input";
	public static final String AGENT_OUTPUT_KEY = "agent_output";
	// defines the endpoint DerivedQuantityClient should act on
	StoreClientInterface kbClient;
	
	private static final Logger LOGGER = LoggerFactory.getLogger(DerivationClient.class);
    
    public DerivationClient(StoreClientInterface kbClient) {
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
    public String createDerivation(List<String> entities, String agentIRI, String agentURL, List<String> inputsIRI) {
    	String createdDerivation = DerivationSparql.createDerivation(this.kbClient, entities, agentIRI, agentURL, inputsIRI);
    	LOGGER.info("Instantiated derivation <" + createdDerivation + "> with the following properties");
    	LOGGER.info(entities + " belongsTo " + createdDerivation);
    	LOGGER.info(createdDerivation + " isDerivedFrom " + inputsIRI);
    	LOGGER.info(createdDerivation + " isDerivedUsing " + agentIRI + " located at " + agentURL);
    	return createdDerivation;
    }
    
    /**
     * use this if all the agent does to the instance is appending time series data, entity do not get replaced
     * @param entity
     * @param agentIRI
     * @param agentURL
     * @param inputsIRI
     */
    public String createDerivationWithTimeSeries(List<String> entities, String agentIRI, String agentURL, List<String> inputsIRI) {
    	String createdDerivation = DerivationSparql.createDerivationWithTimeSeries(this.kbClient, entities, agentIRI, agentURL, inputsIRI);
    	LOGGER.info("Instantiated derivation with time series <" + createdDerivation + "> with the following properties");
    	LOGGER.info(entities + " belongsTo " + createdDerivation);
    	LOGGER.info(createdDerivation + " isDerivedFrom " + inputsIRI);
    	LOGGER.info(createdDerivation + " isDerivedUsing " + agentIRI + " located at " + agentURL);
    	return createdDerivation;
    }
    
    /**
     * adds a timestamp to your input following the w3c standard for unix timestamp https://www.w3.org/TR/owl-time/
     * <entity> <hasTime> <time>, <time> <numericPosition> 123
     * @param entity
     */
    public void addTimeInstance(String entity) {
    	DerivationSparql.addTimeInstance(kbClient, entity);
    	LOGGER.info("Added timestamp to " + entity);
    }
    
    /**
     * you may want to use this to update an input's timestamp, the DerivedQuantityClient does not deal with inputs directly
     */
    public void updateTimestamp(String entity) {
    	DerivationSparql.updateTimeStamp(kbClient, entity);
    	LOGGER.info("Updated timestamp of " + entity);
    }
    
    /**
	 * makes sure the given instance is up-to-date by comparing its timestamp to all of its dependents
	 * the input, derivedIRI, should have an rdf:type DerivedQuantity or DerivedQuantityWithTimeSeries
	 * @param kbClient
	 * @param derivedIRI
	 */
	public void updateDerivation(String derivedIRI) {
		// the graph object makes sure that there is no circular dependency
		DirectedAcyclicGraph<String,DefaultEdge> graph = new DirectedAcyclicGraph<String,DefaultEdge>(DefaultEdge.class);
		try {
			updateDerivation(derivedIRI, graph);
		} catch (Exception e) {
			LOGGER.error(e.getMessage());
			System.out.println(e.getMessage());
			throw new JPSRuntimeException(e);
		}
	}
	
	/**
	 * This checks for any circular dependency and ensures that all the linked inputs have a suitable timestamp attached
	 * This does not check for everything, e.g. instances having appropriate rdf:types, and the agent design
	 * @param derived
	 * @return
	 */
	public boolean validateDerivation(String derived) {
		// keep track of quantities to avoid circular dependencies
		DirectedAcyclicGraph<String,DefaultEdge> graph = new DirectedAcyclicGraph<String,DefaultEdge>(DefaultEdge.class);
        
		try {
			validateDerivation(derived, graph);
			return true;
		} catch (Exception e) {
			LOGGER.error(e.getMessage());
			System.out.println(e.getMessage());
		    throw new JPSRuntimeException(e);
		}
	}
	
	/**
	 * All private functions below
	 */
	
	/**
	 * called by the public function updateInstance
	 * @param instance
	 * @param derivedList
	 */
	private void updateDerivation(String instance, DirectedAcyclicGraph<String,DefaultEdge> graph) {
		// this will query the direct inputs, as well as the derived instance of any of the inputs if the input is part of a derived instance
		List<String> inputsAndDerived = DerivationSparql.getInputsAndDerived(this.kbClient, instance);

		if (!graph.containsVertex(instance)) {
			graph.addVertex(instance);
		}
		
		for (String input : inputsAndDerived) {
			if (!graph.containsVertex(input)) {
				graph.addVertex(input);
			}
			graph.addEdge(instance, input); // will throw an error here if there is circular dependency
			updateDerivation(input, graph);
		}

		// inputs required by the agent
		String[] inputs = DerivationSparql.getInputs(this.kbClient, instance);
		if (inputs.length > 0) {
			// at this point, "instance" is a derived instance for sure, any other instances will not go through this code
			// getInputs queries for <instance> <isDerivedFrom> ?x
			if (isOutOfDate(instance,inputs)) {
				LOGGER.info(instance + " is out-of-date when compared to " + inputs);
				// calling agent to create a new instance
				String agentURL = DerivationSparql.getAgentUrl(kbClient, instance);
				LOGGER.info("Calling agent at " + agentURL);
				JSONObject requestParams = new JSONObject();
				JSONArray iris = new JSONArray();
				for (int i = 0; i < inputs.length; i++) {
					iris.put(inputs[i]);
				}
				requestParams.put(AGENT_INPUT_KEY, iris);
				
				LOGGER.info("Updating " + instance + " using agent at " + agentURL + " with http request " + requestParams);
				String response = AgentCaller.executeGetWithURLAndJSON(agentURL, requestParams.toString());
				
				LOGGER.info("Obtained http response from agent: " + response);
				
				// if it is a derived quantity with time series, there will be no changes to the instances
				if (!DerivationSparql.isDerivedWithTimeSeries(this.kbClient, instance)) {
					// collect new instances created by agent
					JSONArray output = new JSONObject(response).getJSONArray(AGENT_OUTPUT_KEY);
					String[] newEntities = new String[output.length()];
					for (int i = 0; i < output.length(); i++) {
						newEntities[i] = output.getString(i);
					}

					// get all the other entities linked to the derived quantity, to be deleted and replaced with new entities
					// query for ?x <belongsTo> <instance>
					String[] entities = DerivationSparql.getDerivedEntities(kbClient, instance);
					
					// check if any of the old entities is an input for another derived quantity
					// query ?x <isDerivedFrom> <entity>, <entity> a ?y
					// where ?x = a derived instance, ?y = class of entity
					// index 0 = derivedIRIs list, index 1 = type IRI list
					List<List<String>> derivedAndType = DerivationSparql.getIsDerivedFromEntities(kbClient, entities);
					
					// delete old instances
					DerivationSparql.deleteInstances(kbClient, entities);
					LOGGER.info("Deleted old instances: " + entities);
					
					// link new entities to derived instance, adding ?x <belongsTo> <instance>
					DerivationSparql.addNewEntitiesToDerived(kbClient, instance, newEntities);
					LOGGER.info("Added new instances " + newEntities + " to the derivation " + instance);
					
					if (derivedAndType.get(0).size() > 0) {
						LOGGER.info("This derivation contains at least one entity which is an input to another derivation");
						LOGGER.info("Relinking new instance(s) to the derivation by matching their rdf:type");
						// after deleting the old entity, we need to make sure that it remains linked to the appropriate derived instance
						String[] classOfNewEntities = DerivationSparql.getInstanceClass(kbClient, newEntities);
						
						// look for the entity with the same rdf:type that we need to reconnect
						List<String> oldDerivedList = derivedAndType.get(0);
						List<String> oldTypeList = derivedAndType.get(1);
				
						// for each instance in the old derived instance that is connected to another derived instance, reconnect it
						for (int i = 0; i < oldDerivedList.size(); i++) {
							LOGGER.info("Searching within " + newEntities + " with rdf:type " + oldTypeList.get(i));
							// index in the new array with the matching type
							Integer matchingIndex = null;
							for (int j = 0; j < classOfNewEntities.length; j++) {
								if (classOfNewEntities[j].contentEquals(oldTypeList.get(i))) {
									if (matchingIndex != null) {
										throw new JPSRuntimeException("Duplicate rdf:type found within output, the DerivationClient does not support this");
									}
									matchingIndex = j;
								}
							}
							if (matchingIndex == null) {
								String reconnectError = "Unable to find an instance with the same rdf:type to reconnect to " + oldDerivedList.get(i);
								throw new JPSRuntimeException(reconnectError);
							}
						    // reconnect
							DerivationSparql.reconnectInputToDerived(kbClient, newEntities[matchingIndex], oldDerivedList.get(i));
						}
					}
				}
				// if there are no errors, assume update is successful
				DerivationSparql.updateTimeStamp(kbClient, instance);
				LOGGER.info("Updated timestamp of " + instance);
			}
		}
	}
	
	/**
	 * called by the public function validateDerived
	 * @param instance
	 * @param derivedList
	 */
	private void validateDerivation(String instance, DirectedAcyclicGraph<String,DefaultEdge> graph) {
		List<String> inputsAndDerived = DerivationSparql.getInputsAndDerived(this.kbClient, instance);
		if (!graph.containsVertex(instance)) {
			graph.addVertex(instance);
		}
		
		for (String input : inputsAndDerived) {
			if (!graph.containsVertex(input)) {
				graph.addVertex(input);
			}
			graph.addEdge(instance, input); // will throw an error here if there is circular dependency
			validateDerivation(input, graph);
		}
		
		// check that for each derived quantity, there is a timestamp to compare to
		String[] inputs = DerivationSparql.getInputs(this.kbClient, instance);
		if (inputs.length > 0) {
			// getTimestamp will throw an exception if there is no timestamp
			DerivationSparql.getTimestamp(kbClient, instance);
			for (String input : inputs) {
				DerivationSparql.getTimestamp(kbClient, input);
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
	    long instanceTimestamp = DerivationSparql.getTimestamp(this.kbClient, instance);
	    
	    for (int i = 0; i < inputs.length; i++) {
	    	long inputTimestamp = DerivationSparql.getTimestamp(this.kbClient, inputs[i]);
	    	if (inputTimestamp > instanceTimestamp) {
	    		outOfDate = true;
	    		return outOfDate;
	    	}
	    }
	    return outOfDate;
	}
	
	/**
	 * returns the derivation instance linked to this entity
	 * @param entity
	 * @return
	 */
	public String getDerivationOf(String entity) {
		return DerivationSparql.getDerivedIRI(kbClient, entity);
	}
}
