package uk.ac.cam.cares.jps.base.derivation;

import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONArray;
import org.json.JSONObject;
import org.jgrapht.graph.DefaultEdge;
import org.jgrapht.graph.DirectedAcyclicGraph;

import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.interfaces.SetupJobInterface;
import uk.ac.cam.cares.jps.base.interfaces.StoreClientInterface;

/**
 * this class acts as an interface to create and deal with derived quantities
 * @author Kok Foong Lee
 * @author Jiaru Bai
 *
 */
public class DerivationClient {
	// input and output of agents need to be a JSONArray consisting a list of IRIs with the do
	public static final String AGENT_INPUT_KEY = "agent_input";
	public static final String AGENT_OUTPUT_KEY = "agent_output";
	public static final String DERIVATION_KEY = "derivation";
	// defines the endpoint DerivedQuantityClient should act on
	StoreClientInterface kbClient;
	
     /**
     * Logger for error output.
     */
    private static final Logger LOGGER = LogManager.getLogger(DerivationClient.class);
    
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
    	DerivationSparql.addTimeInstance(kbClient, createdDerivation);
    	LOGGER.info("Instantiated derivation <" + createdDerivation + ">");
    	LOGGER.debug("<" + entities + "> belongsTo <" + createdDerivation + ">");
    	LOGGER.debug("<" + createdDerivation + "> isDerivedFrom <" + inputsIRI + ">");
    	LOGGER.debug("<" + createdDerivation + "> isDerivedUsing <" + agentIRI + "> located at " + agentURL);
    	return createdDerivation;
    }
    
	/**
	 * This method creates a new derived instance and adds the following statements
	 * <entity> <belongsTo> <derived>, <derived> <isDerivedUsing> <agentIRI>, <derived> <isDerivedFrom> <inputsIRI>
     * Use this for instances that get replaced by agents, also when the information about agent exists already
	 * @param entities
	 * @param agentIRI
	 * @param inputsIRI
	 * @return
	 */
	public String createDerivation(List<String> entities, String agentIRI, List<String> inputsIRI) {
		String createdDerivation = DerivationSparql.createDerivation(this.kbClient, entities, agentIRI, inputsIRI);
		DerivationSparql.addTimeInstance(this.kbClient, createdDerivation);
		LOGGER.info("Instantiated derivation for asynchronous operation <" + createdDerivation + ">");
		LOGGER.debug("<" + entities + "> belongsTo <" + createdDerivation + ">");
		LOGGER.debug("<" + createdDerivation + "> isDerivedFrom <" + inputsIRI + ">");
		LOGGER.debug("<" + createdDerivation + "> isDerivedUsing <" + agentIRI + ">");
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
    	DerivationSparql.addTimeInstance(kbClient, createdDerivation);
    	LOGGER.info("Instantiated derivation with time series <" + createdDerivation + ">");
    	LOGGER.debug("<" + entities + "> belongsTo <" + createdDerivation + ">");
    	LOGGER.debug("<" + createdDerivation + "> isDerivedFrom <" + inputsIRI + ">");
    	LOGGER.debug("<" + createdDerivation + "> isDerivedUsing <" + agentIRI + "> located at " + agentURL);
    	return createdDerivation;
    }
    
    /**
     * This method creates a new asynchronous derived instance and adds the following statements
	 * <entity> <belongsTo> <derived>, <derived> <isDerivedUsing> <agentIRI>, <derived> <isDerivedFrom> <inputsIRI>
     * Use this for asynchronous instances that get replaced by agents, also when the information about agent exists already
     * @param entities
     * @param agentIRI
     * @param inputsIRI
     * @return
     */
    public String createAsynDerivation(List<String> entities, String agentIRI, List<String> inputsIRI) {
    	String createdDerivation = DerivationSparql.createDerivationAsyn(this.kbClient, entities, agentIRI, inputsIRI);
    	DerivationSparql.addTimeInstance(this.kbClient, createdDerivation);
    	LOGGER.info("Instantiated asynchronous derivation <" + createdDerivation + ">");
    	LOGGER.debug("<" + entities + "> belongsTo <" + createdDerivation + ">");
    	LOGGER.debug("<" + createdDerivation + "> isDerivedFrom <" + inputsIRI + ">");
    	LOGGER.debug("<" + createdDerivation + "> isDerivedUsing <" + agentIRI + ">");
    	return createdDerivation;
    }
    
    /**
     * adds a timestamp to your input following the w3c standard for unix timestamp https://www.w3.org/TR/owl-time/
     * <entity> <hasTime> <time>, <time> <numericPosition> 123
     * @param entity
     */
    public void addTimeInstance(String entity) {
    	DerivationSparql.addTimeInstance(kbClient, entity);
    	LOGGER.info("Added timestamp to <" + entity + ">");
    }
    
    /**
     * removes time instance added using addTimeInstance
     * @param entity
     */
    public void removeTimeInstance(String entity) {
    	DerivationSparql.removeTimeInstance(kbClient, entity);
    	LOGGER.info("Removed timestamp for <" + entity + ">");
    }
    
    /**
     * you may want to use this to update an input's timestamp, the DerivationClient does not deal with inputs directly
     */
    public void updateTimestamp(String entity) {
    	DerivationSparql.updateTimeStamp(kbClient, entity);
    	LOGGER.info("Updated timestamp of <" + entity + ">");
    }
    
    /**
	 * makes sure the given instance is up-to-date by comparing its timestamp to all of its inputs
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
			LOGGER.fatal(e.getMessage());
			throw new JPSRuntimeException(e);
		}
	}
	
	/**
	 * This method checks and makes sure the derived instance is up-to-date by comparing the timestamp
	 * of the derivation to all of its inputs.
	 * @param derivationIRI
	 */
	public void updateDerivationAsyn(String derivationIRI) {
		// the graph object makes sure that there is no circular dependency
		DirectedAcyclicGraph<String,DefaultEdge> graph = new DirectedAcyclicGraph<String,DefaultEdge>(DefaultEdge.class);
		try {
			updateDerivationAsyn(derivationIRI, graph);
		} catch (Exception e) {
			LOGGER.fatal(e.getMessage());
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
			LOGGER.warn(e.getMessage());
		    throw new JPSRuntimeException(e);
		}
	}
	
	public void monitorDerivation(String agentIRI, SetupJobInterface setupJob) {
		List<String> listOfDerivation = DerivationSparql.getDerivations(this.kbClient, agentIRI);
		
		for (String derivation : listOfDerivation) {
			// check if the derivation is an instance of asynchronous derivation
			if (DerivationSparql.isDerivedAsynchronous(this.kbClient, derivation)) {
				if (DerivationSparql.isRequested(this.kbClient, derivation)) {
					JSONObject agentInputs = new JSONObject();
					agentInputs.put(AGENT_INPUT_KEY, DerivationSparql.getInputsMapToAgent(this.kbClient, derivation, agentIRI));
					DerivationSparql.markAsInProgress(this.kbClient, derivation);
					List<String> newDerivedIRI = setupJob.setupJob(agentInputs);
					updateStatusAtJobCompletion(derivation, newDerivedIRI);
				} else if (DerivationSparql.isInProgress(this.kbClient, derivation)) {
					// at the moment the design is the agent just pass when it's detected as "InProgress"
				} else if (DerivationSparql.isFinished(this.kbClient, derivation)) {
					cleanUpFinishedDerivationUpdate(derivation);
				}
			} else {
				// TODO ideally this should call the update or other functions in synchronous derivation function
				LOGGER.info("Derivation instance <" + derivation + "> is not an asynchronous derivation.");
			}
		}
    }
	
//	
//	public void monitorDerivation(String agentIRI) {
//		// follow the updateDerivation method, the graph object makes sure that there is no circular dependency
//		DirectedAcyclicGraph<String,DefaultEdge> graph = new DirectedAcyclicGraph<String,DefaultEdge>(DefaultEdge.class);
//		try {
//			monitorDerivation(agentIRI, graph);
//		} catch (Exception e) {
//			LOGGER.fatal(e.getMessage());
//			throw new JPSRuntimeException(e);
//		}
//	}
	
	/**
	 * All private functions below
	 */
	
	
//	private void monitorDerivation(String agentIRI, DirectedAcyclicGraph<String, DefaultEdge> graph) {
//		// as a first step, we assume one agent only responsible for monitoring one derivation instance
//
//		// query ?derivation <isDerivedUsing> <agentIRI> -- get all derivation that is derived using a given agentIRI
//		List<String> derivations = DerivationSparql.getDerivations(this.kbClient, agentIRI);
//		
//		//  each derivation instance isDerivedUsing the given agentIRI asynchronously
//		for (String instance : derivations) {
//			if (DerivationSparql.isRequested(this.kbClient, instance)) {
//				// here we should setup a job, and mark it as InProgress
//			} else if (DerivationSparql.isInProgress(this.kbClient, instance)) {
//				// here we will just pass for the first iterations
//			} else if (DerivationSparql.isFinished(this.kbClient, instance)) {
//				// here we will reconnect derivation and clean up status etc. 
//			}
//		}
//		
//		
//		
//		// check isRequested --> HTTP request to itself, if 
//		// check inProgress --> check isOutOfDate
//		// 2. query ?derived <belongsTo> ?derivation -- get all derived information of the derivation
//		// 3. query ?derivation <isDerivedFrom> ?inputs -- get all inputs of the derivation
//		// 4. check isOutOfDate(?derived,?inputs) --> if so, mark as isRequested; if inProgress, pass; 
////		DerivationSparql.deleteStatus when !isOutOfDate
//		
//	}
	
	/**
	 * This method marks the derivation as "Requested" when it detects a derivation is outdated. 
	 * @param instance
	 * @param graph
	 */
	private void updateDerivationAsyn(String instance, DirectedAcyclicGraph<String, DefaultEdge> graph) {
		// this method follows the first a few steps of method updateDerivation(String instance, DirectedAcyclicGraph<String, DefaultEdge> graph)
		// TODO in future development, ideally these two method should be merged into the same method?
		List<String> inputsAndDerived = DerivationSparql.getInputsAndDerived(this.kbClient, instance);
		
		if (!graph.containsVertex(instance)) {
			graph.addVertex(instance);
		}
		
		for (String input : inputsAndDerived) {
			if (!graph.containsVertex(input)) {
				graph.addVertex(input);
			}
			graph.addEdge(instance, input);
			updateDerivationAsyn(input, graph);
		}
		
		List<String> inputs = DerivationSparql.getInputs(this.kbClient, instance);
		if (inputs.size() > 0) {
			if (isOutOfDate(instance,inputs)) {
				// if the Derivation is out of date, the first thing we do is checking its status
				if (!DerivationSparql.hasStatus(this.kbClient, instance)) {
					// if there's no status, then mark as requested - a job need to be started to update the derivation
					LOGGER.info("Updating <" + instance + ">, marked as Requested");
					LOGGER.debug("<" + instance + "> is out-of-date when compared to <" + inputs + ">");
					DerivationSparql.markAsRequested(this.kbClient, instance);
				} else {
					// for now, if there's any status, the derivation framework just pass
				}
			} else {
				// if the derivation is up to date, then delete <hasStatus> <Status> if applies
				if (DerivationSparql.hasStatus(this.kbClient, instance)) {
					DerivationSparql.deleteStatus(this.kbClient, instance);
				}
			}
		}
	}
	
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
		List<String> inputs = DerivationSparql.getInputs(this.kbClient, instance);
		if (inputs.size() > 0) {
			// at this point, "instance" is a derived instance for sure, any other instances will not go through this code
			// getInputs queries for <instance> <isDerivedFrom> ?x
			if (isOutOfDate(instance,inputs)) {
				LOGGER.info("Updating <" + instance + ">");
				LOGGER.debug("<" + instance + "> is out-of-date when compared to <" + inputs + ">");
				// calling agent to create a new instance
				String agentURL = DerivationSparql.getAgentUrl(kbClient, instance);
				JSONObject requestParams = new JSONObject();
				JSONArray iris = new JSONArray(inputs);
				requestParams.put(AGENT_INPUT_KEY, iris);
				requestParams.put(DERIVATION_KEY, instance);
				
				LOGGER.debug("Updating <" + instance + "> using agent at <" + agentURL + "> with http request " + requestParams);
				String response = AgentCaller.executeGetWithURLAndJSON(agentURL, requestParams.toString());
				
				LOGGER.debug("Obtained http response from agent: " + response);
				
				// if it is a derived quantity with time series, there will be no changes to the instances
				if (!DerivationSparql.isDerivedWithTimeSeries(this.kbClient, instance)) {
					// collect new instances created by agent
					List<String> newEntities = new JSONObject(response).getJSONArray(AGENT_OUTPUT_KEY).toList()
							.stream().map(iri -> (String) iri).collect(Collectors.toList());

					// get all the other entities linked to the derived quantity, to be deleted and replaced with new entities
					// query for ?x <belongsTo> <instance>
					List<String> entities = DerivationSparql.getDerivedEntities(kbClient, instance);
					
					// check if any of the old entities is an input for another derived quantity
					// query ?x <isDerivedFrom> <entity>, <entity> a ?y
					// where ?x = a derived instance, ?y = class of entity
					// index 0 = derivedIRIs list, index 1 = type IRI list
					List<List<String>> derivedAndType = DerivationSparql.getIsDerivedFromEntities(kbClient, entities);
					
					// delete old instances
					DerivationSparql.deleteInstances(kbClient, entities);
					LOGGER.debug("Deleted old instances: " + Arrays.asList(entities));
					
					// link new entities to derived instance, adding ?x <belongsTo> <instance>
					DerivationSparql.addNewEntitiesToDerived(kbClient, instance, newEntities);
					LOGGER.debug("Added new instances <" + newEntities + "> to the derivation <" + instance + ">");
					
					if (derivedAndType.get(0).size() > 0) {
						LOGGER.debug("This derivation contains at least one entity which is an input to another derivation");
						LOGGER.debug("Relinking new instance(s) to the derivation by matching their rdf:type");
						// after deleting the old entity, we need to make sure that it remains linked to the appropriate derived instance
						List<String> classOfNewEntities = DerivationSparql.getInstanceClass(kbClient, newEntities);
						
						// look for the entity with the same rdf:type that we need to reconnect
						List<String> oldDerivedList = derivedAndType.get(0);
						List<String> oldTypeList = derivedAndType.get(1);
				
						// for each instance in the old derived instance that is connected to another derived instance, reconnect it
						for (int i = 0; i < oldDerivedList.size(); i++) {
							LOGGER.debug("Searching within <" + newEntities + "> with rdf:type <" + oldTypeList.get(i) + ">");
							// index in the new array with the matching type
							Integer matchingIndex = null;
							for (int j = 0; j < classOfNewEntities.size(); j++) {
								if (classOfNewEntities.get(j).contentEquals(oldTypeList.get(i))) {
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
							DerivationSparql.reconnectInputToDerived(kbClient, newEntities.get(matchingIndex), oldDerivedList.get(i));
						}
					}
				}
				// if there are no errors, assume update is successful
				DerivationSparql.updateTimeStamp(kbClient, instance);
				LOGGER.info("Updated timestamp of <" + instance + ">");
			}
		}
	}
	
	/**
	 * This method updates the status of the Derivation at job completion: the status of the derivation will be marked as "Finished" and the newDerivedIRI will be attached to the status. 
	 * @param derivation
	 * @param newDerivedIRI
	 */
	private void updateStatusAtJobCompletion(String derivation, List<String> newDerivedIRI) {
		// mark as Finished
		String statusIRI = DerivationSparql.markAsFinished(this.kbClient, derivation);
		// add newDerivedIRI to Finished status
		DerivationSparql.addNewDerivedIRIToFinishedStatus(this.kbClient, statusIRI, newDerivedIRI);
	}
	
	/**
	 * This method cleans up the "Finished" derivation by reconnecting the new generated derived IRI with derivations and deleting all status. 
	 * @param derivation
	 */
	private void cleanUpFinishedDerivationUpdate(String derivation) {
		// this method largely follows the part of code after obtaining the response from Agent in method updateDerivation(String instance, DirectedAcyclicGraph<String,DefaultEdge> graph)
		// the additional part in this method (compared to the above mentioned method) is: (1) how we get newDerivedIRI; (2) we delete all triples connected to the status of the derivation
		// in the future development, there's a potential these two methods can be merged into one
		
		// (1) get newDerivedIRI
		List<String> newEntities = DerivationSparql.getNewDerivedIRI(this.kbClient, derivation);
		
		// get all the other entities linked to the derived quantity, to be deleted and replaced with new entities
		// query for ?x <belongsTo> <instance>
		List<String> entities = DerivationSparql.getDerivedEntities(kbClient, derivation);
		
		// check if any of the old entities is an input for another derived quantity
		// query ?x <isDerivedFrom> <entity>, <entity> a ?y
		// where ?x = a derived instance, ?y = class of entity
		// index 0 = derivedIRIs list, index 1 = type IRI list
		List<List<String>> derivedAndType = DerivationSparql.getIsDerivedFromEntities(kbClient, entities);
		
		// delete old instances
		DerivationSparql.deleteInstances(kbClient, entities);
		LOGGER.debug("Deleted old instances: " + Arrays.asList(entities));
		
		// link new entities to derived instance, adding ?x <belongsTo> <instance>
		DerivationSparql.addNewEntitiesToDerived(kbClient, derivation, newEntities);
		LOGGER.debug("Added new instances <" + newEntities + "> to the derivation <" + derivation + ">");
		
		if (derivedAndType.get(0).size() > 0) {
			LOGGER.debug("This derivation contains at least one entity which is an input to another derivation");
			LOGGER.debug("Relinking new instance(s) to the derivation by matching their rdf:type");
			// after deleting the old entity, we need to make sure that it remains linked to the appropriate derived instance
			List<String> classOfNewEntities = DerivationSparql.getInstanceClass(kbClient, newEntities);
			
			// look for the entity with the same rdf:type that we need to reconnect
			List<String> oldDerivedList = derivedAndType.get(0);
			List<String> oldTypeList = derivedAndType.get(1);
	
			// for each instance in the old derived instance that is connected to another derived instance, reconnect it
			for (int i = 0; i < oldDerivedList.size(); i++) {
				LOGGER.debug("Searching within <" + newEntities + "> with rdf:type <" + oldTypeList.get(i) + ">");
				// index in the new array with the matching type
				Integer matchingIndex = null;
				for (int j = 0; j < classOfNewEntities.size(); j++) {
					if (classOfNewEntities.get(j).contentEquals(oldTypeList.get(i))) {
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
				DerivationSparql.reconnectInputToDerived(kbClient, newEntities.get(matchingIndex), oldDerivedList.get(i));
			}
		}
		
		// (2) delete all triples connected to status of the derivation
		DerivationSparql.deleteStatus(this.kbClient, derivation);
		
		// if there are no errors, assume update is successful
		DerivationSparql.updateTimeStamp(kbClient, derivation);
		LOGGER.info("Updated timestamp of <" + derivation + ">");
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
		List<String> inputs = DerivationSparql.getInputs(this.kbClient, instance);
		if (inputs.size() > 0) {
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
	private boolean isOutOfDate(String instance, List<String> inputs) {
	    boolean outOfDate = false;
	    long instanceTimestamp = DerivationSparql.getTimestamp(this.kbClient, instance);
	    
	    for (String input : inputs) {
	    	long inputTimestamp = DerivationSparql.getTimestamp(this.kbClient, input);
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

//	/**
//	 * Checks if the derivation status is "Requested".
//	 * @param derivation
//	 * @return
//	 */
//	public boolean isRequested(String derivation) {
//		return DerivationSparql.isRequested(this.kbClient, derivation);
//	}
//
//	/**
//	 * Checks if the derivation status is "InProgress".
//	 * @param derivation
//	 * @return
//	 */
//	public boolean isInProgress(String derivation) {
//		return DerivationSparql.isInProgress(this.kbClient, derivation);
//	}
//
//	/**
//	 * Checks if the derivation status is "Finished".
//	 * @param derivation
//	 * @return
//	 */
//	public boolean isFinished(String derivation) {
//		return DerivationSparql.isFinished(this.kbClient, derivation);
//	}

//	/**
//	 * Marks the derivation status as "Requested".
//	 * @param derivation
//	 */
//	public void markAsRequested(String derivation) {
//		DerivationSparql.markAsRequested(this.kbClient, derivation);
//	}
//
//	/**
//	 * Marks the derivation status as "InProgress".
//	 * @param derivation
//	 */
//	public void markAsInProgress(String derivation) {
//		DerivationSparql.markAsInProgress(this.kbClient, derivation);
//	}
//
//	/**
//	 * Marks the derivation status as "Finished".
//	 * @param derivation
//	 */
//	public void markAsFinished(String derivation) {
//		DerivationSparql.markAsFinished(this.kbClient, derivation);
//	}

	/**
	 * Checks if a derivation has status.
	 * @param derivation
	 * @return
	 */
	public boolean hasStatus(String derivation) {
		return DerivationSparql.hasStatus(this.kbClient, derivation);
	}

	/**
	 * Gets the status of a derivation.
	 * @param derivation
	 * @return
	 */
	public String getStatus(String derivation) {
		return DerivationSparql.getStatus(this.kbClient, derivation);
	}

	/**
	 * Gets the new derived IRI at derivation update (job) completion.
	 * @param derivation
	 * @return
	 */
	public List<String> getNewDerivedIRI(String derivation) {
		return DerivationSparql.getNewDerivedIRI(this.kbClient, derivation);
	}

	/**
	 * Gets the agent IRI that is used to update the derivation.
	 * @param derivedQuantity
	 * @return
	 */
	public String getAgentUrl(String derivedQuantity) {
		return DerivationSparql.getAgentUrl(this.kbClient, derivedQuantity);
	}

//	/**
//	 * Gets a list of derivations that is derived using a given agent IRI.
//	 * @param agentIRI
//	 * @return
//	 */
//	public List<String> getDerivations(String agentIRI) {
//		return DerivationSparql.getDerivations(this.kbClient, agentIRI);
//	}
}
