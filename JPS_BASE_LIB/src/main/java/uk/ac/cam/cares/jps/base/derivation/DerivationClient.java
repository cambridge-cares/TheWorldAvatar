package uk.ac.cam.cares.jps.base.derivation;

import java.time.Instant;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONArray;
import org.json.JSONObject;
import org.jgrapht.graph.DefaultEdge;
import org.jgrapht.graph.DirectedAcyclicGraph;

import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
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
	public static final String BELONGSTO_KEY = "belongsTo";
	// defines the endpoint DerivedQuantityClient should act on
	StoreClientInterface kbClient;
	DerivationSparql sparqlClient;
	
     /**
     * Logger for error output.
     */
    private static final Logger LOGGER = LogManager.getLogger(DerivationClient.class);
    
    /**
     * This constructor is tagged as @Deprecated as ideally user should provide based URL when creating derivation instances.
     * @param kbClient
     */
    @Deprecated
    public DerivationClient(StoreClientInterface kbClient) {
    	this.kbClient = kbClient;
    	this.sparqlClient = new DerivationSparql(kbClient);	
    }
    
    /**
     * This constructor should be used to enable customised derivation instance base URL.
     * @param kbClient
     * @param derivationInstanceBaseURL
     */
    public DerivationClient(StoreClientInterface kbClient, String derivationInstanceBaseURL) {
    	this.kbClient = kbClient;
    	this.sparqlClient = new DerivationSparql(kbClient, derivationInstanceBaseURL);
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
    	String createdDerivation = this.sparqlClient.createDerivation(entities, agentIRI, agentURL, inputsIRI);
    	this.sparqlClient.addTimeInstance(createdDerivation);
    	LOGGER.info("Instantiated derivation <" + createdDerivation + ">");
    	LOGGER.debug("<" + entities + "> belongsTo <" + createdDerivation + ">");
    	LOGGER.debug("<" + createdDerivation + "> isDerivedFrom <" + inputsIRI + ">");
    	LOGGER.debug("<" + createdDerivation + "> isDerivedUsing <" + agentIRI + "> located at " + agentURL);
    	return createdDerivation;
    }
    
    public List<String> bulkCreateDerivations(List<List<String>> entitiesList, List<String> agentIRIList, List<String> agentURLList, List<List<String>> inputsList) {
    	List<String> derivations = this.sparqlClient.bulkCreateDerivations(entitiesList, agentIRIList, agentURLList, inputsList);
    	LOGGER.info("Instantiated derivations " + derivations);
    	
    	// add timestamp to each derivation
    	this.sparqlClient.addTimeInstance(derivations);
    	
    	return derivations;
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
		String createdDerivation = this.sparqlClient.createDerivation(entities, agentIRI, inputsIRI);
		this.sparqlClient.addTimeInstance(createdDerivation);
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
    	String createdDerivation = this.sparqlClient.createDerivationWithTimeSeries(entities, agentIRI, agentURL, inputsIRI);
    	this.sparqlClient.addTimeInstance(createdDerivation);
    	LOGGER.info("Instantiated derivation with time series <" + createdDerivation + ">");
    	LOGGER.debug("<" + entities + "> belongsTo <" + createdDerivation + ">");
    	LOGGER.debug("<" + createdDerivation + "> isDerivedFrom <" + inputsIRI + ">");
    	LOGGER.debug("<" + createdDerivation + "> isDerivedUsing <" + agentIRI + "> located at " + agentURL);
    	return createdDerivation;
    }
    
    public List<String> bulkCreateDerivationsWithTimeSeries(List<List<String>> entitiesList, List<String> agentIRIList, List<String> agentURLList, List<List<String>> inputsList) {
    	List<String> derivations = this.sparqlClient.bulkCreateDerivationsWithTimeSeries(entitiesList, agentIRIList, agentURLList, inputsList);
    	LOGGER.info("Instantiated derivations with time series " + derivations);
    	
    	// add timestamp to each derivation
    	this.sparqlClient.addTimeInstance(derivations);
    	
    	return derivations;
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
    	String createdDerivation = this.sparqlClient.createDerivationAsyn(entities, agentIRI, inputsIRI);
    	this.sparqlClient.addTimeInstance(createdDerivation);
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
    	this.sparqlClient.addTimeInstance(entity);
    	LOGGER.info("Added timestamp to <" + entity + ">");
    }
    
    /**
     * same method as above but in bulk
     * @param entities
     */
    public void addTimeInstance(List<String> entities) {
    	this.sparqlClient.addTimeInstance(entities);
    	LOGGER.info("Added timestamps to <" + entities + ">");
    }
    
    /**
     * manually update the timestamps of pure inputs or derivations
     * entity can be a derivation or a pure input
     * @param entities
     */
    public void updateTimestamps(List<String> entities) {
    	// if the given entity is part of a derivation, update the derivation instead
    	Map<String,String> entityDerivationMap = this.sparqlClient.getDerivationsOf(entities);
    	Map<String,Long> timestamp_map = new HashMap<>();
    	long currentTime = Instant.now().getEpochSecond();
    	for (String entity : entities) {
    		if (entityDerivationMap.containsKey(entity)) {
    			// belongs to a derivation, update timestamp of derivation
    			timestamp_map.put(entityDerivationMap.get(entity), currentTime);
    		} else {
    			// assume this is a pure input, if this does not exist  
    			// nothing should happen
    			timestamp_map.put(entity, currentTime);
    		}
    	}
    	this.sparqlClient.updateTimestamps(timestamp_map);
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
	 * makes sure the given instances are up-to-date by comparing their timestamps
	 * to all of their inputs. The input, derivedIRIs, should have rdf:type
	 * DerivedQuantity or DerivedQuantityWithTimeSeries
	 * 
	 * @param kbClient
	 * @param derivedIRI
	 */
	public void updateDerivations(List<String> derivedIRIs) {
		// the graph object makes sure that there is no circular dependency
		DirectedAcyclicGraph<String, DefaultEdge> graph = new DirectedAcyclicGraph<>(DefaultEdge.class);
		List<Derivation> derivations = this.sparqlClient.getDerivations();
		try {
			for (String derivedIRI : derivedIRIs) {
				Derivation derivation = derivations.stream().filter(d -> d.getIri().equals(derivedIRI)).findFirst().get();
				updateDerivation(derivation, graph);
			}
			
			// update timestamps in KG
			Map<String, Long> derivationTime_map = new HashMap<>();
			for (Derivation derivation : derivations) {
				if (derivation.getUpdateStatus()) {
					derivationTime_map.put(derivation.getIri(), derivation.getTimestamp());
				}
			}
			this.sparqlClient.updateTimestamps(derivationTime_map);
		} catch (Exception e) {
			LOGGER.fatal(e.getMessage());
			throw new JPSRuntimeException(e);
		}
	}

	/**
	 * updates all derivations in the triple-store
	 */
	public void updateDerivations() {
		List<Derivation> derivations = this.sparqlClient.getDerivations();
		
		// find derivations with entities that are not input of anything (the top nodes)
		List<Derivation> topNodes = new ArrayList<>();
		for (Derivation derivation : derivations) {
			// all entities need to match the condition
			if (derivation.getEntities().stream().allMatch(e -> !e.isInputToDerivation())) {
				topNodes.add(derivation);
			}
		}
		
		// the graph object makes sure that there is no circular dependency
		DirectedAcyclicGraph<String, DefaultEdge> graph = new DirectedAcyclicGraph<>(DefaultEdge.class);
		try {
			for (Derivation derivation : topNodes) {
				updateDerivation(derivation, graph);
			}
			
			// update timestamps in kg
			Map<String, Long> derivationTime_map = new HashMap<>();
			for (Derivation derivation : derivations) {
				if (derivation.getUpdateStatus()) {
					derivationTime_map.put(derivation.getIri(), derivation.getTimestamp());
				}
			}
			this.sparqlClient.updateTimestamps(derivationTime_map);
			
		} catch (Exception e) {
			LOGGER.fatal(e.getMessage());
			throw new JPSRuntimeException(e);
		}
	}
	
	/**
	 * This checks for any circular dependency and ensures that all the linked
	 * inputs have a suitable timestamp attached. This does not check for
	 * everything, e.g. instances having appropriate rdf:types, and the agent design
	 * 
	 * @param derived
	 * @return
	 */
	public boolean validateDerivations() {
		// check if any instances that should be pure inputs but part of a derivation
		if (!this.sparqlClient.validatePureInputs()) {
			throw new JPSRuntimeException("Entities belonging to a derivation should not have timestamps attached");
		}
		List<Derivation> derivations = this.sparqlClient.getDerivations();
		
		// find derivations with entities that are not input of anything (the top nodes)
		List<Derivation> topNodes = new ArrayList<>();
		for (Derivation derivation : derivations) {
			// all entities need to match the condition
			if (derivation.getEntities().stream().allMatch(e -> !e.isInputToDerivation())) {
				topNodes.add(derivation);
			}
		}
		
		// the graph object makes sure that there is no circular dependency
		DirectedAcyclicGraph<String, DefaultEdge> graph = new DirectedAcyclicGraph<>(DefaultEdge.class);
		try {
			for (Derivation derivation : topNodes) {
				validateDerivation(derivation, graph);
			}
		} catch (Exception e) {
			LOGGER.fatal(e.getMessage());
			throw new JPSRuntimeException(e);
		}
		
		return true;
	}
	
	/**
	 * This method retrieves the agent inputs that mapped against the OntoAgent I/O signature.
	 * @param derivation
	 * @param agentIRI
	 * @return
	 */
	public JSONObject retrieveAgentInputs(String derivation, String agentIRI) {
		JSONObject agentInputs = new JSONObject();
		agentInputs.put(AGENT_INPUT_KEY, this.sparqlClient.getInputsMapToAgent(derivation, agentIRI));
		return agentInputs;
	}
	
	/**
	 * drops absolutely everything
	 */
	public void dropAllDerivationsAndTimestamps() {
		dropAllDerivations();
		dropAllTimestamps();
	}
	
	/**
	 * clears all derivations from the kg, only removes timestamps directly attached
	 * to derivations, does not remove timestamps of pure inputs
	 */
	public void dropAllDerivations() {
		this.sparqlClient.dropAllDerivations();
		LOGGER.info("Dropped all derivations");
	}
	
	/**
	 * optional, removes timestamps of inputs that you added manually with addTimeInstance
	 */
	public void dropAllTimestamps() {
		this.sparqlClient.dropAllTimestamps();
		LOGGER.info("Dropped all timestamps");
	}
	
	/**
	 * This method updates the status of the Derivation at job completion: the status of the derivation will be marked as "Finished" and the newDerivedIRI will be attached to the status. 
	 * @param derivation
	 * @param newDerivedIRI
	 */
	public void updateStatusAtJobCompletion(String derivation, List<String> newDerivedIRI) {
		// mark as Finished
		String statusIRI = this.sparqlClient.markAsFinished(derivation);
		// add newDerivedIRI to Finished status
		this.sparqlClient.addNewDerivedIRIToFinishedStatus(statusIRI, newDerivedIRI);
	}
	
	/**
	 * This method cleans up the "Finished" derivation by reconnecting the new generated derived IRI with derivations and deleting all status. 
	 * @param derivation
	 */
	public void cleanUpFinishedDerivationUpdate(String derivation) {
		// this method largely follows the part of code after obtaining the response from Agent in method updateDerivation(String instance, DirectedAcyclicGraph<String,DefaultEdge> graph)
		// the additional part in this method (compared to the above mentioned method) is: (1) how we get newDerivedIRI; (2) we delete all triples connected to the status of the derivation
		// in the future development, there's a potential these two methods can be merged into one
		
		// (1) get newDerivedIRI
		List<String> newEntities = this.sparqlClient.getNewDerivedIRI(derivation);
		
		// get all the other entities linked to the derived quantity, to be deleted and replaced with new entities
		// query for ?x <belongsTo> <instance>
		List<String> entities = this.sparqlClient.getDerivedEntities(derivation);
		
		// check if any of the old entities is an input for another derived quantity
		// query ?x <isDerivedFrom> <entity>, <entity> a ?y
		// where ?x = a derived instance, ?y = class of entity
		// index 0 = derivedIRIs list, index 1 = type IRI list
		List<List<String>> derivedAndType = this.sparqlClient.getIsDerivedFromEntities(entities);
		
		// delete old instances
		this.sparqlClient.deleteInstances(entities);
		LOGGER.debug("Deleted old instances: " + Arrays.asList(entities));
		
		// link new entities to derived instance, adding ?x <belongsTo> <instance>
		this.sparqlClient.addNewEntitiesToDerived(derivation, newEntities);
		LOGGER.debug("Added new instances <" + newEntities + "> to the derivation <" + derivation + ">");
		
		if (derivedAndType.get(0).size() > 0) {
			LOGGER.debug("This derivation contains at least one entity which is an input to another derivation");
			LOGGER.debug("Relinking new instance(s) to the derivation by matching their rdf:type");
			// after deleting the old entity, we need to make sure that it remains linked to the appropriate derived instance
			List<String> classOfNewEntities = this.sparqlClient.getInstanceClass(newEntities);
			
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
				this.sparqlClient.reconnectInputToDerived(newEntities.get(matchingIndex), oldDerivedList.get(i));
			}
		}
		
		// (2) delete all triples connected to status of the derivation
		this.sparqlClient.deleteStatus(derivation);
		
		// if there are no errors, assume update is successful
		this.sparqlClient.updateTimeStamp(derivation);
		LOGGER.info("Updated timestamp of <" + derivation + ">");
	}
	
	/**
	 * Checks if the derivation is an instance of DerivationAsyn.
	 * @param derivation
	 * @return
	 */
	public boolean isDerivedAsynchronous(String derivation) {
		return this.sparqlClient.isDerivedAsynchronous(derivation);
	}

	/**
	 * Checks if the derivation status is "Requested".
	 * @param derivation
	 * @return
	 */
	public boolean isRequested(String derivation) {
		return this.sparqlClient.isRequested(derivation);
	}

	/**
	 * Checks if the derivation status is "InProgress".
	 * @param derivation
	 * @return
	 */
	public boolean isInProgress(String derivation) {
		return this.sparqlClient.isInProgress(derivation);
	}

	/**
	 * Checks if the derivation status is "Finished".
	 * @param derivation
	 * @return
	 */
	public boolean isFinished(String derivation) {
		return this.sparqlClient.isFinished(derivation);
	}

	/**
	 * Marks the derivation status as "Requested".
	 * @param derivation
	 */
	public void markAsRequested(String derivation) {
		this.sparqlClient.markAsRequested(derivation);
	}

	/**
	 * Marks the derivation status as "InProgress".
	 * @param derivation
	 */
	public void markAsInProgress(String derivation) {
		this.sparqlClient.markAsInProgress(derivation);
	}

	/**
	 * Marks the derivation status as "Finished".
	 * @param derivation
	 */
	public void markAsFinished(String derivation) {
		this.sparqlClient.markAsFinished(derivation);
	}

	/**
	 * Checks if a derivation has status.
	 * @param derivation
	 * @return
	 */
	public boolean hasStatus(String derivation) {
		return this.sparqlClient.hasStatus(derivation);
	}

	/**
	 * Gets the status of a derivation.
	 * @param derivation
	 * @return
	 */
	public String getStatus(String derivation) {
		return this.sparqlClient.getStatus(derivation);
	}

	/**
	 * Gets the new derived IRI at derivation update (job) completion.
	 * @param derivation
	 * @return
	 */
	public List<String> getNewDerivedIRI(String derivation) {
		return this.sparqlClient.getNewDerivedIRI(derivation);
	}

	/**
	 * Gets the agent IRI that is used to update the derivation.
	 * @param derivedQuantity
	 * @return
	 */
	public String getAgentUrl(String derivedQuantity) {
		return this.sparqlClient.getAgentUrl(derivedQuantity);
	}

	/**
	 * Gets a list of derivations that is derived using a given agent IRI.
	 * @param agentIRI
	 * @return
	 */
	public List<String> getDerivations(String agentIRI) {
		return this.sparqlClient.getDerivations(agentIRI);
	}
	
	/**
	 * All private functions below
	 */
	
	/**
	 * This method marks the derivation as "Requested" when it detects a derivation is outdated. 
	 * @param instance
	 * @param graph
	 */
	private void updateDerivationAsyn(String instance, DirectedAcyclicGraph<String, DefaultEdge> graph) {
		// this method follows the first a few steps of method updateDerivation(String instance, DirectedAcyclicGraph<String, DefaultEdge> graph)
		// TODO in future development, ideally these two method should be merged into the same method?
		List<String> inputsAndDerived = this.sparqlClient.getInputsAndDerived(instance);
		
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
		
		List<String> inputs = this.sparqlClient.getInputs(instance);
		if (inputs.size() > 0) {
			if (isOutOfDate(instance,inputs)) {
				// if the Derivation is out of date, the first thing we do is checking its status
				if (!this.sparqlClient.hasStatus(instance)) {
					// if there's no status, then mark as requested - a job need to be started to update the derivation
					LOGGER.info("Updating <" + instance + ">, marked as Requested");
					LOGGER.debug("<" + instance + "> is out-of-date when compared to <" + inputs + ">");
					this.sparqlClient.markAsRequested(instance);
				} else {
					// for now, if there's any status, the derivation framework just pass
				}
			} else {
				// if the derivation is up to date, then delete <hasStatus> <Status> if applies
				if (this.sparqlClient.hasStatus(instance)) {
					this.sparqlClient.deleteStatus(instance);
				}
			}
		}
	}
	
	/**
	 * called by the public function updateInstance
	 * @param instance
	 * @param derivedList
	 */
	private void updateDerivation(Derivation derivation, DirectedAcyclicGraph<String,DefaultEdge> graph) {
		// inputs that are part of another derivation (for recursive call)
		// don't need direct inputs here
		List<Derivation> inputsWithBelongsTo = derivation.getInputsWithBelongsTo();

		if (!graph.containsVertex(derivation.getIri())) {
			graph.addVertex(derivation.getIri());
		}
		
		for (Derivation input : inputsWithBelongsTo) {
			if (!graph.containsVertex(input.getIri())) {
				graph.addVertex(input.getIri());
			}
			if (null != graph.addEdge(derivation.getIri(), input.getIri())) { // will throw an error here if there is circular dependency
				// addEdge will return 'null' if the edge has already been added as DAGs can't
				// have duplicated edges so we can stop traversing this branch.
				updateDerivation(input, graph);
			}
		}

		// inputs required by the agent
		List<String> inputs = derivation.getAgentInputs();
		if (inputs.size() > 0) {
			// at this point, "instance" is a derived instance for sure, any other instances will not go through this code
			// getInputs queries for <instance> <isDerivedFrom> ?x
			if (derivation.isOutOfDate()) {
				LOGGER.info("Updating <" + derivation.getIri() + ">");
				// calling agent to create a new instance
				String agentURL = derivation.getAgentURL();
				JSONObject requestParams = new JSONObject();
				JSONArray iris = new JSONArray(inputs);
				requestParams.put(AGENT_INPUT_KEY, iris);
				requestParams.put(BELONGSTO_KEY, derivation.getEntitiesIri()); // IRIs of belongsTo
				
				LOGGER.debug("Updating <" + derivation.getIri() + "> using agent at <" + agentURL + "> with http request " + requestParams);
				String response = AgentCaller.executeGetWithURLAndJSON(agentURL, requestParams.toString());
				
				LOGGER.debug("Obtained http response from agent: " + response);
				
				// if it is a derived quantity with time series, there will be no changes to the instances
				if (!derivation.isDerivationWithTimeSeries()) {
					// collect new instances created by agent
					List<String> newEntitiesString = new JSONObject(response).getJSONArray(AGENT_OUTPUT_KEY).toList()
							.stream().map(iri -> (String) iri).collect(Collectors.toList());

					// delete old instances
					this.sparqlClient.deleteBelongsTo(derivation.getIri());
					LOGGER.debug("Deleted old instances of: " + derivation.getIri());
					
					// link new entities to derived instance, adding ?x <belongsTo> <instance>
					this.sparqlClient.addNewEntitiesToDerived(derivation.getIri(), newEntitiesString);
					LOGGER.debug("Added new instances <" + newEntitiesString + "> to the derivation <" + derivation.getIri() + ">");
					
					// entities that are input to another derivation
					List<Entity> inputToAnotherDerivation = derivation.getEntities()
							.stream().filter(e -> e.isInputToDerivation()).collect(Collectors.toList());
					
					List<Entity> newEntities = this.sparqlClient.initialiseNewEntities(newEntitiesString);
					
					if (inputToAnotherDerivation.size() > 0) {
						LOGGER.debug("This derivation contains at least one entity which is an input to another derivation");
						LOGGER.debug("Relinking new instance(s) to the derivation by matching their rdf:type");
						// after deleting the old entity, we need to make sure that it remains linked to the appropriate derived instance
						List<String> newInputs = new ArrayList<>();
						List<String> derivationsToReconnect = new ArrayList<>();
						for (Entity oldInput : inputToAnotherDerivation) {
							// find within new Entities with the same rdf:type
							List<Entity> matchingEntity = newEntities.stream().filter(e -> e.getRdfType().equals(oldInput.getRdfType())).collect(Collectors.toList());
							
							if (matchingEntity.size() != 1) {
								String errmsg = "When the agent writes new instances, make sure that there is 1 instance with matching rdf:type over the old set";
								LOGGER.error(errmsg);
								LOGGER.error("Number of matching entities = " + matchingEntity.size());
								throw new JPSRuntimeException(errmsg);
							}
							
							// update cached data
							Derivation derivationToReconnect = oldInput.getInputOf();
							derivationToReconnect.addInput(matchingEntity.get(0));
							derivationToReconnect.removeInput(oldInput);
							
							newInputs.add(matchingEntity.get(0).getIri());
							derivationsToReconnect.add(derivationToReconnect.getIri());
						}
						// update triple-store and cached data
						this.sparqlClient.reconnectInputToDerived(newInputs, derivationsToReconnect);
						derivation.replaceEntities(newEntities);
					}
				}
				// if there are no errors, assume update is successful
				long newTimestamp = Instant.now().getEpochSecond();
				derivation.setTimestamp(newTimestamp);
				derivation.setUpdateStatus(true);
			}
		}
	}
	
	private void validateDerivation(Derivation derivation, DirectedAcyclicGraph<String,DefaultEdge> graph) {
		List<Derivation> inputsWithBelongsTo = derivation.getInputsWithBelongsTo();
		
		if (!graph.containsVertex(derivation.getIri())) {
			graph.addVertex(derivation.getIri());
		}
		
		for (Derivation input : inputsWithBelongsTo) {
			if (!graph.containsVertex(input.getIri())) {
				graph.addVertex(input.getIri());
			}
			if (null != graph.addEdge(derivation.getIri(), input.getIri())) { // will throw an error here if there is circular dependency
				// addEdge will return 'null' if the edge has already been added as DAGs can't
				// have duplicated edges so we can stop traversing this branch.
				validateDerivation(input, graph);
			}
		}

		// this mainly checks for the presence of timestamp in pure inputs
		List<Entity> inputs = derivation.getInputs();
		for (Entity input : inputs) {
			if (!input.hasBelongsTo()) {
				if (input.getTimestamp() == null) {
					throw new JPSRuntimeException(input.getIri() + " does not have a timestamp");
				}
			}
		}
	}
	
	/**
	 * compares the timestamps of quantities used to derived this instance
	 * returns true if any of its input is newer
	 * @param instance
	 * @return
	 */
	@Deprecated
	private boolean isOutOfDate(String instance, List<String> inputs) {
	    boolean outOfDate = false;
	    long instanceTimestamp = this.sparqlClient.getTimestamp(instance);
	    
	    for (String input : inputs) {
	    	long inputTimestamp = this.sparqlClient.getTimestamp(input);
	    	if (inputTimestamp > instanceTimestamp) {
	    		outOfDate = true;
	    		return outOfDate;
	    	}
	    }
	    return outOfDate;
	}
}
