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
	boolean upstreamDerivationPendingUpdate;
	
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
    
    public void updateTimestamp(String entity) {
    	updateTimestamps(Arrays.asList(entity));
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
			// the flag upstreamDerivationRequested is set as false by default
			upstreamDerivationPendingUpdate = false;
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
	public JSONObject retrieveAgentInputIRIs(String derivation, String agentIRI) {
		JSONObject agentInputs = new JSONObject();
		agentInputs.put(AGENT_INPUT_KEY, this.sparqlClient.getInputsMapToAgent(derivation, agentIRI));
		
		// mark derivation status as InProgress
		// record timestamp at the point the derivation status is marked as InProgress
		this.sparqlClient.updateStatusBeforeSetupJob(derivation);
		
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
		// mark as Finished and add newDerivedIRI to Finished status
		this.sparqlClient.updateStatusAtJobCompletion(derivation, newDerivedIRI);
	}
	
	/**
	 * This method checks at the status "PendingUpdate" to decide whether change it to "Requested".
	 * @param derivation
	 */
	public List<String> checkAtPendingUpdate(String derivation) {
		// get a list of upstream derivations that need an update
		// (IMMEDIATE upstream derivations in the chain - <derivation> <isDerivedFrom>/<belongsTo> <upstreamDerivation>)
		// if all IMMEDIATE upstream derivations are up-to-date,
		// or if the derivation is the first one in the chain, this function returns empty list
		List<String> upstreamDerivationsNeedUpdate = this.sparqlClient.getUpstreamDerivationsNeedUpdate(derivation);
		
		// if the list is empty, mark this derivation as Requested
		// TODO when the list is not empty, it is possible to add more operations as now we know exactly which IMMEDIATE upstream derivation(s) need an update
		// TODO additional support to be added when detecting any upstream derivation needs an update is synchronous derivation
		if (upstreamDerivationsNeedUpdate.isEmpty()) {
			this.sparqlClient.markAsRequested(derivation);
		}

		return upstreamDerivationsNeedUpdate;
	}
	
	/**
	 * This method cleans up the "Finished" derivation by reconnecting the new generated derived IRI with derivations and deleting all status. 
	 * @param derivation
	 */
	public void cleanUpFinishedDerivationUpdate(String derivation) {
		// this method largely follows the part of code after obtaining the response from Agent in method updateDerivation(String instance, DirectedAcyclicGraph<String,DefaultEdge> graph)
		// the additional part in this method (compared to the above mentioned method) is: (1) how we get newDerivedIRI; (2) we delete all triples connected to the status of the derivation
		// in the future development, there's a potential these two methods can be merged into one
		
		// (1) get newDerivedIRI as the new instances created by agent
		List<String> newEntitiesString = this.sparqlClient.getNewDerivedIRI(derivation);
		
		// get those old entities that are inputs as other derivations
		// query for ?x <belongsTo> <derivation>; ?downstreamDerivation <isDerivedFrom> ?x.		
		// also ?x <rdf:type> ?xType; ?downstreamDerivation <rdf:type> ?preDevType.
		List<Entity> oldEntitiesAsInput = this.sparqlClient.getDerivedEntitiesAndDownstreamDerivation(derivation);
		
		// delete old instances
		// IT SHOULD BE NOTED THAT DELETION OF OLD ENTITIES SHOULD ONLY BE DONE AFTER YOU STORED THE OLD ENTITIES INTO LOCAL VARIABLE "oldEntitiesAsInput"
		this.sparqlClient.deleteBelongsTo(derivation);
		LOGGER.debug("Deleted old instances of derivation: " + derivation);
		
		// link new entities to derived instance, adding ?x <belongsTo> <instance>
		this.sparqlClient.addNewEntitiesToDerived(derivation, newEntitiesString);
		LOGGER.debug("Added new instances <" + newEntitiesString + "> to the derivation <" + derivation + ">");
		
		// create local variable for the new entities for reconnecting purpose
		List<Entity> newEntities = this.sparqlClient.initialiseNewEntities(newEntitiesString);
		
		if (oldEntitiesAsInput.size() > 0) {
			LOGGER.debug("This derivation contains at least one entity which is an input to another derivation");
			LOGGER.debug("Relinking new instance(s) to the derivation by matching their rdf:type");
			// after deleting the old entity, we need to make sure that it remains linked to the appropriate derived instance
			List<String> newInputs = new ArrayList<>();
			List<String> derivationsToReconnect = new ArrayList<>();
			for (Entity oldInput : oldEntitiesAsInput) {
				// find within new Entities with the same rdf:type
				List<Entity> matchingEntity = newEntities.stream().filter(e -> e.getRdfType().equals(oldInput.getRdfType())).collect(Collectors.toList());
				
				if (matchingEntity.size() != 1) {
					String errmsg = "When the agent writes new instances, make sure that there is 1 instance with matching rdf:type over the old set";
					LOGGER.error(errmsg);
					LOGGER.error("Number of matching entities = " + matchingEntity.size());
					throw new JPSRuntimeException(errmsg);
				}
				
				// add IRI of the matched instance and the derivation it should connect to
				newInputs.add(matchingEntity.get(0).getIri());
				derivationsToReconnect.add(oldInput.getInputOf().getIri());
			}
			// reconnect within the triple store
			this.sparqlClient.reconnectInputToDerived(newInputs, derivationsToReconnect);
		}
		
		// (2) delete all triples connected to status of the derivation
		this.sparqlClient.deleteStatus(derivation);
		
		// if there are no errors, assume update is successful
		// retrieve the recorded value in {<derivation> <retrievedInputsAt> timestamp}
		// also delete it after value retrieved
		Map<String, Long> derivationTime_map = this.sparqlClient.retrieveInputReadTimestamp(derivation);
		// update timestamp with the retrieved value
		this.sparqlClient.updateTimestamps(derivationTime_map);
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
	 * This method retrieves the status rdf:type in the format of an enum of a given derivation instance IRI.
	 * @param derivation
	 * @return
	 */
	public StatusType getStatusType(String derivation) {
		return this.sparqlClient.getStatusType(derivation);
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
	 * Gets a list of paired derivations and their status type (if applicable) that are derived using a given agent IRI.
	 * @param agentIRI
	 * @return
	 */
	public Map<String, StatusType> getDerivationsAndStatusType(String agentIRI) {
		return this.sparqlClient.getDerivationsAndStatusType(agentIRI);
	}
	
	/**
	 * This method retrieves a list of derivation instance IRI given a list of derived quantities.
	 * @param entities
	 * @return
	 */
	public Map<String,String> getDerivationsOf(List<String> entities) {
		return this.sparqlClient.getDerivationsOf(entities);
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
			if (graph.addVertex(input) && (null != graph.addEdge(instance, input))) {
				// (1) graph.addVertex(input) will try to add input as vertex if not already exist in the graph
				// (2) (null != graph.addEdge(instance, input)) will throw an error here if there is circular dependency
				// continuing... (2) addEdge will return 'null' if the edge has already been added as DAGs can't
				// continuing... (2) have duplicated edges so we can stop traversing this branch.
				// only when both (1) and (2) are true, we can update input
				// otherwise, node <D1> will be traversed multiple times if we have below chain of derivations
				// and we run updateDerivationAsyn(<D3>, graph):
				// <I3> <belongsTo> <D3> .
				// <D3> <isDerivedFrom> <I2.1> .
				// <D3> <isDerivedFrom> <I2.2> .
				// <I2.1> <belongsTo> <D2.1> .
				// <I2.2> <belongsTo> <D2.2> .
				// <D2.1> <isDerivedFrom> <I1> .
				// <D2.2> <isDerivedFrom> <I1> .
				// <I1> <belongsTo> <D1> .
				// <D1> <isDerivedFrom> <I0.1> .
				// <D1> <isDerivedFrom> <I0.2> .
				// <D1> <isDerivedFrom> <I0.3> .
				updateDerivationAsyn(input, graph);
			}
		}
		
		List<String> inputs = this.sparqlClient.getInputs(instance);
		if (inputs.size() > 0) {
			// here only derivation instance will enter, first we check if it is an asynchronous derivation
			if (isDerivedAsynchronous(instance)) {
				// we start with checking if this derivation is OutOfDate
				if (isOutOfDate(instance, inputs)) {
					// if it is OutOfDate and no status, just mark it as PendingUpdate
					// from PendingUpdate to other status will be handled from AsynAgent side
					if (!this.sparqlClient.hasStatus(instance)) {
						this.sparqlClient.markAsPendingUpdate(instance);
					}
					// set the flag to true so that other derivations will know there is one derivation upstream already PendingUpdate
					// thus they can be marked as PendingUpdate as well
					upstreamDerivationPendingUpdate = true;
				} else {
					// if the Derivation is not OutOfDate, then only consider mark it as PendingUpdate if meet all below situations
					// (1) there is upstream derivation being marked as PendingUpdate;
					// (2) this Derivation does NOT have any status, otherwise just leave it with its existing status
					if (upstreamDerivationPendingUpdate && !this.sparqlClient.hasStatus(instance)) {
						this.sparqlClient.markAsPendingUpdate(instance);
					}
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
				// record timestamp at the point the request is sent to the agent
				long newTimestamp = Instant.now().getEpochSecond();
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
