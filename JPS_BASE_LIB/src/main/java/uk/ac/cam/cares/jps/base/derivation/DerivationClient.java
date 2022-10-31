package uk.ac.cam.cares.jps.base.derivation;

import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.Stream;

import org.apache.http.HttpResponse;
import org.apache.http.ParseException;
import org.apache.http.client.ClientProtocolException;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClients;
import org.apache.http.util.EntityUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.TriplePattern;
import org.json.JSONArray;
import org.json.JSONObject;
import org.jgrapht.graph.DefaultEdge;
import org.jgrapht.graph.DirectedAcyclicGraph;

import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.interfaces.StoreClientInterface;

/**
 * this class acts as an interface to create and deal with derived quantities
 * 
 * @author Kok Foong Lee
 * @author Jiaru Bai
 *
 */
public class DerivationClient {
	// input and output of agents need to be a JSONArray consisting a list of IRIs
	// with the do
	public static final String AGENT_INPUT_KEY = "agent_input";
	public static final String AGENT_OUTPUT_KEY = "agent_output";
	public static final String BELONGSTO_KEY = "belongsTo";
	public static final String DERIVATION_KEY = "derivation";
	public static final String DERIVATION_TYPE_KEY = "derivation_rdftype";
	public static final String DOWNSTREAMDERIVATION_KEY = "downstream_derivation";
	public static final String SYNC_NEW_INFO_FLAG = "sync_new_info";
	public static final String AGENT_IRI_KEY = "agent_service_iri";

	// NOTE GET_AGENT_INPUT_PARAMS_KEY_JPSHTTPSERVLET wraps around
	// GET_AGENT_INPUT_PARAMS defined in JPSHttpServlet.java, which is used to wrap
	// the input parameters of an HTTP request to a JSONObject with the key "query",
	// ideally the key in JPSHttpServlet.java should made public so that we don't
	// need to re-implement it here
	public static final String GET_AGENT_INPUT_PARAMS_KEY_JPSHTTPSERVLET = "?query=";

	// defines the endpoint DerivedQuantityClient should act on
	StoreClientInterface kbClient;
	DerivationSparql sparqlClient;
	boolean upstreamDerivationRequested;

	/**
	 * Logger for error output.
	 */
	private static final Logger LOGGER = LogManager.getLogger(DerivationClient.class);

	/**
	 * This constructor is tagged as @Deprecated as ideally user should provide
	 * based URL when creating derivation instances.
	 * 
	 * @param kbClient
	 */
	@Deprecated
	public DerivationClient(StoreClientInterface kbClient) {
		this.kbClient = kbClient;
		this.sparqlClient = new DerivationSparql(kbClient);
	}

	/**
	 * This constructor should be used to enable customised derivation instance base
	 * URL.
	 * 
	 * @param kbClient
	 * @param derivationInstanceBaseURL
	 */
	public DerivationClient(StoreClientInterface kbClient, String derivationInstanceBaseURL) {
		this.kbClient = kbClient;
		this.sparqlClient = new DerivationSparql(kbClient, derivationInstanceBaseURL);
	}

	/**
	 * This creates a new derived instance and adds the following statements
	 * <entity> <belongsTo> <derived>, <derived> <isDerivedUsing> <agentIRI>,
	 * <agentIRI> <hasHttpUrl> <agentURL>, <derived> <isDerivedFrom> <inputsIRI>
	 * Use this for instances that get replaced by agents
	 * 
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

	public List<String> bulkCreateDerivations(List<List<String>> entitiesList, List<String> agentIRIList,
			List<String> agentURLList, List<List<String>> inputsList) {
		List<String> derivations = this.sparqlClient.bulkCreateDerivations(entitiesList, agentIRIList, agentURLList,
				inputsList);
		LOGGER.info("Instantiated derivations " + derivations);

		// add timestamp to each derivation
		this.sparqlClient.addTimeInstance(derivations);

		// validate derivations
		// this is to prevent the potential circular dependencies in the markup added but not detected at creation
		if (!derivations.isEmpty()) {
			validateDerivations();
		}

		return derivations;
	}

	/**
	 * This method creates a new derived instance and adds the following statements
	 * <entity> <belongsTo> <derived>, <derived> <isDerivedUsing> <agentIRI>,
	 * <derived> <isDerivedFrom> <inputsIRI>
	 * Use this for instances that get replaced by agents, also when the information
	 * about agent exists already
	 * 
	 * @param entities
	 * @param agentIRI
	 * @param inputsIRI
	 * @return
	 */
	public String createDerivation(List<String> entities, String agentIRI, List<String> inputsIRI) {
		String createdDerivation = this.sparqlClient.createDerivation(entities, agentIRI, inputsIRI);
		this.sparqlClient.addTimeInstance(createdDerivation);
		LOGGER.info("Instantiated derivation <" + createdDerivation + ">");
		LOGGER.debug("<" + entities + "> belongsTo <" + createdDerivation + ">");
		LOGGER.debug("<" + createdDerivation + "> isDerivedFrom <" + inputsIRI + ">");
		LOGGER.debug("<" + createdDerivation + "> isDerivedUsing <" + agentIRI + ">");
		return createdDerivation;
	}

	/**
	 * use this if all the agent does to the instance is appending time series data,
	 * entity do not get replaced
	 * 
	 * @param entity
	 * @param agentIRI
	 * @param agentURL
	 * @param inputsIRI
	 */
	public String createDerivationWithTimeSeries(List<String> entities, String agentIRI, String agentURL,
			List<String> inputsIRI) {
		String createdDerivation = this.sparqlClient.createDerivationWithTimeSeries(entities, agentIRI, agentURL,
				inputsIRI);
		this.sparqlClient.addTimeInstance(createdDerivation);
		LOGGER.info("Instantiated derivation with time series <" + createdDerivation + ">");
		LOGGER.debug("<" + entities + "> belongsTo <" + createdDerivation + ">");
		LOGGER.debug("<" + createdDerivation + "> isDerivedFrom <" + inputsIRI + ">");
		LOGGER.debug("<" + createdDerivation + "> isDerivedUsing <" + agentIRI + "> located at " + agentURL);
		return createdDerivation;
	}

	public List<String> bulkCreateDerivationsWithTimeSeries(List<List<String>> entitiesList, List<String> agentIRIList,
			List<String> agentURLList, List<List<String>> inputsList) {
		List<String> derivations = this.sparqlClient.bulkCreateDerivationsWithTimeSeries(entitiesList, agentIRIList,
				agentURLList, inputsList);
		LOGGER.info("Instantiated derivations with time series " + derivations);

		// add timestamp to each derivation
		this.sparqlClient.addTimeInstance(derivations);

		// validate derivations
		// this is to prevent the potential circular dependencies in the markup added but not detected at creation
		if (!derivations.isEmpty()) {
			validateDerivations();
		}

		return derivations;
	}

	/**
	 * This method creates a new synchronous derived instance on spot via sending an
	 * HTTP request to the agentURL queried from the knowledge graph that associated
	 * with the given agentIRI.
	 * 
	 * @param agentIRI
	 * @param inputsIRI
	 * @param derivationType
	 * @return
	 * @throws ClientProtocolException
	 * @throws IOException
	 */
	public Derivation createSyncDerivationForNewInfo(String agentIRI, List<String> inputsIRI, String derivationType) {
		// retrieve agentURL for HTTP request
		String agentURL = this.sparqlClient.getAgentUrlGivenAgentIRI(agentIRI);
		return createSyncDerivationForNewInfo(agentIRI, agentURL, inputsIRI, derivationType);
	}

	/**
	 * This method creates a new synchronous derived instance on spot via sending an
	 * HTTP request to the provided agentURL, which user should make sure that it is
	 * associated with the provided agentIRI.
	 * 
	 * @param agentIRI
	 * @param inputsIRI
	 * @param derivationType
	 * @return
	 * @throws ClientProtocolException
	 * @throws IOException
	 */
	public Derivation createSyncDerivationForNewInfo(String agentIRI, String agentURL, List<String> inputsIRI,
			String derivationType) {
		// create a unique IRI for this new derived quantity
		String derivationIRI = this.sparqlClient.createDerivationIRI(derivationType);
		Derivation createdDerivation = new Derivation(derivationIRI, derivationType);

		// add time instance to inputs in case any of them is pure inputs but haven't got timestamp attached
		// nothing happens if the inputs are derived data, or there're already timestamps attached
		this.sparqlClient.addTimeInstanceCurrentTimestamp(inputsIRI);

		// add mapped inputs to createdDerivation
		JSONObject mappedInputs = this.sparqlClient.mapInstancesToAgentInputs(inputsIRI, agentIRI);
		Iterator<String> inputTypes = mappedInputs.keys();
		while (inputTypes.hasNext()) {
			String rdfType = inputTypes.next();
			mappedInputs.getJSONArray(rdfType).toList().stream().forEach(iri -> {
				Entity inp = new Entity((String) iri);
				inp.setRdfType(rdfType);
				createdDerivation.addInput(inp);
			});
		}

		// construct HTTP requestParams
		JSONObject requestParams = new JSONObject();
		requestParams.put(AGENT_INPUT_KEY, mappedInputs); // mapped IRIs of isDerivedFrom
		requestParams.put(DERIVATION_KEY, derivationIRI); // IRI of this derivation
		requestParams.put(DERIVATION_TYPE_KEY, derivationType); // rdf:type of this derivation
		requestParams.put(SYNC_NEW_INFO_FLAG, true); // set flag to indicate the derivation is for new info
		requestParams.put(AGENT_IRI_KEY, agentIRI); // agent IRI

		// execute HTTP request to create new information
		LOGGER.debug("Creating <" + derivationIRI + "> using agent at <" + agentURL
				+ "> with http request " + requestParams);
		HttpResponse httpResponse;
		CloseableHttpClient httpClient = HttpClients.createDefault();
		String originalRequest = agentURL + GET_AGENT_INPUT_PARAMS_KEY_JPSHTTPSERVLET + requestParams.toString();
		HttpGet httpGet;
		String response;
		try {
			httpGet = new HttpGet(
				agentURL + GET_AGENT_INPUT_PARAMS_KEY_JPSHTTPSERVLET
					+ URLEncoder.encode(requestParams.toString(), StandardCharsets.UTF_8.toString()));
			httpResponse = httpClient.execute(httpGet);
			if (httpResponse.getStatusLine().getStatusCode() != 200) {
				String msg = "Failed to update derivation <" + derivationIRI + "> with original request: "
						+ originalRequest;
				String body = EntityUtils.toString(httpResponse.getEntity());
				LOGGER.error(msg);
				throw new JPSRuntimeException(msg + " Error body: " + body);
			}
			response = EntityUtils.toString(httpResponse.getEntity());
			LOGGER.debug("Obtained http response from agent: " + response);
		} catch (Exception e) {
			e.printStackTrace();
			throw new JPSRuntimeException("Failed to update derivation <" + derivationIRI + "> with original request: "
				+ originalRequest, e);
		}

		// process the agentResponse to add the created outputs to createdDerivation
		JSONObject agentResponse = new JSONObject(response);
		Iterator<String> keys = agentResponse.getJSONObject(DerivationClient.AGENT_OUTPUT_KEY).keys();
		while (keys.hasNext()) {
			String iri = keys.next();
			Entity ne = new Entity(iri);
			ne.setRdfType(agentResponse.getJSONObject(DerivationClient.AGENT_OUTPUT_KEY).getString(iri));
			createdDerivation.addEntity(ne);
		}

		LOGGER.info("Instantiated derivation <" + createdDerivation.getIri() + "> with derivation type <"
				+ createdDerivation.getRdfType() + ">");
		LOGGER.debug("<" + createdDerivation.getEntitiesIri() + "> belongsTo <" + createdDerivation.getIri() + ">");
		LOGGER.debug("<" + createdDerivation.getIri() + "> isDerivedFrom <" + inputsIRI + ">");
		LOGGER.debug("<" + createdDerivation.getIri() + "> isDerivedUsing <" + agentIRI + ">");
		return createdDerivation;
	}

	/**
	 * This method writes triples of the new created synchronous derivation for new
	 * information (new instances) to the knowledge graph.
	 * 
	 * @param outputTriples
	 * @param entities
	 * @param agentIRI
	 * @param inputsIRI
	 * @param derivationIRI
	 * @param derivationType
	 * @param retrievedInputsAt
	 */
	public void writeSyncDerivationNewInfo(List<TriplePattern> outputTriples, List<String> entities,
			String agentIRI, List<String> inputsIRI, String derivationIRI, String derivationType,
			Long retrievedInputsAt) {
		this.sparqlClient.writeSyncDerivationNewInfo(outputTriples, entities, agentIRI, inputsIRI, derivationIRI,
				derivationType, retrievedInputsAt);
	}

	/**
	 * This method creates a new asynchronous derived instance and adds the
	 * following statements
	 * <entity> <belongsTo> <derived>, <derived> <isDerivedUsing> <agentIRI>,
	 * <derived> <isDerivedFrom> <inputsIRI>
	 * Use this for asynchronous instances that get replaced by agents, also when
	 * the information about agent exists already
	 * 
	 * @param entities
	 * @param agentIRI
	 * @param inputsIRI
	 * @return
	 */
	public String createAsyncDerivation(List<String> entities, String agentIRI, List<String> inputsIRI,
			boolean forUpdate) {
		String createdDerivation = this.sparqlClient.createDerivationAsync(entities, agentIRI, inputsIRI, forUpdate);
		this.sparqlClient.addTimeInstance(createdDerivation);
		// mark up the derivation with current timestamp
		if (!forUpdate) {
			this.sparqlClient.updateTimeStamp(createdDerivation);
		}
		LOGGER.info("Instantiated asynchronous derivation <" + createdDerivation + ">");
		LOGGER.debug("<" + entities + "> belongsTo <" + createdDerivation + ">");
		LOGGER.debug("<" + createdDerivation + "> isDerivedFrom <" + inputsIRI + ">");
		LOGGER.debug("<" + createdDerivation + "> isDerivedUsing <" + agentIRI + ">");
		return createdDerivation;
	}

	/**
	 * This method creates a new asynchronous derived instance given an existing
	 * derivation and adds the following statements
	 * <entity> <belongsTo> <derived>, <derived> <isDerivedUsing> <agentIRI>,
	 * <derived> <isDerivedFrom> <inputsIRI>
	 * Note that the <inputsIRI> to be used are actually derivation outputs
	 * retrieved from the given derivation.
	 * Use this for asynchronous instances that get replaced by agents, also when
	 * the information about agent exists already
	 * 
	 * @param entities
	 * @param agentIRI
	 * @param derivation
	 * @param forUpdate
	 * @return
	 */
	public String createAsyncDerivation(List<String> entities, String agentIRI, String derivation, boolean forUpdate) {
		// first retrieve a list of inputs
		List<String> inputsIRI = this.sparqlClient.retrieveMatchingInstances(derivation, agentIRI);
		// then create asynchronous derivation as usual
		return createAsyncDerivation(entities, agentIRI, inputsIRI, forUpdate);
	}

	public String createAsyncDerivationForNewInfo(String agentIRI, List<String> inputsAndDerivations) {
		return createAsyncDerivation(new ArrayList<>(), agentIRI, inputsAndDerivations, true);
	}

	public List<String> bulkCreateAsyncDerivations(List<List<String>> entitiesList, List<String> agentIRIList,
			List<List<String>> inputsList, List<Boolean> forUpdateFlagList) {
		List<String> derivations = this.sparqlClient.bulkCreateDerivationsAsync(entitiesList, agentIRIList, inputsList, forUpdateFlagList);
		LOGGER.info("Instantiated asynchronous derivations " + derivations);

		// add timestamp to each derivation
		this.sparqlClient.addTimeInstance(derivations);

		// mark up the derivation with current timestamp for those that not created for async update
		// i.e. those created for markup
		for (int i = 0; i < derivations.size(); i++) {
			if (!forUpdateFlagList.get(i)) {
				this.sparqlClient.updateTimeStamp(derivations.get(i));
			}
		}

		// validate derivations
		// this is to prevent the potential circular dependencies in the markup added but not detected at creation
		if (!derivations.isEmpty()) {
			validateDerivations();
		}

		return derivations;
	}

	public List<String> bulkCreateAsyncDerivationsForNewInfo(List<String> agentIRIList, List<List<String>> inputsAndDerivationsList) {
		List<List<String>> entitiesList = IntStream.range(0, agentIRIList.size()).mapToObj(i -> new ArrayList<String>())
				.collect(Collectors.toList());
		List<Boolean> forAsyncUpdateFlagList = IntStream.range(0, entitiesList.size()).mapToObj(i -> true)
				.collect(Collectors.toList());

		List<String> derivations = this.sparqlClient.bulkCreateDerivationsAsync(entitiesList, agentIRIList, inputsAndDerivationsList,
				forAsyncUpdateFlagList);
		LOGGER.info("Instantiated asynchronous derivations " + derivations);

		// add timestamp to each derivation
		this.sparqlClient.addTimeInstance(derivations);

		// validate derivations
		// this is to prevent the potential circular dependencies in the markup added but not detected at creation
		if (!derivations.isEmpty()) {
			validateDerivations();
		}

		return derivations;
	}

	/**
	 * adds a timestamp to your input following the w3c standard for unix timestamp
	 * https://www.w3.org/TR/owl-time/
	 * <entity> <hasTime> <time>, <time> <numericPosition> 123
	 * 
	 * @param entity
	 */
	public void addTimeInstance(String entity) {
		// calls the method that adds timestamp in bulk
		addTimeInstance(Arrays.asList(entity));
		LOGGER.info("Added timestamp to <" + entity + "> if it doesn't have a timestamp already");
	}

	/**
	 * same method as above but in bulk
	 * 
	 * @param entities
	 */
	public void addTimeInstance(List<String> entities) {
		this.sparqlClient.addTimeInstance(entities);
		LOGGER.info("Added timestamps to <" + entities + "> if they don't have a timestamp already");
	}

	/**
	 * adds a timestamp to your input following the w3c standard for unix timestamp
	 * https://www.w3.org/TR/owl-time/
	 * <entity> <hasTime> <time>, <time> <numericPosition> currentTimestamp
	 *
	 * @param entity
	 */
	public void addTimeInstanceCurrentTimestamp(String entity) {
		addTimeInstanceCurrentTimestamp(Arrays.asList(entity));
		LOGGER.info("Added time instances with current timestamps to <" + entity + "> if it doesn't have a timestamp already");
	}

	/**
	 * same method as above but in bulk
	 *
	 * @param entities
	 */
	public void addTimeInstanceCurrentTimestamp(List<String> entities) {
		this.sparqlClient.addTimeInstanceCurrentTimestamp(entities);
		LOGGER.info("Added time instances with current timestamps to <" + entities + "> if they don't have a timestamp already");
	}

	/**
	 * manually update the timestamps of pure inputs or derivations
	 * entity can be a derivation or a pure input
	 * 
	 * @param entities
	 */
	public void updateTimestamps(List<String> entities) {
		// if the given entity is part of a derivation, update the derivation instead
		Map<String, String> entityDerivationMap = this.sparqlClient.getDerivationsOf(entities);
		Map<String, Long> timestampMap = new HashMap<>();
		long currentTime = Instant.now().getEpochSecond();
		for (String entity : entities) {
			if (entityDerivationMap.containsKey(entity)) {
				// belongs to a derivation, update timestamp of derivation
				timestampMap.put(entityDerivationMap.get(entity), currentTime);
			} else {
				// assume this is a pure input, if this does not exist
				// nothing should happen
				timestampMap.put(entity, currentTime);
			}
		}
		this.sparqlClient.updateTimestamps(timestampMap);
	}

	public void updateTimestamp(String entity) {
		updateTimestamps(Arrays.asList(entity));
	}

	/**
	 * This method checks and makes sure the derived instance is up-to-date by
	 * comparing the timestamp
	 * of the derivation to all of its inputs.
	 * 
	 * @param derivationIRI
	 */
	@Deprecated
	public void updateDerivationAsyn(String derivationIRI) {
		// the graph object makes sure that there is no circular dependency
		DirectedAcyclicGraph<String, DefaultEdge> graph = new DirectedAcyclicGraph<String, DefaultEdge>(
				DefaultEdge.class);
		try {
			// the flag upstreamDerivationRequested is set as false by default
			upstreamDerivationRequested = false;
			updateDerivationAsyn(derivationIRI, graph);
		} catch (Exception e) {
			LOGGER.fatal(e.getMessage());
			throw new JPSRuntimeException(e);
		}
	}

	/**
	 * This method updates a DAG of pure asynchronous derivations or asynchronous
	 * derivations depending on synchronous derivations.
	 * 
	 * @param derivationIRI
	 */
	public void updateMixedAsyncDerivation(String derivationIRI) {
		// the graph object makes sure that there is no circular dependency
		DirectedAcyclicGraph<String, DefaultEdge> graph = new DirectedAcyclicGraph<String, DefaultEdge>(
				DefaultEdge.class);
		Derivation derivation = this.sparqlClient.getDerivation(derivationIRI);
		try {
			updateMixedAsyncDerivation(derivation, graph);
		} catch (Exception e) {
			LOGGER.fatal(e.getMessage());
			throw new JPSRuntimeException(e);
		}
	}

	/**
	 * This method updates the given and its upstream pure synchronous derivations.
	 * 
	 * @param derivationIRI
	 */
	public void updatePureSyncDerivation(String derivationIRI) {
		updatePureSyncDerivations(Arrays.asList(derivationIRI));
	}

	/**
	 * This method updates the list of given and their upstream pure synchronous
	 * derivations.
	 * 
	 * @param derivationIRI
	 */
	public void updatePureSyncDerivations(List<String> derivationIRIs) {
		// the graph object makes sure that there is no circular dependency
		DirectedAcyclicGraph<String, DefaultEdge> graph = new DirectedAcyclicGraph<String, DefaultEdge>(
				DefaultEdge.class);
		List<Derivation> derivations = this.sparqlClient.getAllDerivationsInKG();
		try {
			for (String derivationIRI : derivationIRIs) {
				Derivation derivation = derivations.stream().filter(d -> d.getIri().equals(derivationIRI)).findFirst()
						.get();
				updatePureSyncDerivation(derivation, graph);
			}
		} catch (Exception e) {
			LOGGER.fatal(e.getMessage());
			throw new JPSRuntimeException(e);
		}
	}

	/**
	 * This method updates all synchronous derivations in the knowledge graph.
	 */
	public void updateAllSyncDerivations() {
		List<Derivation> derivations = this.sparqlClient.getAllDerivationsInKG();

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
				updatePureSyncDerivation(derivation, graph);
			}
		} catch (Exception e) {
			LOGGER.fatal(e.getMessage());
			throw new JPSRuntimeException(e);
		}
	}

	/**
	 * This method is a wrapper method of updateMixedAsyncDerivation(String
	 * derivationIRI) and updatePureSyncDerivation(String derivationIRI) that
	 * updates the given derivationIRI regardless its rdf:type.
	 * 
	 * @param derivationIRI
	 */
	public void unifiedUpdateDerivation(String derivationIRI) {
		// depend on the rdf:type of the root derivationIRI, different method is
		// triggered
		try {
			if (isDerivedAsynchronous(derivationIRI)) {
				// update pure async or async depend on sync derivations
				updateMixedAsyncDerivation(derivationIRI);
			} else {
				// update pure sync derivations
				updatePureSyncDerivations(Arrays.asList(derivationIRI));
			}
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
	 * NOTE this method is marked as Deprecated as it updates the timestamp of the
	 * whole derivation DAG in one-go after all the update is finished, which
	 * restricts the amount of entities that requesting the same information at the
	 * same time, as we prefer a decentralised deployment of agents, the amount of
	 * entities that require an update should NOT be confined. The new method
	 * updatePureSyncDerivations(List<String>) is recommended as an alternative.
	 * 
	 * @param kbClient
	 * @param derivedIRI
	 */
	@Deprecated
	public void updateDerivations(List<String> derivedIRIs) {
		// the graph object makes sure that there is no circular dependency
		DirectedAcyclicGraph<String, DefaultEdge> graph = new DirectedAcyclicGraph<>(DefaultEdge.class);
		List<Derivation> derivations = this.sparqlClient.getDerivations();
		try {
			for (String derivedIRI : derivedIRIs) {
				Derivation derivation = derivations.stream().filter(d -> d.getIri().equals(derivedIRI)).findFirst()
						.get();
				updateDerivation(derivation, graph);
			}

			// update timestamps in KG
			Map<String, Long> derivationTimeMap = new HashMap<>();
			for (Derivation derivation : derivations) {
				if (derivation.getUpdateStatus()) {
					derivationTimeMap.put(derivation.getIri(), derivation.getTimestamp());
				}
			}
			this.sparqlClient.updateTimestamps(derivationTimeMap);
		} catch (Exception e) {
			LOGGER.fatal(e.getMessage());
			throw new JPSRuntimeException(e);
		}
	}

	/**
	 * updates all derivations in the triple-store
	 * 
	 * * NOTE this method is marked as Deprecated as it updates the timestamp of the
	 * whole derivation DAG in one-go after all the update is finished, which
	 * restricts the amount of entities that requesting the same information at the
	 * same time, as we prefer a decentralised deployment of agents, the amount of
	 * entities that require an update should NOT be confined. The new method
	 * updateAllSyncDerivations() is recommended as an alternative.
	 * 
	 */
	@Deprecated
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
			Map<String, Long> derivationTimeMap = new HashMap<>();
			for (Derivation derivation : derivations) {
				if (derivation.getUpdateStatus()) {
					derivationTimeMap.put(derivation.getIri(), derivation.getTimestamp());
				}
			}
			this.sparqlClient.updateTimestamps(derivationTimeMap);

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
		List<Derivation> derivations = this.sparqlClient.getAllDerivationsInKG();

		// find derivations (the top nodes) if it meet below criteria
		// (1) with entities that are not input of anything
		// (2) don't have any outputs (entities), also no directedDownstream derivations
		List<Derivation> topNodes = new ArrayList<>();
		for (Derivation derivation : derivations) {
			// if derivation has entities, then all entities need to match the condition
			if (!derivation.getEntities().isEmpty()) {
				if (derivation.getEntities().stream().allMatch(e -> !e.isInputToDerivation())) {
					topNodes.add(derivation);
				}
			} else {
				// if the derivation doesn't have entities (outputs), then it MUST not
				// have any directedDownstream derivations
				if (derivation.getDirectedDownstreams().isEmpty()) {
					topNodes.add(derivation);
				}
			}
		}

		// if there are derivations exist in the triple store, but no topNodes identified
		// then it will be considered as circular dependency
		// e.g. no topNodes will be identified for below situation
		// e1 <belongsTo> d1. d1 <isDerivedFrom> i1.
		// i1 <belongsTo> d2. d2 <isDerivedFrom> e1.
		if (!derivations.isEmpty() && topNodes.isEmpty()) {
			throw new JPSRuntimeException(
				"Derivations exist in triple store but no top nodes identified. Circular dependency likely occurred.");
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
	 * This method retrieves the agent inputs that mapped against the OntoAgent I/O
	 * signature.
	 * 
	 * @param derivation
	 * @param agentIRI
	 * @return
	 */
	public JSONObject retrieveAgentInputIRIs(String derivation, String agentIRI) {
		JSONObject agentInputs = new JSONObject();
		agentInputs.put(AGENT_INPUT_KEY, this.sparqlClient.getInputsMapToAgent(derivation, agentIRI));

		return agentInputs;
	}

	public boolean updateStatusBeforeSetupJob(String derivation) {
		// mark derivation status as InProgress
		// record timestamp at the point the derivation status is marked as InProgress
		// also add uuidLock to the derivation
		// this method will return a boolean to indicate if the status update is successful
		return this.sparqlClient.updateStatusBeforeSetupJob(derivation);
	}

	/**
	 * drops absolutely everything
	 */
	public void dropAllDerivationsAndTimestamps() {
		dropAllDerivations();
		dropAllTimestamps();
	}

	/**
	 * drops absolutely everything except for triples with OntoAgent
	 */
	public void dropAllDerivationsAndTimestampsNotOntoAgent() {
		dropAllDerivationsNotOntoAgent();
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
	 * clears all derivations from the kg, only removes timestamps directly attached
	 * to derivations, does not remove timestamps of pure inputs, does not remove
	 * triples that can be part of OntoAgent
	 */
	public void dropAllDerivationsNotOntoAgent() {
		this.sparqlClient.dropAllDerivationsNotOntoAgent();
		LOGGER.info("Dropped all derivations but not OntoAgent triples");
	}

	/**
	 * optional, removes timestamps of inputs that you added manually with
	 * addTimeInstance
	 */
	public void dropAllTimestamps() {
		this.sparqlClient.dropAllTimestamps();
		LOGGER.info("Dropped all timestamps");
	}

	/**
	 * This method updates the status of the Derivation at job completion: the
	 * status of the derivation will be marked as "Finished" and the newDerivedIRI
	 * will be attached to the status.
	 * 
	 * @param derivation
	 * @param newDerivedIRI
	 */
	public void updateStatusAtJobCompletion(String derivation, List<String> newDerivedIRI,
			List<TriplePattern> newTriples) {
		// mark as Finished and add newDerivedIRI to Finished status
		// also delete the uuidLock
		this.sparqlClient.updateStatusAtJobCompletion(derivation, newDerivedIRI, newTriples);
	}

	/**
	 * This method checks at the status "Requested" to see if any immediate upstream
	 * derivations are yet to be updated.
	 * 
	 * @param derivation
	 */
	public Map<String, List<String>> checkImmediateUpstreamDerivation(String derivation) {
		// get a list of immediate upstream derivations that need an update
		// (IMMEDIATE upstream derivations in the chain - <derivation>
		// <isDerivedFrom>/<belongsTo> <upstreamDerivation>)
		// if all IMMEDIATE upstream derivations are up-to-date,
		// or if the derivation is the first one in the chain, this function returns
		// empty list
		// TODO when the list is not empty, it is possible to add more operations as now
		// we know exactly which IMMEDIATE upstream derivation(s) need an update
		// TODO additional support to be added when detecting any upstream derivation
		// needs an update is synchronous derivation
		Map<String, List<String>> upstreamDerivationsNeedUpdate = this.sparqlClient
				.getUpstreamDerivationsNeedUpdate(derivation);
		return upstreamDerivationsNeedUpdate;
	}

	public List<String> groupSyncDerivationsToUpdate(Map<String, List<String>> derivationsToUpdate) {
		List<String> syncDerivations = new ArrayList<>();
		if (!derivationsToUpdate.isEmpty()) {
			for (String rdfType : Arrays.asList(DerivationSparql.ONTODERIVATION_DERIVATION,
					DerivationSparql.ONTODERIVATION_DERIVATIONWITHTIMESERIES)) {
				if (derivationsToUpdate.containsKey(rdfType)) {
					syncDerivations.addAll(derivationsToUpdate.get(rdfType));
				}
			}
		}
		return syncDerivations;
	}

	/**
	 * This method cleans up the "Finished" derivation in the knowledge graph by
	 * deleting all old instances, reconnecting the new generated derived IRI with
	 * derivations, deleting all status, and updating timestamp in one-go.
	 * 
	 * @param derivation
	 */
	public void cleanUpFinishedDerivationUpdate(String derivation) {
		// this method is similar to the part of code in DerivationAgent that updates
		// the updated synchronous derivations by first matching the old-new instances
		// and then calling reconnectNewDerivedIRIs(List<TriplePattern>
		// outputTriples, Map<String, List<String>> newIriDownstreamDerivationMap,
		// String derivation, Long retrievedInputsAt)

		// in the future development, there's a potential these two methods can be
		// merged into one

		// four stages of operations:
		// 1. query the finished derivation and all its immediate downstream derivations
		// (including the status and new derived IRIs)
		Derivation finishedDerivation = this.sparqlClient.getDerivationWithImmediateDownstream(derivation);
		// create list of old and new instances
		List<Entity> oldEntitiesAsInput = finishedDerivation.getEntities().stream().filter(e -> e.isInputToDerivation())
				.collect(Collectors.toList());
		List<Entity> newEntities = finishedDerivation.getStatus().getNewDerivedIRI();
		// initialise a map with all new derived IRIs as key and an empty list
		Map<String, List<String>> newIriDownstreamDerivationMap = new HashMap<>();
		newEntities.stream().forEach(e -> newIriDownstreamDerivationMap.put(e.getIri(), new ArrayList<>()));

		// 2. match old-new instances by their rdf:type to determine the downstream
		// derivations the new instances should be connected to, note that if NONE of
		// the old outputs of derivaiton is input of other derivations, or if the
		// derivation was created for new info (i.e. there are no old outputs), the code
		// will NOT enter the next if clause block
		if (oldEntitiesAsInput.size() > 0) {
			LOGGER.debug("This derivation contains at least one entity which is an input to another derivation");
			LOGGER.debug("Relinking new instance(s) to the derivation by matching their rdf:type");
			// before replacing the old entity with the new instance, we need to make sure
			// that it remains linked to the appropriate derivation, thus we first do the
			// instance matching here
			for (Entity oldInput : oldEntitiesAsInput) {
				// find within new Entities with the same rdf:type
				List<Entity> matchingEntity = newEntities.stream()
						.filter(e -> e.getRdfType().equals(oldInput.getRdfType())).collect(Collectors.toList());

				if (matchingEntity.size() != 1) {
					String errmsg = "When the agent writes new instances, make sure that there is 1 instance with matching rdf:type over the old set";
					LOGGER.error(errmsg);
					LOGGER.error("Number of matching entities = " + matchingEntity.size());
					throw new JPSRuntimeException(errmsg);
				}

				// add IRI of the matched instance and the derivation it should connect to
				newIriDownstreamDerivationMap.get(matchingEntity.get(0).getIri())
						.addAll(oldInput.getInputOf().stream().map(d -> d.getIri()).collect(Collectors.toList()));
			}
		}

		// 3. we also need to check and update the connection accordingly if any of the
		// downstream derivations are directly connected to this derivation, i.e. the
		// current derivation was created for new information, and other derivation
		// instances further depend on the current one
		List<Derivation> directedDownstream = finishedDerivation.getDirectedDownstreams();
		if (!directedDownstream.isEmpty()) {
			// here the instance matching is done by querying the I/O signature of the
			// agents that responsible for monitoring the retrieved downstream derivations,
			// note that it is not directly matching the rdf:type, rather, it is matching
			// {?instance rdf:type*/rdfs:subClassOf* ?agentInputType.}
			Map<String, List<String>> map = this.sparqlClient.matchNewDerivedIriToDownsFroNewInfo(
					newEntities.stream().map(e -> e.getIri()).collect(Collectors.toList()),
					directedDownstream.stream().map(d -> d.getIri()).collect(Collectors.toList()));
			// add matched instance-derivations to map
			map.forEach((inst, derivs) -> {
				if (newIriDownstreamDerivationMap.containsKey(inst)) {
					newIriDownstreamDerivationMap.get(inst).addAll(derivs);
				} else {
					newIriDownstreamDerivationMap.put(inst, derivs);
				}
			});
		}

		// 4. fire SPARQL update to update the knowledge graph in one-go, for the
		// specific changes done in this update, see comments of method
		// updateFinishedAsyncDerivation(String derivation,
		// Map<String, List<String>> newIriDownstreamDerivationMap)
		this.sparqlClient.updateFinishedAsyncDerivation(derivation,
				newIriDownstreamDerivationMap);
	}

	/**
	 * Checks if the derivation is an instance of DerivationAsyn.
	 * 
	 * @param derivation
	 * @return
	 */
	public boolean isDerivedAsynchronous(String derivation) {
		return this.sparqlClient.isDerivedAsynchronous(derivation);
	}

	/**
	 * This method retrieves the status rdf:type in the format of an enum of a given
	 * derivation instance IRI.
	 * 
	 * @param derivation
	 * @return
	 */
	public StatusType getStatusType(String derivation) {
		return this.sparqlClient.getStatusType(derivation);
	}

	/**
	 * Gets the new derived IRI at derivation update (job) completion.
	 * 
	 * @param derivation
	 * @return
	 */
	public List<String> getNewDerivedIRI(String derivation) {
		return this.sparqlClient.getNewDerivedIRI(derivation);
	}

	/**
	 * Gets the agent IRI that is used to update the derivation.
	 * 
	 * @param derivedQuantity
	 * @return
	 */
	public String getAgentUrl(String derivedQuantity) {
		return this.sparqlClient.getAgentUrl(derivedQuantity);
	}

	/**
	 * Gets a list of derivations that is derived using a given agent IRI.
	 * 
	 * @param agentIRI
	 * @return
	 */
	public List<String> getDerivations(String agentIRI) {
		return this.sparqlClient.getDerivations(agentIRI);
	}

	/**
	 * Gets a list of paired derivations and their status type (if applicable) that
	 * are derived using a given agent IRI.
	 * 
	 * @param agentIRI
	 * @return
	 */
	public Map<String, StatusType> getDerivationsAndStatusType(String agentIRI) {
		return this.sparqlClient.getDerivationsAndStatusType(agentIRI);
	}

	/**
	 * This method retrieves a list of derivation instance IRI given a list of
	 * derived quantities.
	 * 
	 * @param entities
	 * @return
	 */
	public Map<String, String> getDerivationsOf(List<String> entities) {
		return this.sparqlClient.getDerivationsOf(entities);
	}

	/**
	 * This method updates the knowledge graph when the update of one derivation is
	 * finished.
	 * 
	 * @param outputTriples
	 * @param newIriDownstreamDerivationMap
	 * @param derivation
	 * @param retrievedInputsAt
	 */
	public boolean reconnectNewDerivedIRIs(List<TriplePattern> outputTriples,
			Map<String, List<String>> newIriDownstreamDerivationMap, String derivation,
			Long retrievedInputsAt) {
		return this.sparqlClient.reconnectNewDerivedIRIs(outputTriples, newIriDownstreamDerivationMap,
				derivation, retrievedInputsAt);
	}

	/**
	 * This method retrieves the derivation instance given the derivation IRI.
	 * 
	 * @param derivationIRI
	 * @return
	 */
	public Derivation getDerivation(String derivationIRI) {
		return this.sparqlClient.getDerivation(derivationIRI);
	}

	/**
	 * All private functions below
	 */

	/**
	 * This method marks the derivation as "Requested" when it detects a derivation
	 * is outdated.
	 * 
	 * @param instance
	 * @param graph
	 */
	@Deprecated
	private void updateDerivationAsyn(String instance, DirectedAcyclicGraph<String, DefaultEdge> graph) {
		// this method follows the first a few steps of method updateDerivation(String
		// instance, DirectedAcyclicGraph<String, DefaultEdge> graph)
		// TODO in future development, ideally these two method should be merged into
		// the same method?
		List<String> inputsAndDerived = this.sparqlClient.getInputsAndDerived(instance);

		if (!graph.containsVertex(instance)) {
			graph.addVertex(instance);
		}

		for (String input : inputsAndDerived) {
			if (graph.addVertex(input) && (null != graph.addEdge(instance, input))) {
				// (1) graph.addVertex(input) will try to add input as vertex if not already
				// exist in the graph
				// (2) (null != graph.addEdge(instance, input)) will throw an error here if
				// there is circular dependency
				// continuing... (2) addEdge will return 'null' if the edge has already been
				// added as DAGs can't
				// continuing... (2) have duplicated edges so we can stop traversing this
				// branch.
				// only when both (1) and (2) are true, we can update input
				// otherwise, node <D1> will be traversed multiple times if we have below chain
				// of derivations
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
			// here only derivation instance will enter, first we check if it is an
			// asynchronous derivation
			if (isDerivedAsynchronous(instance)) {
				// we start with checking if this derivation is OutOfDate
				if (isOutOfDate(instance, inputs)) {
					// if it is OutOfDate and no status, just mark it as Requested
					// from Requested to other status will be handled from AsynAgent side
					if (!this.sparqlClient.hasStatus(instance)) {
						this.sparqlClient.markAsRequested(instance);
					}
					// set the flag to true so that other derivations will know there is one
					// derivation upstream already Requested
					// thus they can be marked as Requested as well
					upstreamDerivationRequested = true;
				} else {
					// if the Derivation is not OutOfDate, then only consider mark it as Requested
					// if meet all below situations
					// (1) there is upstream derivation being marked as Requested;
					// (2) this Derivation does NOT have any status, otherwise just leave it with
					// its existing status
					if (upstreamDerivationRequested && !this.sparqlClient.hasStatus(instance)) {
						this.sparqlClient.markAsRequested(instance);
					}
				}
			}
		}
	}

	/**
	 * This method marks the derivation as "Requested" if the derivation is
	 * outdated. This applies to both a-/sync derivations in a DAG of pure
	 * async derivation or mixed types of derivations (async depends on sync).
	 * 
	 * @param derivation
	 * @param graph
	 */
	private void updateMixedAsyncDerivation(Derivation derivation, DirectedAcyclicGraph<String, DefaultEdge> graph) {
		// inputs that are part of another derivation (for recursive call)
		// don't need direct inputs here
		List<Derivation> immediateUpstreamDerivations = this.sparqlClient
				.getAllImmediateUpstreamDerivations(derivation.getIri());

		if (!graph.containsVertex(derivation.getIri())) {
			graph.addVertex(derivation.getIri());
		}

		for (Derivation upstream : immediateUpstreamDerivations) {
			if (graph.addVertex(upstream.getIri()) & (null != graph.addEdge(derivation.getIri(), upstream.getIri()))) {
				// (1) graph.addVertex(input) will try to add input as vertex if not already
				// exist in the graph
				// (2) (null != graph.addEdge(instance, input)) will throw an error here if
				// there is circular dependency; addEdge will return 'null' if the edge has
				// already been added as DAGs can't have duplicated edges so we can stop
				// traversing this branch.
				// NOTE both (1) and (2) will execute as here we are using Non-short-circuit
				// Operator "&", instead of short-circuit operator "&&"
				// Only when both (1) and (2) are true, we can update input, otherwise, node
				// <D1> will be traversed multiple times if we have below DAG of derivations
				// and we run updateMixedAsyncDerivation(<D3>, graph):
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
				updateMixedAsyncDerivation(upstream, graph);
			}
		}

		// mark the derivation as Requested if the current derivation is outdated and
		// does NOT have status already
		this.sparqlClient.markAsRequestedIfOutdated(derivation.getIri());
	}

	/**
	 * This method updates the DAG of pure synchronous derivations. The differences
	 * between this method and method updateDerivation(Derivation derivation,
	 * DirectedAcyclicGraph<String, DefaultEdge> graph) please see NOTE in comments.
	 * 
	 * @param derivation
	 * @param graph
	 * @throws IOException
	 * @throws ClientProtocolException
	 */
	private void updatePureSyncDerivation(Derivation derivation, DirectedAcyclicGraph<String, DefaultEdge> graph)
			throws ClientProtocolException, IOException {
		// inputs that are part of another derivation (for recursive call)
		// don't need direct inputs here
		List<Derivation> upstreamDerivations = derivation.getInputsWithBelongsTo();

		if (!graph.containsVertex(derivation.getIri())) {
			graph.addVertex(derivation.getIri());
		}

		for (Derivation upstream : upstreamDerivations) {
			if (graph.addVertex(upstream.getIri()) & (null != graph.addEdge(derivation.getIri(), upstream.getIri()))) {
				// NOTE difference 1 - resolve multiple traverse problem
				// the above line is different from the checking in method
				// updateDerivation(Derivation derivation, DirectedAcyclicGraph<String,
				// DefaultEdge> graph) - this is to prevent the multiple traverse of the
				// upstream derivation in a DAG structure as demonstrated in the comments of
				// method updateMixedAsyncDerivation(Derivation derivation,
				// DirectedAcyclicGraph<String, DefaultEdge> graph)
				updatePureSyncDerivation(upstream, graph);
			}
		}

		// inputs required by the agent
		List<String> inputs = derivation.getAgentInputs();
		if (inputs.size() > 0) {
			// at this point, "instance" is a derived instance for sure, any other instances
			// will not go through this code
			// getInputs queries for <instance> <isDerivedFrom> ?x
			if (derivation.isOutOfDate()) {
				LOGGER.info("Updating <" + derivation.getIri() + ">");
				// calling agent to create a new instance
				String agentURL = derivation.getAgentURL();
				JSONObject requestParams = new JSONObject();
				// NOTE difference 2 - pass in information collected from derivation
				requestParams.put(AGENT_INPUT_KEY, derivation.getAgentInputsMap()); // mapped IRIs of isDerivedFrom
				requestParams.put(BELONGSTO_KEY, derivation.getBelongsToMap()); // mapped IRIs of belongsTo
				requestParams.put(DERIVATION_KEY, derivation.getIri()); // IRI of this derivation
				requestParams.put(DERIVATION_TYPE_KEY, derivation.getRdfType()); // rdf:type of this derivation
				requestParams.put(DOWNSTREAMDERIVATION_KEY, derivation.getDownstreamDerivationMap()); // downstream
				requestParams.put(SYNC_NEW_INFO_FLAG, false); // set flag to indicate the derivation is for update

				LOGGER.debug("Updating <" + derivation.getIri() + "> using agent at <" + agentURL
						+ "> with http request " + requestParams);

				// execute update via HTTP GET, note that below block replaces the previous way
				// of execute HTTP reqeust via calling AgentCaller.executeGetWithURLAndJSON,
				// i.e.:
				// String response = AgentCaller.executeGetWithURLAndJSON(agentURL,
				// requestParams.toString());
				// TODO we may be able to re-use AgentCaller once the dependency is resolved:
				// this change is motivated by the fact that the Java dependency javax is not
				// packaged in py4jps so an error will be thrown in python side when derivation
				// agent requesting update for sync derivation when dealing with mixed
				// derivation DAG (all the Java agents working fine as such dependency is
				// provided in tomcat at deployment), the error message:
				// java.lang.NoClassDefFoundError: javax/servlet/ServletInputStream
				// at
				// uk.ac.cam.cares.jps.base.discovery.AgentCaller.createURIWithURLandJSON(AgentCaller.java:185)
				// at
				// uk.ac.cam.cares.jps.base.discovery.AgentCaller.executeGetWithURLAndJSON(AgentCaller.java:178)
				// at
				// uk.ac.cam.cares.jps.base.derivation.DerivationClient.updatePureSyncDerivation(DerivationClient.java:1010)
				HttpResponse httpResponse;
				CloseableHttpClient httpClient = HttpClients.createDefault();
				String originalRequest = agentURL + GET_AGENT_INPUT_PARAMS_KEY_JPSHTTPSERVLET
						+ requestParams.toString();
				HttpGet httpGet = new HttpGet(agentURL + GET_AGENT_INPUT_PARAMS_KEY_JPSHTTPSERVLET
						+ URLEncoder.encode(requestParams.toString(), StandardCharsets.UTF_8.toString()));

				httpResponse = httpClient.execute(httpGet);
				if (httpResponse.getStatusLine().getStatusCode() != 200) {
					String msg = "Failed to update derivation <" + derivation.getIri() + "> with original request: "
							+ originalRequest;
					String body = EntityUtils.toString(httpResponse.getEntity());
					LOGGER.error(msg);
					throw new JPSRuntimeException(msg + " Error body: " + body);
				}
				String response = EntityUtils.toString(httpResponse.getEntity());
				LOGGER.debug("Obtained http response from agent: " + response);

				// NOTE difference 3 - as the update on knowledge graph will be done by the
				// DerivationAgent for normal Derivation, here we only need to update the cached
				// value for normal Derivation, whereas for DerivationWithTimeSeries, we need to
				// update the timestamp and status (if presented)
				JSONObject agentResponse = new JSONObject(response);
				// NOTE difference 4 - the timestamp is read from the agent response and used
				// for updating the cached derivations
				derivation.setTimestamp(agentResponse.getLong(DerivationOutputs.RETRIEVED_INPUTS_TIMESTAMP_KEY));

				// if it is a derived quantity with time series, there will be no changes to the
				// instances, only timestamp will be updated
				if (!derivation.isDerivationWithTimeSeries()) {

					// entities that are input to another derivation
					List<Entity> inputToAnotherDerivation = derivation.getEntities()
							.stream().filter(e -> e.isInputToDerivation()).collect(Collectors.toList());

					// NOTE difference 5 - here we create lists to be used when reconnecting
					// inputs and updating cached data, as now the new entiteis are returned as part
					// of HTTP response, we can create list of Entities directly
					// TODO we may consider return the entities if we decided to provide the
					// TODO function accessInformation
					List<Entity> newEntities = new ArrayList<>();
					Iterator<String> keys = agentResponse.getJSONObject(DerivationClient.AGENT_OUTPUT_KEY).keys();
					while (keys.hasNext()) {
						String iri = keys.next();
						Entity ne = new Entity(iri);
						ne.setRdfType(agentResponse.getJSONObject(DerivationClient.AGENT_OUTPUT_KEY).getString(iri));
						newEntities.add(ne);
					}

					if (inputToAnotherDerivation.size() > 0) {
						LOGGER.debug(
								"This derivation contains at least one entity which is an input to another derivation");
						LOGGER.debug("Relinking new instance(s) to the derivation by matching their rdf:type");

						// UPDATE CACHED DATA
						// here we do the mapping in memory first to get the mapping between downstream
						// derivations and new instances to be connected - we need to make sure that the
						// new instances remains linked to the appropriate downstream derivations

						for (Entity oldInput : inputToAnotherDerivation) {
							// find within new Entities with the same rdf:type
							List<Entity> matchingEntity = newEntities.stream()
									.filter(e -> e.getRdfType().equals(oldInput.getRdfType()))
									.collect(Collectors.toList());

							if (matchingEntity.size() != 1) {
								String errmsg = "When the agent writes new instances, make sure that there is 1 instance with matching rdf:type over the old set";
								LOGGER.error(errmsg);
								LOGGER.error("Number of matching entities = " + matchingEntity.size());
								throw new JPSRuntimeException(errmsg);
							}

							// update cached data
							oldInput.getInputOf().forEach(d -> {
								Derivation derivationToReconnect = d;
								derivationToReconnect.addInput(matchingEntity.get(0));
								derivationToReconnect.removeInput(oldInput);
							});
						}
					}
					// also update cached data if newEntities were generated
					if (!newEntities.isEmpty()) {
						derivation.replaceEntities(newEntities);
					}
				} else {
					// NOTE difference 7 - update timestamp after the update of every
					// DerivationWithTimeSeries, so here we update timestamp, delete status (for
					// sync in mixed type DAGs) in one-go
					this.sparqlClient.updateTimestampDeleteStatus(derivation.getIri(), derivation.getTimestamp());
				}
			}
		}
	}

	/**
	 * called by the public function updateInstance
	 * 
	 * @param instance
	 * @param derivedList
	 */
	@Deprecated
	private void updateDerivation(Derivation derivation, DirectedAcyclicGraph<String, DefaultEdge> graph) {
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
			if (null != graph.addEdge(derivation.getIri(), input.getIri())) { // will throw an error here if there is
																				// circular dependency
				// addEdge will return 'null' if the edge has already been added as DAGs can't
				// have duplicated edges so we can stop traversing this branch.
				updateDerivation(input, graph);
			}
		}

		// inputs required by the agent
		List<String> inputs = derivation.getAgentInputs();
		if (inputs.size() > 0) {
			// at this point, "instance" is a derived instance for sure, any other instances
			// will not go through this code
			// getInputs queries for <instance> <isDerivedFrom> ?x
			if (derivation.isOutOfDate()) {
				LOGGER.info("Updating <" + derivation.getIri() + ">");
				// calling agent to create a new instance
				String agentURL = derivation.getAgentURL();
				JSONObject requestParams = new JSONObject();
				JSONArray iris = new JSONArray(inputs);
				requestParams.put(AGENT_INPUT_KEY, iris);
				requestParams.put(BELONGSTO_KEY, derivation.getEntitiesIri()); // IRIs of belongsTo

				LOGGER.debug("Updating <" + derivation.getIri() + "> using agent at <" + agentURL
						+ "> with http request " + requestParams);
				// record timestamp at the point the request is sent to the agent
				long newTimestamp = Instant.now().getEpochSecond();
				String response = AgentCaller.executeGetWithURLAndJSON(agentURL, requestParams.toString());

				LOGGER.debug("Obtained http response from agent: " + response);

				// if it is a derived quantity with time series, there will be no changes to the
				// instances
				if (!derivation.isDerivationWithTimeSeries()) {
					// collect new instances created by agent
					List<String> newEntitiesString = new JSONObject(response).getJSONArray(AGENT_OUTPUT_KEY).toList()
							.stream().map(iri -> (String) iri).collect(Collectors.toList());

					// delete old instances
					this.sparqlClient.deleteBelongsTo(derivation.getIri());
					LOGGER.debug("Deleted old instances of: " + derivation.getIri());

					// link new entities to derived instance, adding ?x <belongsTo> <instance>
					this.sparqlClient.addNewEntitiesToDerived(derivation.getIri(), newEntitiesString);
					LOGGER.debug("Added new instances <" + newEntitiesString + "> to the derivation <"
							+ derivation.getIri() + ">");

					// entities that are input to another derivation
					List<Entity> inputToAnotherDerivation = derivation.getEntities()
							.stream().filter(e -> e.isInputToDerivation()).collect(Collectors.toList());

					// TODO below lines are only changed to make the code compile
					// TODO its functions are NOT tested due to marked as Deprecated
					List<Entity> newEntities = this.sparqlClient.initialiseNewEntities(derivation.getIri());

					if (inputToAnotherDerivation.size() > 0) {
						LOGGER.debug(
								"This derivation contains at least one entity which is an input to another derivation");
						LOGGER.debug("Relinking new instance(s) to the derivation by matching their rdf:type");
						// after deleting the old entity, we need to make sure that it remains linked to
						// the appropriate derived instance
						List<String> newInputs = new ArrayList<>();
						List<String> derivationsToReconnect = new ArrayList<>();
						for (Entity oldInput : inputToAnotherDerivation) {
							// find within new Entities with the same rdf:type
							List<Entity> matchingEntity = newEntities.stream()
									.filter(e -> e.getRdfType().equals(oldInput.getRdfType()))
									.collect(Collectors.toList());

							if (matchingEntity.size() != 1) {
								String errmsg = "When the agent writes new instances, make sure that there is 1 instance with matching rdf:type over the old set";
								LOGGER.error(errmsg);
								LOGGER.error("Number of matching entities = " + matchingEntity.size());
								throw new JPSRuntimeException(errmsg);
							}

							// update cached data
							// TODO below lines are only changed to make the code compile
							// TODO its functions are NOT tested due to marked as Deprecated
							oldInput.getInputOf().forEach(d -> {
								Derivation derivationToReconnect = d;
								derivationToReconnect.addInput(matchingEntity.get(0));
								derivationToReconnect.removeInput(oldInput);

								newInputs.add(matchingEntity.get(0).getIri());
								derivationsToReconnect.add(derivationToReconnect.getIri());
							});
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

	private void validateDerivation(Derivation derivation, DirectedAcyclicGraph<String, DefaultEdge> graph) {
		// we also need to consider the derivations that are created for new information
		// here we assume that the directedUpstream derivation doesn't have outptus yet
		List<Derivation> allUpstreamDerivations = Stream
				.concat(derivation.getInputsWithBelongsTo().stream(), derivation.getDirectedUpstreams().stream())
				.distinct().collect(Collectors.toList());

		if (!graph.containsVertex(derivation.getIri())) {
			graph.addVertex(derivation.getIri());
		}

		for (Derivation upstream : allUpstreamDerivations) {
			if (!derivation.isDerivationAsyn() && upstream.isDerivationAsyn()) {
				// this checking is added to raise an error when a sync derivation is depending
				// on an async derivation
				throw new JPSRuntimeException("Synchronous derivation <" + derivation.getIri()
						+ "> depends on asynchronous derivation <" + upstream.getIri() + ">.");
			}

			if (graph.addVertex(upstream.getIri()) & (null != graph.addEdge(derivation.getIri(), upstream.getIri()))) {
				// NOTE the changes made here combined the two condition check into one, this
				// is to prevent the multiple traverse of the upstream derivation in a DAG
				// structure as demonstrated in the comments of method
				// updateMixedAsyncDerivation(Derivation derivation,
				// DirectedAcyclicGraph<String, DefaultEdge> graph)

				// NOTE Non-short-circuit Operator "&", instead of short-circuit operator "&&"
				// was used here so that the second condition will ALWAYS be checked, so will
				// throw an error here if there is circular dependency
				// addEdge will return 'null' if the edge has already been added as DAGs
				// can't have duplicated edges so we can stop traversing this branch.
				validateDerivation(upstream, graph);
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
	 * 
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
