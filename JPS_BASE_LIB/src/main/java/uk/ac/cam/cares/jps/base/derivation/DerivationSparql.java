package uk.ac.cam.cares.jps.base.derivation;

import static org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf.iri;

import java.io.IOException;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import org.eclipse.rdf4j.sparqlbuilder.constraint.Expression;
import org.eclipse.rdf4j.sparqlbuilder.constraint.Expressions;
import org.eclipse.rdf4j.sparqlbuilder.core.Prefix;
import org.eclipse.rdf4j.sparqlbuilder.core.PropertyPaths;
import org.eclipse.rdf4j.sparqlbuilder.core.SparqlBuilder;
import org.eclipse.rdf4j.sparqlbuilder.core.Variable;
import org.eclipse.rdf4j.sparqlbuilder.core.query.ModifyQuery;
import org.eclipse.rdf4j.sparqlbuilder.core.query.Queries;
import org.eclipse.rdf4j.sparqlbuilder.core.query.SelectQuery;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.GraphPattern;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.GraphPatternNotTriples;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.GraphPatterns;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.SubSelect;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.TriplePattern;
import org.eclipse.rdf4j.sparqlbuilder.rdf.Iri;
import org.eclipse.rdf4j.sparqlbuilder.rdf.RdfPredicate;
import org.eclipse.rdf4j.model.vocabulary.OWL;
import org.eclipse.rdf4j.model.vocabulary.RDFS;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;
import org.apache.http.HttpResponse;
import org.apache.http.ParseException;
import org.apache.http.util.EntityUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.interfaces.StoreClientInterface;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

/**
 * SPARQL queries/updates for instances related to derived quantities
 * These functions are called by the DerivationClient class
 * 
 * @author Kok Foong Lee
 * @author Jiaru Bai
 *
 */
public class DerivationSparql {
	private StoreClientInterface storeClient;
	private String derivationInstanceBaseURL; // an example of this can be
												// "https://www.example.com/triplestore/repository/"

	public static String derivednamespace = "https://raw.githubusercontent.com/cambridge-cares/TheWorldAvatar/main/JPS_Ontology/ontology/ontoderivation/OntoDerivation.owl#";

	// placeholder string used by method getAllDerivations()
	private static final String PLACEHOLDER = "http://This_is_a_placeholder_string";
	// placeholder Iri used by method getDerivation(String rootDerivationIRI)
	private static final Iri PLACEHOLDER_IRI = iri(PLACEHOLDER);

	// status concepts
	private static String REQUESTED = "Requested";
	private static String INPROGRESS = "InProgress";
	private static String FINISHED = "Finished";

	// derivation types
	public static String DERIVATION = "Derivation";
	public static String DERIVATIONWITHTIMESERIES = "DerivationWithTimeSeries";
	public static String DERIVATIONASYN = "DerivationAsyn";
	public static String ONTODERIVATION_DERIVATION = derivednamespace + DERIVATION;
	public static String ONTODERIVATION_DERIVATIONASYN = derivednamespace + DERIVATIONASYN;
	public static String ONTODERIVATION_DERIVATIONWITHTIMESERIES = derivednamespace + DERIVATIONWITHTIMESERIES;

	// prefix/namespace
	private static Prefix p_agent = SparqlBuilder.prefix("agent",
			iri("http://www.theworldavatar.com/ontology/ontoagent/MSM.owl#"));
	private static Prefix p_derived = SparqlBuilder.prefix("derived", iri(derivednamespace));
	private static Prefix p_time = SparqlBuilder.prefix("time", iri("http://www.w3.org/2006/time#"));

	// classes
	private static Iri Service = p_agent.iri("Service");
	private static Iri Operation = p_agent.iri("Operation");
	private static Iri TimePosition = p_time.iri("TimePosition");
	private static Iri Derivation = p_derived.iri(DERIVATION);
	private static Iri DerivationWithTimeSeries = p_derived.iri(DERIVATIONWITHTIMESERIES);
	private static Iri DerivationAsyn = p_derived.iri(DERIVATIONASYN);
	private static Iri Status = p_derived.iri("Status");
	private static Iri Requested = p_derived.iri(REQUESTED);
	private static Iri InProgress = p_derived.iri(INPROGRESS);
	private static Iri Finished = p_derived.iri(FINISHED);
	private static Iri InstantClass = p_time.iri("Instant");

	// object properties
	private static Iri hasHttpUrl = p_agent.iri("hasHttpUrl");
	private static Iri hasOperation = p_agent.iri("hasOperation");
	private static Iri hasInput = p_agent.iri("hasInput");
	private static Iri hasMandatoryPart = p_agent.iri("hasMandatoryPart");
	private static Iri hasType = p_agent.iri("hasType");
	private static Iri hasName = p_agent.iri("hasName");
	private static Iri isDerivedFrom = p_derived.iri("isDerivedFrom");
	private static Iri isDerivedUsing = p_derived.iri("isDerivedUsing");
	private static Iri belongsTo = p_derived.iri("belongsTo");
	private static Iri hasStatus = p_derived.iri("hasStatus");
	private static Iri hasNewDerivedIRI = p_derived.iri("hasNewDerivedIRI");
	private static Iri hasTime = p_time.iri("hasTime");
	private static Iri numericPosition = p_time.iri("numericPosition");
	private static Iri hasTRS = p_time.iri("hasTRS");
	private static Iri inTimePosition = p_time.iri("inTimePosition");

	// data properties
	private static Iri retrievedInputsAt = p_derived.iri("retrievedInputsAt");
	private static Iri uuidLock = p_derived.iri("uuidLock");

	// the derived quantity client relies on matching rdf:type to figure out which
	// old instances to delete
	// if your instances have more than 1 rdf:type, you must add them to this list
	// so that the client can figure out which to use
	private static List<Iri> classesToIgnore = Arrays.asList(iri(OWL.THING), iri(OWL.NAMEDINDIVIDUAL));

	//
	private static final Map<String, StatusType> statusToType;
	static {
		Map<String, StatusType> statusMap = new HashMap<>();
		statusMap.put(derivednamespace.concat(REQUESTED), StatusType.REQUESTED);
		statusMap.put(derivednamespace.concat(INPROGRESS), StatusType.INPROGRESS);
		statusMap.put(derivednamespace.concat(FINISHED), StatusType.FINISHED);
		statusToType = statusMap;
	}

	//
	private static final Map<String, Iri> derivationToIri;
	static {
		Map<String, Iri> derivationTypeMap = new HashMap<>();
		derivationTypeMap.put(derivednamespace.concat(DERIVATION), Derivation);
		derivationTypeMap.put(derivednamespace.concat(DERIVATIONWITHTIMESERIES), DerivationWithTimeSeries);
		derivationTypeMap.put(derivednamespace.concat(DERIVATIONASYN), DerivationAsyn);
		derivationToIri = derivationTypeMap;
	}

	// all possible derivation rdf:type
	public static final List<String> derivationTypes = derivationToIri.keySet().stream().collect(Collectors.toList());

	// all possible derivation status rdf:type
	public static final List<String> statusType = statusToType.keySet().stream().collect(Collectors.toList());

	private static final Logger LOGGER = LogManager.getLogger(DerivationSparql.class);

	/**
	 * This constructor is tagged as @Deprecated as ideally user should provide
	 * based URL when creating derivation instances.
	 * 
	 * @param storeClient
	 */
	@Deprecated
	public DerivationSparql(StoreClientInterface storeClient) {
		this.storeClient = storeClient;
		this.derivationInstanceBaseURL = derivednamespace; // this line is added to the original constructor so that the
															// current codes will not break
	}

	/**
	 * This constructor should be used to enable customised derivation instance base
	 * URL.
	 * 
	 * @param storeClient
	 * @param derivationInstanceBaseURL
	 */
	public DerivationSparql(StoreClientInterface storeClient, String derivationInstanceBaseURL) {
		this.storeClient = storeClient;
		this.derivationInstanceBaseURL = derivationInstanceBaseURL;
	}

	/**
	 * This method unifies all methods that create multiple derivations in one go.
	 * 
	 * @param entitiesList
	 * @param agentIRIList
	 * @param agentURLList
	 * @param inputsList
	 * @param derivationTypeList
	 * @param forAsyncUpdateFlagList
	 * @return
	 */
	List<String> unifiedBulkCreateDerivations(List<List<String>> entitiesList, List<String> agentIRIList,
			List<String> agentURLList, List<List<String>> inputsList, List<Iri> derivationTypeList, List<Boolean> forAsyncUpdateFlagList) {
		ModifyQuery modify = Queries.MODIFY();

		if (entitiesList.size() != agentIRIList.size()) {
			String errmsg = "Size of entities list is different from agent IRI list";
			LOGGER.fatal(errmsg);
			throw new JPSRuntimeException(errmsg);
		}

		if (entitiesList.size() != agentURLList.size()) {
			String errmsg = "Size of entities list is different from agent URL list";
			LOGGER.fatal(errmsg);
			throw new JPSRuntimeException(errmsg);
		}

		if (entitiesList.size() != inputsList.size()) {
			String errmsg = "Size of entities list is different from inputs list";
			LOGGER.fatal(errmsg);
			throw new JPSRuntimeException(errmsg);
		}

		if (entitiesList.size() != derivationTypeList.size()) {
			String errmsg = "Size of entities list is different from derivationType list";
			LOGGER.fatal(errmsg);
			throw new JPSRuntimeException(errmsg);
		}

		if (entitiesList.size() != forAsyncUpdateFlagList.size()) {
			String errmsg = "Size of entities list is different from forAsyncUpdateFlag list";
			LOGGER.fatal(errmsg);
			throw new JPSRuntimeException(errmsg);
		}

		List<String> derivations = new ArrayList<>();

		for (int i = 0; i < entitiesList.size(); i++) {
			List<String> entities = entitiesList.get(i);
			List<String> inputs = inputsList.get(i);
			String agentIRI = agentIRIList.get(i);
			String agentURL = agentURLList.get(i);
			Boolean forUpdateFlag = forAsyncUpdateFlagList.get(i);
			Iri derivationType = derivationTypeList.get(i);
			// create a unique IRI for this new derived quantity
			String derivedQuantity = derivationInstanceBaseURL + "derived_" + UUID.randomUUID().toString();
			derivations.add(derivedQuantity);
			Iri derived_iri = iri(derivedQuantity);

			modify.insert(derived_iri.isA(derivationType));

			// add belongsTo
			for (String entity : entities) {
				modify.insert(iri(entity).has(belongsTo, derived_iri));
			}

			// link inputs
			for (String input : inputs) {
				modify.insert(derived_iri.has(isDerivedFrom, iri(input)));
			}

			// add status triples if it's async derivation for update
			if (forUpdateFlag) {
				Iri status_iri = iri(derivationInstanceBaseURL + "status_" + UUID.randomUUID().toString());
				modify.insert(derived_iri.has(hasStatus, status_iri));
				modify.insert(status_iri.isA(Requested));
			}

			// link to agent
			// here it is assumed that an agent only has one operation
			modify.insert(derived_iri.has(isDerivedUsing, iri(agentIRI)));
			// only add information about ontoagent:Operation to knowledge graph if the
			// agent url is provided and is NOT PLACEHOLDER string
			if (!agentURL.contentEquals(PLACEHOLDER)) {
				String operation_iri = derivationInstanceBaseURL + UUID.randomUUID().toString();
				// add agent url
				modify.insert(iri(agentIRI).isA(Service).andHas(hasOperation, iri(operation_iri)));
				modify.insert(iri(operation_iri).isA(Operation).andHas(hasHttpUrl, iri(agentURL)));
			}
		}

		modify.prefix(p_derived, p_agent);
		storeClient.executeUpdate(modify.getQueryString());
		return derivations;
	}

	/**
	 * creates a new instance of derived quantity, grouping the given entities under
	 * this instance
	 * whenever this derived quantity gets updated, the provided entities will get
	 * deleted by the client
	 * 
	 * @param kbClient
	 * @param entities
	 * @param agentIRI
	 * @param agentURL
	 * @param inputs
	 */
	String createDerivation(List<String> entities, String agentIRI, String agentURL, List<String> inputs) {
		ModifyQuery modify = Queries.MODIFY();

		// create a unique IRI for this new derived quantity
		String derivedQuantity = derivationInstanceBaseURL + "derived_" + UUID.randomUUID().toString();

		Iri derived_iri = iri(derivedQuantity);

		modify.insert(derived_iri.isA(Derivation));

		// add belongsTo
		// first check if the entities are already belongsTo other derivation
		Map<String, String> entityDerivationMap = getDerivationsOf(entities);
		if (entityDerivationMap.isEmpty()) {
			for (String entity : entities) {
				modify.insert(iri(entity).has(belongsTo, derived_iri));
			}
		} else {
			String errmsg = "ERROR: some entities are already part of another derivation" + entityDerivationMap.toString();
			LOGGER.fatal(errmsg);
			throw new JPSRuntimeException(errmsg);
		}

		// link to agent
		// here it is assumed that an agent only has one operation
		modify.insert(derived_iri.has(isDerivedUsing, iri(agentIRI)));
		String operation_iri = derivationInstanceBaseURL + UUID.randomUUID().toString();
		// add agent url
		modify.insert(iri(agentIRI).isA(Service).andHas(hasOperation, iri(operation_iri)));
		modify.insert(iri(operation_iri).isA(Operation).andHas(hasHttpUrl, iri(agentURL)));

		// link to each input
		for (String input : inputs) {
			modify.insert(derived_iri.has(isDerivedFrom, iri(input)));
		}

		storeClient.setQuery(modify.prefix(p_time, p_derived, p_agent).getQueryString());
		storeClient.executeUpdate();

		return derivedQuantity;
	}

	/**
	 * This method creates a new instance of derived quantity, grouping the given
	 * entities under this instance,
	 * whenever this derived quantity gets updated, the provided entities will get
	 * deleted by the client.
	 * This method primarily follows createDerivation(StoreClientInterface kbClient,
	 * List<String> entities,
	 * String agentIRI, String agentURL, List<String> inputs), except that this
	 * method does NOT create statements
	 * about the OntoAgent:Operation and OntoAgent:hasHttpUrl. Rather, this method
	 * assumes the triples
	 * {<Agent> <msm:hasOperation> <Operation>} and {<Operation> <msm:hasHttpUrl>
	 * <URL>}
	 * already exist in respective OntoAgent instances.
	 * 
	 * @param kbClient
	 * @param entities
	 * @param agentIRI
	 * @param inputs
	 * @return
	 */
	String createDerivation(List<String> entities, String agentIRI, List<String> inputs) {
		ModifyQuery modify = Queries.MODIFY();

		// create a unique IRI for this new derived quantity
		String derivedQuantity = derivationInstanceBaseURL + "derived_" + UUID.randomUUID().toString();

		Iri derived_iri = iri(derivedQuantity);

		modify.insert(derived_iri.isA(Derivation));

		// add belongsTo
		// first check if the entities are already belongsTo other derivation
		Map<String, String> entityDerivationMap = getDerivationsOf(entities);
		if (entityDerivationMap.isEmpty()) {
			for (String entity : entities) {
				modify.insert(iri(entity).has(belongsTo, derived_iri));
			}
		} else {
			String errmsg = "ERROR: some entities are already part of another derivation" + entityDerivationMap.toString();
			LOGGER.fatal(errmsg);
			throw new JPSRuntimeException(errmsg);
		}

		// link to agent
		// here it is assumed that an agent only has one operation
		modify.insert(derived_iri.has(isDerivedUsing, iri(agentIRI)));

		// link to each input
		for (String input : inputs) {
			modify.insert(derived_iri.has(isDerivedFrom, iri(input)));
		}

		modify.prefix(p_time, p_derived, p_agent);

		storeClient.setQuery(modify.prefix(p_time, p_derived, p_agent).getQueryString());
		storeClient.executeUpdate();
		return derivedQuantity;
	}

	/**
	 * This method creates a new IRI of derivation instance.
	 * 
	 * @return
	 */
	String createDerivationIRI() {
		// create a unique IRI for this new derived quantity
		String derivedQuantity = derivationInstanceBaseURL + "derived_" + UUID.randomUUID().toString();
		return derivedQuantity;
	}

	/**
	 * This method queries the agentURL given agentIRI.
	 * 
	 * @param agentIRI
	 * @return
	 */
	String getAgentUrlGivenAgentIRI(String agentIRI) {
		SelectQuery query = Queries.SELECT();

		String queryKey = "url";
		Variable url = SparqlBuilder.var(queryKey);

		Iri agentIri = iri(agentIRI);

		GraphPattern queryPattern = agentIri.has(PropertyPaths.path(hasOperation, hasHttpUrl), url);

		query.select(url).where(queryPattern).prefix(p_agent);

		storeClient.setQuery(query.getQueryString());

		String queryResult = storeClient.executeQuery().getJSONObject(0).getString(queryKey);

		return queryResult;
	}

	/**
	 * This method writes all triples generated for the new created synchronous
	 * derivation to the knowledge graph.
	 * 
	 * @param outputTriples
	 * @param entities
	 * @param agentIRI
	 * @param inputsIRI
	 * @param derivationIRI
	 * @param derivationType
	 * @param retrievedInputsAt
	 */
	void writeSyncDerivationNewInfo(List<TriplePattern> outputTriples, List<String> entities,
			String agentIRI, List<String> inputsIRI, String derivationIRI, String derivationType,
			Long retrievedInputsAt) {
		ModifyQuery modify = Queries.MODIFY();

		// add <derivation> <rdf:type> <derivationType>
		modify.insert(iri(derivationIRI).isA(iri(derivationType)));

		// insert all generated output triples (new information)
		outputTriples.forEach(t -> modify.insert(t));

		// add belongsTo
		// first check if the entities are already belongsTo other derivation
		Map<String, String> entityDerivationMap = getDerivationsOf(entities);
		if (entityDerivationMap.isEmpty()) {
			for (String entity : entities) {
				modify.insert(iri(entity).has(belongsTo, iri(derivationIRI)));
			}
		} else {
			String errmsg = "ERROR: some entities are already part of another derivation" + entityDerivationMap.toString();
			LOGGER.fatal(errmsg);
			throw new JPSRuntimeException(errmsg);
		}

		// add <derivation> <isDerivedFrom> <input> for all inputs
		inputsIRI.forEach(input -> {
			modify.insert(iri(derivationIRI).has(isDerivedFrom, iri(input)));
		});

		// add <derivation> <isDerivedUsing> <agentIRI>
		modify.insert(iri(derivationIRI).has(isDerivedUsing, iri(agentIRI)));

		// add timestamp instance for the created derivation, unix time following the
		// w3c standard
		String time_instant = derivationInstanceBaseURL + "time" + UUID.randomUUID().toString();
		String time_unix = derivationInstanceBaseURL + "time" + UUID.randomUUID().toString();
		Iri time_instant_iri = iri(time_instant);
		Iri time_unix_iri = iri(time_unix);
		modify.insert(iri(derivationIRI).has(hasTime, time_instant_iri));
		modify.insert(time_instant_iri.isA(InstantClass).andHas(inTimePosition, time_unix_iri));
		modify.insert(time_unix_iri.isA(TimePosition).andHas(numericPosition, retrievedInputsAt).andHas(hasTRS,
				iri("http://dbpedia.org/resource/Unix_time")));

		// execute SPARQL update
		storeClient.setQuery(modify.prefix(p_time, p_derived, p_agent).getQueryString());
		storeClient.executeUpdate();
	}

	/**
	 * same method as above but this creates multiple derivations in 1 go
	 * 
	 * @param entities
	 * @param agentIRI
	 * @param agentURL
	 * @param inputs
	 */
	List<String> bulkCreateDerivations(List<List<String>> entitiesList, List<String> agentIRIList,
			List<String> agentURLList, List<List<String>> inputsList) {
		List<Iri> derivationTypeList = IntStream.range(0, entitiesList.size()).mapToObj(i -> Derivation)
				.collect(Collectors.toList());
		List<Boolean> forAsyncUpdateFlagList = IntStream.range(0, entitiesList.size()).mapToObj(i -> false)
				.collect(Collectors.toList());
		return unifiedBulkCreateDerivations(entitiesList, agentIRIList, agentURLList, inputsList,
				derivationTypeList, forAsyncUpdateFlagList);
	}

	/**
	 * same method as above, but for instances with time series and these instances
	 * do not get deleted
	 * therefore there is no need to provide a list of entities for this to annotate
	 * 
	 * @param kbClient
	 * @param entities
	 * @param agentIRI
	 * @param agentURL
	 * @param inputs
	 */
	String createDerivationWithTimeSeries(List<String> entities, String agentIRI, String agentURL,
			List<String> inputs) {
		ModifyQuery modify = Queries.MODIFY();

		// create a unique IRI for this new derived quantity
		String derivedQuantity = derivationInstanceBaseURL + "derived_" + UUID.randomUUID().toString();

		Iri derived_iri = iri(derivedQuantity);

		modify.insert(derived_iri.isA(DerivationWithTimeSeries));

		// add belongsTo
		// first check if the entities are already belongsTo other derivation
		Map<String, String> entityDerivationMap = getDerivationsOf(entities);
		if (entityDerivationMap.isEmpty()) {
			for (String entity : entities) {
				modify.insert(iri(entity).has(belongsTo, derived_iri));
			}
		} else {
			String errmsg = "ERROR: some entities are already part of another derivation" + entityDerivationMap.toString();
			LOGGER.fatal(errmsg);
			throw new JPSRuntimeException(errmsg);
		}

		// link to agent
		modify.insert(derived_iri.has(isDerivedUsing, iri(agentIRI)));
		String operation_iri = derivationInstanceBaseURL + UUID.randomUUID().toString();
		// add agent url
		modify.insert(iri(agentIRI).isA(Service).andHas(hasOperation, iri(operation_iri)));
		modify.insert(iri(operation_iri).isA(Operation).andHas(hasHttpUrl, iri(agentURL)));

		// link to each input
		for (String input : inputs) {
			modify.insert(derived_iri.has(isDerivedFrom, iri(input)));
		}

		modify.prefix(p_time, p_derived, p_agent);

		storeClient.setQuery(modify.prefix(p_time, p_derived, p_agent).getQueryString());
		storeClient.executeUpdate();

		return derivedQuantity;
	}

	/**
	 * same method as above but initialise a large number of derivations in 1 go
	 * 
	 * @param entitiesList
	 * @param agentIRIList
	 * @param agentURLList
	 * @param inputsList
	 */
	List<String> bulkCreateDerivationsWithTimeSeries(List<List<String>> entitiesList, List<String> agentIRIList,
			List<String> agentURLList, List<List<String>> inputsList) {
		List<Iri> derivationTypeList = IntStream.range(0, entitiesList.size()).mapToObj(i -> DerivationWithTimeSeries)
				.collect(Collectors.toList());
		List<Boolean> forAsyncUpdateFlagList = IntStream.range(0, entitiesList.size()).mapToObj(i -> false)
				.collect(Collectors.toList());
		return unifiedBulkCreateDerivations(entitiesList, agentIRIList, agentURLList, inputsList,
				derivationTypeList, forAsyncUpdateFlagList);
	}

	/**
	 * This method creates a new instance of asynchronous derived quantity, grouping
	 * the given entities under this instance,
	 * whenever this derived quantity gets updated, the provided entities will get
	 * deleted by the client.
	 * This method primarily follows createDerivation(StoreClientInterface kbClient,
	 * List<String> entities,
	 * String agentIRI, String agentURL, List<String> inputs), except that this
	 * method does NOT create statements
	 * about the OntoAgent:Operation and OntoAgent:hasHttpUrl. Rather, this method
	 * assumes the triples
	 * {<Agent> <msm:hasOperation> <Operation>} and {<Operation> <msm:hasHttpUrl>
	 * <URL>}
	 * already exist in respective OntoAgent instances.
	 * 
	 * @param kbClient
	 * @param entities
	 * @param agentIRI
	 * @param inputs
	 * @return
	 */
	String createDerivationAsync(List<String> entities, String agentIRI, List<String> inputs, boolean forUpdate) {
		ModifyQuery modify = Queries.MODIFY();

		// create a unique IRI for this new derived quantity
		String derivedQuantity = derivationInstanceBaseURL + "derivedAsyn_" + UUID.randomUUID().toString();

		Iri derived_iri = iri(derivedQuantity);

		modify.insert(derived_iri.isA(DerivationAsyn));

		// add belongsTo
		// first check if the entities are already belongsTo other derivation
		Map<String, String> entityDerivationMap = getDerivationsOf(entities);
		if (entityDerivationMap.isEmpty()) {
			for (String entity : entities) {
				modify.insert(iri(entity).has(belongsTo, derived_iri));
			}
		} else {
			String errmsg = "ERROR: some entities are already part of another derivation" + entityDerivationMap.toString();
			LOGGER.fatal(errmsg);
			throw new JPSRuntimeException(errmsg);
		}

		// link to agent
		// here it is assumed that an agent only has one operation
		modify.insert(derived_iri.has(isDerivedUsing, iri(agentIRI)));

		// link to each input
		for (String input : inputs) {
			modify.insert(derived_iri.has(isDerivedFrom, iri(input)));
		}

		// if the derivation is created for update, mark it as Requested
		if (forUpdate) {
			String statusIRI = derivationInstanceBaseURL + "status_" + UUID.randomUUID().toString();

			TriplePattern insert_tp = derived_iri.has(hasStatus, iri(statusIRI));
			TriplePattern insert_tp_rdf_type = iri(statusIRI).isA(Requested);

			modify.insert(insert_tp);
			modify.insert(insert_tp_rdf_type);
		}

		modify.prefix(p_time, p_derived, p_agent);

		storeClient.setQuery(modify.prefix(p_time, p_derived, p_agent).getQueryString());
		storeClient.executeUpdate();
		return derivedQuantity;
	}

	/**
	 * This method enables creating multiple asynchronous derivations in one go.
	 * Note that this method DOES create statements about OntoAgent:Operation
	 * and OntoAgent:hasHttpUrl, i.e. below triples
	 * {<Agent> <msm:hasOperation> <Operation>} and {<Operation> <msm:hasHttpUrl>
	 * <URL>}
	 * 
	 * @param entitiesList
	 * @param agentIRIList
	 * @param agentURLList
	 * @param inputsList
	 * @param forAsyncUpdateFlagList
	 * @return
	 */
	List<String> bulkCreateDerivationsAsync(List<List<String>> entitiesList, List<String> agentIRIList,
			List<String> agentURLList, List<List<String>> inputsList, List<Boolean> forAsyncUpdateFlagList) {
		List<Iri> derivationTypeList = IntStream.range(0, entitiesList.size()).mapToObj(i -> DerivationAsyn)
				.collect(Collectors.toList());
		return unifiedBulkCreateDerivations(entitiesList, agentIRIList, agentURLList, inputsList,
				derivationTypeList, forAsyncUpdateFlagList);
	}

	/**
	 * This method enables creating multiple asynchronous derivations in one go.
	 * Note that this method does NOT create statements about OntoAgent:Operation
	 * and OntoAgent:hasHttpUrl. Rather, this method assumes the triples
	 * {<Agent> <msm:hasOperation> <Operation>} and {<Operation> <msm:hasHttpUrl>
	 * <URL>}
	 * already exist in respective OntoAgent instances.
	 * 
	 * @param entitiesList
	 * @param agentIRIList
	 * @param inputsList
	 * @param forAsyncUpdateFlagList
	 * @return
	 */
	List<String> bulkCreateDerivationsAsync(List<List<String>> entitiesList, List<String> agentIRIList,
			List<List<String>> inputsList, List<Boolean> forAsyncUpdateFlagList) {
		List<String> agentURLList = IntStream.range(0, entitiesList.size()).mapToObj(i -> PLACEHOLDER)
				.collect(Collectors.toList());
		List<Iri> derivationTypeList = IntStream.range(0, entitiesList.size()).mapToObj(i -> DerivationAsyn)
				.collect(Collectors.toList());
		return unifiedBulkCreateDerivations(entitiesList, agentIRIList, agentURLList, inputsList,
				derivationTypeList, forAsyncUpdateFlagList);
	}

	/**
	 * This method enables creating multiple derivations with potentially mixed
	 * derivation type in one go.
	 * Note that this method DOES create statements about OntoAgent:Operation
	 * and OntoAgent:hasHttpUrl, i.e. below triples
	 * {<Agent> <msm:hasOperation> <Operation>} and {<Operation> <msm:hasHttpUrl>
	 * <URL>}
	 * 
	 * @param entitiesList
	 * @param agentIRIList
	 * @param agentURLList
	 * @param inputsList
	 */
	List<String> bulkCreateMixedDerivations(List<List<String>> entitiesList, List<String> agentIRIList,
			List<String> agentURLList, List<List<String>> inputsList, List<String> derivationRdfTypeList,
			List<Boolean> forAsyncUpdateFlagList) {
		List<Iri> derivationTypeList = derivationRdfTypeList.stream().map(iri -> derivationToIri.get(iri))
				.collect(Collectors.toList());
		return unifiedBulkCreateDerivations(entitiesList, agentIRIList, agentURLList, inputsList,
				derivationTypeList, forAsyncUpdateFlagList);
	}

	/**
	 * This method enables creating multiple derivations with potentially mixed
	 * derivation type in one go.
	 * Note that this method does NOT create statements about OntoAgent:Operation
	 * and OntoAgent:hasHttpUrl. Rather, this method assumes the triples
	 * {<Agent> <msm:hasOperation> <Operation>} and {<Operation> <msm:hasHttpUrl>
	 * <URL>}
	 * already exist in respective OntoAgent instances.
	 * 
	 * @param entitiesList
	 * @param agentIRIList
	 * @param inputsList
	 */
	List<String> bulkCreateMixedDerivations(List<List<String>> entitiesList, List<String> agentIRIList,
			List<List<String>> inputsList, List<String> derivationRdfTypeList, List<Boolean> forAsyncUpdateFlagList) {
		List<String> agentURLList = IntStream.range(0, entitiesList.size()).mapToObj(i -> PLACEHOLDER)
				.collect(Collectors.toList());
		List<Iri> derivationTypeList = derivationRdfTypeList.stream().map(iri -> derivationToIri.get(iri))
				.collect(Collectors.toList());
		return unifiedBulkCreateDerivations(entitiesList, agentIRIList, agentURLList, inputsList,
				derivationTypeList, forAsyncUpdateFlagList);
	}

	/**
	 * This method marks the status of the derivation as "Requested".
	 * 
	 * @param storeClient
	 * @param derivation
	 */
	@Deprecated
	String markAsRequested(String derivation) {
		deleteStatus(derivation);
		ModifyQuery modify = Queries.MODIFY();

		String statusIRI = getNameSpace(derivation) + "status_" + UUID.randomUUID().toString();

		TriplePattern insert_tp = iri(derivation).has(hasStatus, iri(statusIRI));
		TriplePattern insert_tp_rdf_type = iri(statusIRI).isA(Requested);

		modify.prefix(p_derived).insert(insert_tp);
		modify.prefix(p_derived).insert(insert_tp_rdf_type);

		storeClient.executeUpdate(modify.getQueryString());
		return statusIRI;
	}

	/**
	 * This method marks the status of the derivation as "InProgress",
	 * also records the timestamp at the point the derivation status is marked as
	 * InProgress:
	 * <derivation> <retrievedInputsAt> timestamp.
	 * A uuidLock is also added to the derivation to uniquely identify the entity that
	 * successfully marked the derivation as InProgress:
	 * <derivation> <uuidLock> uuid.
	 * 
	 * This method returns a boolean value if the uuid is added to the derivation.
	 * 
	 * @param derivation
	 * @return
	 */
	boolean updateStatusBeforeSetupJob(String derivation) {
		SelectQuery query = Queries.SELECT();
		ModifyQuery modify = Queries.MODIFY();
		Variable status = query.var();
		Variable statusType = query.var();
		Variable existingTimestamp = query.var();

		// query the status and statusType
		GraphPattern query_gp = GraphPatterns.and(
			iri(derivation).has(hasStatus, status), status.isA(statusType));
		TriplePattern delete_tp = status.isA(statusType);
		TriplePattern insert_tp_rdf_type = status.isA(InProgress);

		// record timestamp at the point the derivation status is marked as InProgress
		// <derivation> <retrievedInputsAt> timestamp.
		long retrievedInputsAtTimestamp = Instant.now().getEpochSecond();
		TriplePattern insert_tp_retrieved_inputs_at = iri(derivation).has(retrievedInputsAt,
				retrievedInputsAtTimestamp);
		// add uuidLock to the derivation, so that the agent can query if the SPARQL update is successful
		// this is to avoid the case where concurrent updates are made to the same derivation by different agent threads
		String uuid = UUID.randomUUID().toString();
		TriplePattern insert_tp_uuid_lock = iri(derivation).has(uuidLock, uuid);
		// the retrievedInputsAt data property should only be added when there's no such
		// data property already - this will prevent duplicate timestamp in case of
		// super fast monitoring
		TriplePattern existingRetrievedInputsAtPattern = iri(derivation).has(retrievedInputsAt, existingTimestamp);
		modify.delete(delete_tp).insert(insert_tp_rdf_type, insert_tp_retrieved_inputs_at, insert_tp_uuid_lock)
				.where(GraphPatterns.and(query_gp.filterNotExists(existingRetrievedInputsAtPattern)))
				.prefix(p_derived);
		storeClient.executeUpdate(modify.getQueryString());

		// check if the uuid added to the derivation is the same one
		query = Queries.SELECT();
		Variable addedUUID = query.var();
		query.prefix(p_derived).select(addedUUID).where(iri(derivation).has(uuidLock, addedUUID));
		JSONArray queryResult = storeClient.executeQuery(query.getQueryString());
		if (queryResult.isEmpty()) {
			throw new JPSRuntimeException("Failed to add uuidLock to the derivation: " + derivation);
		} else {
			JSONObject queryResultObject = queryResult.getJSONObject(0);
			String addedUUIDString = queryResultObject.getString(addedUUID.getQueryString().substring(1));
			if (addedUUIDString.equals(uuid)) {
				return true;
			} else {
				return false;
			}
		}
	}

	/**
	 * This method updates the status and job completion.
	 * 
	 * @param derivation
	 * @param newDerivedIRI
	 */
	void updateStatusAtJobCompletion(String derivation, List<String> newDerivedIRI, List<TriplePattern> newTriples) {
		SelectQuery query = Queries.SELECT();
		ModifyQuery modify = Queries.MODIFY();
		Variable status = query.var();
		Variable statusType = query.var();
		Variable uuid = query.var();

		GraphPattern query_gp = GraphPatterns.and(
			iri(derivation).has(hasStatus, status), status.isA(statusType),
			iri(derivation).has(uuidLock, uuid));
		TriplePattern delete_tp = status.isA(statusType);
		TriplePattern delete_uuid_lock = iri(derivation).has(uuidLock, uuid);
		TriplePattern insert_tp_rdf_type = status.isA(Finished);

		modify.delete(delete_tp, delete_uuid_lock).where(query_gp).prefix(p_derived);
		modify.insert(insert_tp_rdf_type);

		// also add all new generated triples
		newTriples.stream().forEach(t -> modify.insert(t));

		for (String newIRI : newDerivedIRI) {
			modify.insert(status.has(hasNewDerivedIRI, iri(newIRI)));
		}

		storeClient.executeUpdate(modify.getQueryString());
	}

	/**
	 * This method retrieves the rdf:type of the status of the given derivation
	 * instance.
	 * 
	 * @param derivation
	 * @return
	 */
	StatusType getStatusType(String derivation) {
		String statusQueryKey = "status";
		String statusTypeQueryKey = "statusType";
		Variable status = SparqlBuilder.var(statusQueryKey);
		Variable statusType = SparqlBuilder.var(statusTypeQueryKey);

		SelectQuery query = Queries.SELECT();

		// ignore certain rdf:type
		Expression<?>[] entityFilters = new Expression<?>[classesToIgnore.size()];
		for (int j = 0; j < classesToIgnore.size(); j++) {
			entityFilters[j] = Expressions.notEquals(statusType, classesToIgnore.get(j));
		}

		GraphPattern queryPattern = iri(derivation).has(hasStatus, status);
		GraphPattern queryPattern2 = status.isA(statusType).filter(Expressions.and(entityFilters));

		query.prefix(p_derived).where(queryPattern, queryPattern2).select(status, statusType);

		JSONArray queryResult = storeClient.executeQuery(query.getQueryString());

		if (queryResult.isEmpty()) {
			return StatusType.NOSTATUS;
		} else {
			return statusToType.get(queryResult.getJSONObject(0).getString(statusTypeQueryKey));
		}
	}

	/**
	 * This method checks if the derivation has any status assigned to it.
	 * 
	 * @param storeClient
	 * @param derivation
	 * @return
	 */
	@Deprecated
	boolean hasStatus(String derivation) {
		SelectQuery query = Queries.SELECT();

		TriplePattern queryPattern = iri(derivation).has(hasStatus, query.var());
		query.prefix(p_derived).where(queryPattern);

		JSONArray queryResult = storeClient.executeQuery(query.getQueryString());

		if (queryResult.isEmpty()) {
			return false;
		} else {
			return true;
		}
	}

	/**
	 * This method retrieves the new derived IRI of a derivation after the job
	 * completed.
	 * 
	 * @param storeClient
	 * @param derivation
	 * @return
	 */
	List<String> getNewDerivedIRI(String derivation) {
		if (getStatusType(derivation) == StatusType.FINISHED) {
			String derivedQueryKey = "newDerivedIRI";

			SelectQuery query = Queries.SELECT();

			Variable newDerivedIRI = SparqlBuilder.var(derivedQueryKey);

			GraphPattern derivedPattern = iri(derivation).has(PropertyPaths.path(hasStatus, hasNewDerivedIRI),
					newDerivedIRI);

			query.prefix(p_derived).where(derivedPattern).select(newDerivedIRI);
			JSONArray queryResult = storeClient.executeQuery(query.getQueryString());

			List<String> newDerived = new ArrayList<>();

			for (int i = 0; i < queryResult.length(); i++) {
				newDerived.add(queryResult.getJSONObject(i).getString(derivedQueryKey));
			}

			return newDerived;
		} else {
			throw new JPSRuntimeException(
					"Unable to retrieve the new derived IRI as derivation <" + derivation + "> is not finished.");
		}
	}

	/**
	 * returns true of given instance exists
	 * 
	 * @param storeClient
	 * @param instance
	 * @return
	 */
	@Deprecated
	private boolean checkInstanceExists(String instance) {
		SelectQuery query = Queries.SELECT();

		// includes both cases where the instance is a subject and object
		// {<instance> ?x0 ?x1} {?x2 ?x3 <instance>}
		GraphPattern queryPattern = GraphPatterns.and(iri(instance).has(query.var(), query.var()).optional(),
				query.var().has(query.var(), iri(instance)).optional());

		query.where(queryPattern);

		JSONArray queryresult = storeClient.executeQuery(query.getQueryString());

		if (queryresult.getJSONObject(0).isEmpty()) {
			return false;
		} else {
			return true;
		}
	}

	/**
	 * add a time stamp instance to the input or derivation if it does not exist
	 * the derivation uses the unix timestamp
	 * 
	 * @param kbClient
	 * @param input
	 */
	void addTimeInstance(String entity) {
		ModifyQuery modify = Queries.MODIFY();

		// add time stamp instance for the given entity
		String time_instant = derivationInstanceBaseURL + "time" + UUID.randomUUID().toString();
		String time_unix = derivationInstanceBaseURL + "time" + UUID.randomUUID().toString();

		long timestamp = 0;
		Iri time_instant_iri = iri(time_instant);
		Iri time_unix_iri = iri(time_unix);

		// unix time following the w3c standard
		modify.insert(iri(entity).has(hasTime, time_instant_iri));
		modify.insert(time_instant_iri.isA(InstantClass).andHas(inTimePosition, time_unix_iri));
		modify.insert(time_unix_iri.isA(TimePosition).andHas(numericPosition, timestamp).andHas(hasTRS,
				iri("http://dbpedia.org/resource/Unix_time")));

		storeClient.setQuery(modify.prefix(p_time).getQueryString());
		storeClient.executeUpdate();
	}

	/**
	 * same method as above, but update in bulk
	 * 
	 * @param entities
	 */
	void addTimeInstance(List<String> entities) {
		ModifyQuery modify = Queries.MODIFY();

		for (String entity : entities) {
			// add time stamp instance for the given entity
			String time_instant = derivationInstanceBaseURL + "time" + UUID.randomUUID().toString();
			String time_unix = derivationInstanceBaseURL + "time" + UUID.randomUUID().toString();

			long timestamp = 0;
			Iri time_instant_iri = iri(time_instant);
			Iri time_unix_iri = iri(time_unix);

			// unix time following the w3c standard
			modify.insert(iri(entity).has(hasTime, time_instant_iri));
			modify.insert(time_instant_iri.isA(InstantClass).andHas(inTimePosition, time_unix_iri));
			modify.insert(time_unix_iri.isA(TimePosition).andHas(numericPosition, timestamp).andHas(hasTRS,
					iri("http://dbpedia.org/resource/Unix_time")));
		}

		storeClient.executeUpdate(modify.prefix(p_time).getQueryString());
	}

	/**
	 * returns the url of the agent used to calculate the given derived quantity
	 * 
	 * @param kbClient
	 * @param derivedQuantity
	 * @return
	 */
	String getAgentUrl(String derivedQuantity) {
		SelectQuery query = Queries.SELECT();

		String queryKey = "url";
		Variable url = SparqlBuilder.var(queryKey);

		Iri derivedQuantityIRI = iri(derivedQuantity);

		GraphPattern queryPattern = derivedQuantityIRI.has(PropertyPaths.path(isDerivedUsing, hasOperation, hasHttpUrl),
				url);

		query.select(url).where(queryPattern).prefix(p_agent, p_derived);

		storeClient.setQuery(query.getQueryString());

		String queryResult = storeClient.executeQuery().getJSONObject(0).getString(queryKey);

		return queryResult;
	}

	/**
	 * query the list of inputs for the given derived quantity
	 * TODO SPARQL query string duplication with method getDerivations()
	 * TODO To be break down into smaller chunks
	 * 
	 * @param kbClient
	 * @param derivedQuantity
	 * @return
	 */
	List<String> getInputs(String derivedQuantity) {
		String queryKey = "input";
		Variable input = SparqlBuilder.var(queryKey);
		GraphPattern queryPattern = iri(derivedQuantity).has(isDerivedFrom, input);
		SelectQuery query = Queries.SELECT();

		query.prefix(p_derived).where(queryPattern).select(input);
		storeClient.setQuery(query.getQueryString());
		JSONArray queryResult = storeClient.executeQuery();

		List<String> inputs = new ArrayList<>();

		for (int i = 0; i < queryResult.length(); i++) {
			inputs.add(queryResult.getJSONObject(i).getString(queryKey));
		}

		return inputs;
	}

	/**
	 * This method retrieves the agent inputs given the IRI of
	 * OntoDerivation:Derivation instance,
	 * and maps those inputs against the I/O signature declared in the OntoAgent
	 * instance of the agent.
	 * The inputs are finally structured as a JSONObject to be feed into the agent
	 * for execution.
	 * 
	 * @param kbClient
	 * @param derivedQuantity
	 * @param agentIRI
	 * @return
	 */
	JSONObject getInputsMapToAgent(String derivedQuantity, String agentIRI) {
		String typeKey = "type";
		String inputKey = "input";

		Variable type = SparqlBuilder.var(typeKey);
		Variable input = SparqlBuilder.var(inputKey);

		// make use of SPARQL Property Paths
		GraphPattern agentTypePattern = iri(agentIRI)
				.has(PropertyPaths.path(hasOperation, hasInput, hasMandatoryPart, hasType), type);
		GraphPattern derivationInputPattern = iri(derivedQuantity).has(isDerivedFrom, input);
		GraphPattern mappingPattern = input.has(PropertyPaths.path(PropertyPaths.zeroOrMore(RdfPredicate.a),
				PropertyPaths.zeroOrMore(iri(RDFS.SUBCLASSOF.toString()))), type);
		SelectQuery query = Queries.SELECT().distinct();

		query.prefix(p_derived, p_agent).where(agentTypePattern, derivationInputPattern, mappingPattern).select(input,
				type);
		storeClient.setQuery(query.getQueryString());
		JSONArray queryResult = storeClient.executeQuery();

		// construct the JSONObject for agent input
		JSONObject agentInputs = new JSONObject();
		for (int i = 0; i < queryResult.length(); i++) {
			if (agentInputs.has(queryResult.getJSONObject(i).getString(typeKey))) {
				if (agentInputs.get(queryResult.getJSONObject(i).getString(typeKey)) instanceof JSONArray) {
					agentInputs.getJSONArray(queryResult.getJSONObject(i).getString(typeKey))
							.put(queryResult.getJSONObject(i).getString(inputKey));
				} else {
					agentInputs.put(queryResult.getJSONObject(i).getString(typeKey),
							new JSONArray().put(agentInputs.get(queryResult.getJSONObject(i).getString(typeKey))));
					agentInputs.getJSONArray(queryResult.getJSONObject(i).getString(typeKey))
							.put(queryResult.getJSONObject(i).getString(inputKey));
				}
			} else {
				agentInputs.put(queryResult.getJSONObject(i).getString(typeKey),
						new JSONArray().put(queryResult.getJSONObject(i).getString(inputKey)));
			}
		}

		return agentInputs;
	}

	/**
	 * This method maps the given list of inputs against the I/O signature declared
	 * in the OntoAgent instance of the given agent IRI.
	 * The inputs are finally structured as a JSONObject to be feed into the agent
	 * for execution.
	 * 
	 * @param kbClient
	 * @param derivedQuantity
	 * @param agentIRI
	 * @return
	 */
	JSONObject mapInstancesToAgentInputs(List<String> inputs, String agentIRI) {
		String typeKey = "type";
		String inputKey = "input";

		Variable type = SparqlBuilder.var(typeKey);
		Variable input = SparqlBuilder.var(inputKey);

		// make use of SPARQL Property Paths
		GraphPattern agentTypePattern = iri(agentIRI)
				.has(PropertyPaths.path(hasOperation, hasInput, hasMandatoryPart, hasType), type);
		GraphPattern inputValuesPattern = new ValuesPattern(input,
				inputs.stream().map(i -> iri(i)).collect(Collectors.toList()));
		GraphPattern mappingPattern = input.has(PropertyPaths.path(PropertyPaths.zeroOrMore(RdfPredicate.a),
				PropertyPaths.zeroOrMore(iri(RDFS.SUBCLASSOF.toString()))), type);
		SelectQuery query = Queries.SELECT().distinct();

		query.prefix(p_derived, p_agent).where(agentTypePattern, inputValuesPattern, mappingPattern).select(input,
				type);
		storeClient.setQuery(query.getQueryString());
		JSONArray queryResult = storeClient.executeQuery();

		// construct the JSONObject for agent input
		JSONObject agentInputs = new JSONObject();
		for (int i = 0; i < queryResult.length(); i++) {
			if (agentInputs.has(queryResult.getJSONObject(i).getString(typeKey))) {
				if (agentInputs.get(queryResult.getJSONObject(i).getString(typeKey)) instanceof JSONArray) {
					agentInputs.getJSONArray(queryResult.getJSONObject(i).getString(typeKey))
							.put(queryResult.getJSONObject(i).getString(inputKey));
				} else {
					agentInputs.put(queryResult.getJSONObject(i).getString(typeKey),
							new JSONArray().put(agentInputs.get(queryResult.getJSONObject(i).getString(typeKey))));
					agentInputs.getJSONArray(queryResult.getJSONObject(i).getString(typeKey))
							.put(queryResult.getJSONObject(i).getString(inputKey));
				}
			} else {
				agentInputs.put(queryResult.getJSONObject(i).getString(typeKey),
						new JSONArray().put(queryResult.getJSONObject(i).getString(inputKey)));
			}
		}

		return agentInputs;
	}

	/**
	 * This method matches the output instances of the upstream derivation with the
	 * input signature of
	 * the agent that monitors the downstream derivation. The matched instances are
	 * finally structured
	 * as a List<String> to be used for creating the downstream derivation.
	 * 
	 * @param upstreamDerivation
	 * @param agentIRI
	 * @return
	 */
	List<String> retrieveMatchingInstances(String upstreamDerivation, String agentIRI) {
		String typeKey = "type";
		String outputKey = "output";

		Variable type = SparqlBuilder.var(typeKey);
		Variable output = SparqlBuilder.var(outputKey);

		// make use of SPARQL Property Paths
		GraphPattern agentTypePattern = iri(agentIRI)
				.has(PropertyPaths.path(hasOperation, hasInput, hasMandatoryPart, hasType), type);
		GraphPattern derivationOutputPattern = output.has(belongsTo, iri(upstreamDerivation));
		GraphPattern mappingPattern = output.has(PropertyPaths.path(PropertyPaths.zeroOrMore(RdfPredicate.a),
				PropertyPaths.zeroOrMore(iri(RDFS.SUBCLASSOF.toString()))), type);
		SelectQuery query = Queries.SELECT().distinct();

		query.prefix(p_derived, p_agent).where(agentTypePattern, derivationOutputPattern, mappingPattern).select(output,
				type);
		// query.prefix(p_derived,p_agent).where(agentTypePattern,derivationOutputPattern).select(output,type);
		storeClient.setQuery(query.getQueryString());
		JSONArray queryResult = storeClient.executeQuery();
		List<String> matchingInstances = new ArrayList<>();
		for (int i = 0; i < queryResult.length(); i++) {
			matchingInstances.add(queryResult.getJSONObject(i).getString(outputKey));
		}
		return matchingInstances;
	}

	/**
	 * This method matches the new derived instances of the derivation with the
	 * input signature of the downstream derivations that directly connected to that
	 * derivation via OntoDerivation:isDerivedFrom. The matched instances are
	 * finally structured as a Map<String, List<String>> to be used for reconnecting
	 * the new derived instances with the downstream derivations created for new
	 * information.
	 * 
	 * @param instances
	 * @param downstreamDerivationsForNewInfo
	 * @return
	 */
	Map<String, List<String>> matchNewDerivedIriToDownsFroNewInfo(List<String> instances,
			List<String> downstreamDerivationsForNewInfo) {
		Map<String, List<String>> map = new HashMap<>();
		SelectQuery query = Queries.SELECT().distinct();
		Variable derivation = query.var();
		Variable type = query.var();
		Variable instance = query.var();

		GraphPattern matchingPattern = GraphPatterns.and(new ValuesPattern(derivation,
				downstreamDerivationsForNewInfo.stream().map(i -> iri(i)).collect(Collectors.toList())),
				derivation.has(PropertyPaths.path(isDerivedUsing, hasOperation, hasInput, hasMandatoryPart, hasType),
						type),
				new ValuesPattern(instance, instances.stream().map(i -> iri(i)).collect(Collectors.toList())),
				instance.has(PropertyPaths.path(PropertyPaths.zeroOrMore(RdfPredicate.a),
						PropertyPaths.zeroOrMore(iri(RDFS.SUBCLASSOF.toString()))), type));

		query.prefix(p_derived, p_agent).where(matchingPattern).select(instance, derivation);

		JSONArray queryResult = storeClient.executeQuery(query.getQueryString());

		for (int i = 0; i < queryResult.length(); i++) {
			String inst = queryResult.getJSONObject(i).getString(instance.getQueryString().substring(1));
			String deriv = queryResult.getJSONObject(i).getString(derivation.getQueryString().substring(1));
			if (map.containsKey(inst)) {
				map.get(inst).add(deriv);
			} else {
				map.put(inst, new ArrayList<>(Arrays.asList(deriv)));
			}
		}
		return map;
	}

	/**
	 * This method retrieves a list of derivations that <isDerivedUsing> a given
	 * <agentIRI>.
	 * 
	 * @param kbClient
	 * @param agentIRI
	 * @return
	 */
	List<String> getDerivations(String agentIRI) {
		String queryKey = "derivation";
		Variable derivation = SparqlBuilder.var(queryKey);
		GraphPattern queryPattern = derivation.has(isDerivedUsing, iri(agentIRI));
		SelectQuery query = Queries.SELECT();

		query.prefix(p_derived, p_agent).select(derivation).where(queryPattern);
		storeClient.setQuery(query.getQueryString());
		JSONArray queryResult = storeClient.executeQuery();

		List<String> derivations = new ArrayList<>();
		for (int i = 0; i < queryResult.length(); i++) {
			derivations.add(queryResult.getJSONObject(i).getString(queryKey));
		}

		return derivations;
	}

	/**
	 * This method retrieves a mapped list of derivations that <isDerivedUsing> a
	 * given <agentIRI> and their statusType.
	 * 
	 * @param agentIRI
	 * @return
	 */
	Map<String, StatusType> getDerivationsAndStatusType(String agentIRI) {
		String queryKey = "derivation";
		String statusQueryKey = "status";
		String statusTypeQueryKey = "statusType";

		Variable derivation = SparqlBuilder.var(queryKey);
		Variable status = SparqlBuilder.var(statusQueryKey);
		Variable statusType = SparqlBuilder.var(statusTypeQueryKey);

		// ignore certain rdf:type
		Expression<?>[] entityFilters = new Expression<?>[classesToIgnore.size()];
		for (int j = 0; j < classesToIgnore.size(); j++) {
			entityFilters[j] = Expressions.notEquals(statusType, classesToIgnore.get(j));
		}

		GraphPattern queryPattern = derivation.has(isDerivedUsing, iri(agentIRI)).andIsA(DerivationAsyn);
		GraphPatternNotTriples optionalPattern = GraphPatterns.optional(GraphPatterns
				.and(derivation.has(hasStatus, status), status.isA(statusType).filter(Expressions.and(entityFilters))));

		SelectQuery query = Queries.SELECT();

		query.prefix(p_derived, p_agent).select(derivation, statusType).where(queryPattern, optionalPattern);
		storeClient.setQuery(query.getQueryString());
		JSONArray queryResult = storeClient.executeQuery();

		Map<String, StatusType> derivationsAndStatusType = new HashMap<>();
		for (int i = 0; i < queryResult.length(); i++) {
			if (queryResult.getJSONObject(i).has(statusTypeQueryKey)) {
				derivationsAndStatusType.put(queryResult.getJSONObject(i).getString(queryKey),
						statusToType.get(queryResult.getJSONObject(i).getString(statusTypeQueryKey)));
			} else {
				derivationsAndStatusType.put(queryResult.getJSONObject(i).getString(queryKey), StatusType.NOSTATUS);
			}
		}

		return derivationsAndStatusType;
	}

	/**
	 * This method retrieves a list of upstream derivations and their rdf:type that
	 * directly linked with the given derivation in the chain and need an update.
	 * 
	 * @param derivation
	 * @return
	 */
	Map<String, List<String>> getUpstreamDerivationsNeedUpdate(String derivation) {
		String upsDevQueryKey = "upstreamDerivation";
		String upsDevTypeQueryKey = "upstreamDerivationType";
		String upsDevTimeQueryKey = "upstreamDerivationTimestamp";
		String statusQueryKey = "status";
		String statusTypeQueryKey = "statusType";
		String pureInputTimeQueryKey = "pureInputTimestamp";
		String inputsBelongingToDevTimeQueryKey = "inputsBelongingToDerivationTimestamp";

		SelectQuery query = Queries.SELECT().distinct();

		Variable upstreamDerivation = SparqlBuilder.var(upsDevQueryKey);
		Variable upstreamDerivationType = SparqlBuilder.var(upsDevTypeQueryKey);
		Variable upstreamDerivationTimestamp = SparqlBuilder.var(upsDevTimeQueryKey);
		Variable status = SparqlBuilder.var(statusQueryKey);
		Variable statusType = SparqlBuilder.var(statusTypeQueryKey);
		Variable pureInputTimestamp = SparqlBuilder.var(pureInputTimeQueryKey);
		Variable inputsBelongingToDerivationTimestamp = SparqlBuilder.var(inputsBelongingToDevTimeQueryKey);

		// ignore certain rdf:type
		Expression<?>[] entityFilters = new Expression<?>[classesToIgnore.size()];
		for (int j = 0; j < classesToIgnore.size(); j++) {
			entityFilters[j] = Expressions.notEquals(statusType, classesToIgnore.get(j));
		}

		// check if the upstreamDerivation (outdated timestamp compared to pure input ||
		// outdated timestamp compared to its own upstream derivations || has status)
		Expression<?> upstreamDerivationFilter = Expressions.or(
				// ?upstreamDerivationTimestamp < ?pureInputTimestamp
				Expressions.lt(upstreamDerivationTimestamp, pureInputTimestamp),
				// ?upstreamDerivationTimestamp < ?inputsBelongingToDerivationTimestamp
				Expressions.lt(upstreamDerivationTimestamp, inputsBelongingToDerivationTimestamp),
				// ?statusType IN (derived:Requested, derived:InProgress, derived:Finished)
				Expressions.equals(statusType, Requested),
				Expressions.equals(statusType, InProgress),
				Expressions.equals(statusType, Finished));

		GraphPattern upstreamDerivationPattern = GraphPatterns.and(
				iri(derivation).has(PropertyPaths.path(isDerivedFrom, zeroOrOne(belongsTo)),
						upstreamDerivation),
				upstreamDerivation.isA(upstreamDerivationType),
				new ValuesPattern(upstreamDerivationType,
						derivationTypes.stream().map(i -> derivationToIri.get(i)).collect(Collectors.toList())));
		GraphPattern upDevTimePattern = upstreamDerivation
				.has(PropertyPaths.path(hasTime, inTimePosition, numericPosition), upstreamDerivationTimestamp);
		GraphPattern upDevStatusTypePattern = GraphPatterns
				.optional(GraphPatterns.and(upstreamDerivation.has(hasStatus, status),
						status.isA(statusType).filter(Expressions.and(entityFilters))));
		GraphPattern upDevPureInputTimePattern = upstreamDerivation
				.has(PropertyPaths.path(isDerivedFrom, hasTime, inTimePosition, numericPosition), pureInputTimestamp)
				.optional();
		GraphPattern inputsBelongsToDevTimePattern = upstreamDerivation
				.has(PropertyPaths.path(isDerivedFrom, belongsTo, hasTime, inTimePosition, numericPosition),
						inputsBelongingToDerivationTimestamp)
				.optional();

		// Complete query string:
		// PREFIX derived:
		// <https://raw.githubusercontent.com/cambridge-cares/TheWorldAvatar/main/JPS_Ontology/ontology/ontoderivation/OntoDerivation.owl#>
		// PREFIX time: <http://www.w3.org/2006/time#>
		// SELECT DISTINCT ?upstreamDerivation ?upstreamDerivationType
		// WHERE {
		// <derivation> derived:isDerivedFrom/derived:belongsTo? ?upstreamDerivation.
		// ?upstreamDerivation a ?upstreamDerivationType.
		// VALUES ?upstreamDerivationType {derived:DerivationAsyn derived:Derivation
		// derived:DerivationWithTimeSeries}
		// ?upstreamDerivation time:hasTime/time:inTimePosition/time:numericPosition
		// ?upstreamDerivationTimestamp.
		// OPTIONAL{?upstreamDerivation derived:hasStatus ?status .
		// {?status a ?statusType .
		// FILTER((?statusType != <http://www.w3.org/2002/07/owl#Thing> && ?statusType
		// != <http://www.w3.org/2002/07/owl#NamedIndividual>))
		// }
		// }
		// OPTIONAL{?upstreamDerivation
		// derived:isDerivedFrom/time:hasTime/time:inTimePosition/time:numericPosition
		// ?pureInputTimestamp}
		// OPTIONAL{?upstreamDerivation
		// derived:isDerivedFrom/derived:belongsTo/time:hasTime/time:inTimePosition/time:numericPosition
		// ?inputsBelongingToDerivationTimestamp}
		// FILTER((?upstreamDerivationTimestamp < ?pureInputTimestamp ||
		// ?upstreamDerivationTimestamp < ?inputsBelongingToDerivationTimestamp ||
		// ?statusType = derived:Requested || ?statusType = derived:InProgress ||
		// ?statusType = derived:Finished))
		// }

		// it should be noted that the final FILTER has a simplified version but was not
		// implemented as IN operator was not found in SparqlBuilder (to the best of the
		// author's knowledge):
		// FILTER(?upstreamDerivationTimestamp < ?pureInputTimestamp ||
		// ?upstreamDerivationTimestamp < ?inputsBelongingToDerivationTimestamp ||
		// ?statusType IN (derived:Requested, derived:InProgress, derived:Finished))

		query.prefix(p_derived, p_time).select(upstreamDerivation, upstreamDerivationType)
				.where(GraphPatterns
						.and(upstreamDerivationPattern, upDevTimePattern, upDevStatusTypePattern,
								upDevPureInputTimePattern, inputsBelongsToDevTimePattern)
						.filter(upstreamDerivationFilter));

		JSONArray queryResult = storeClient.executeQuery(query.getQueryString());

		Map<String, List<String>> upstreamDerivationMap = new HashMap<>();

		for (int i = 0; i < queryResult.length(); i++) {
			String derivedIRI = queryResult.getJSONObject(i).getString(upsDevQueryKey);
			String derivationType = queryResult.getJSONObject(i).getString(upsDevTypeQueryKey);
			if (!upstreamDerivationMap.containsKey(derivationType)) {
				upstreamDerivationMap.put(derivationType, new ArrayList<>(Arrays.asList(derivedIRI)));
			} else {
				upstreamDerivationMap.get(derivationType).add(derivedIRI);
			}
		}

		return upstreamDerivationMap;
	}

	/**
	 * This method marks the status of the derivation as "Requested" if the
	 * derivation is outdated and there's no status marked already.
	 * 
	 * A derivation is outdated IN FACT if:
	 * (1) its timestamp is outdated (smaller) compared to the timestamp of its
	 * upstream derivations or pure inputs; (2) any of its upstream derivations
	 * have status (the derivation might still up-to-date when comparing the
	 * timestamp, but its timestamp will be outdated once the update of upstream
	 * derivations are done, thus it is in fact outdated from a global view).
	 * 
	 * @param derivationIRI
	 */
	void markAsRequestedIfOutdated(String derivationIRI) {
		// complete query string
		// PREFIX time: <http://www.w3.org/2006/time#>
		// PREFIX derived:
		// <https://raw.githubusercontent.com/cambridge-cares/TheWorldAvatar/main/JPS_Ontology/ontology/ontoderivation/OntoDerivation.owl#>
		// INSERT { ?derivation derived:hasStatus <statusIRI> .
		// <statusIRI> a derived:Requested . }
		// WHERE { { SELECT ?derivation
		// WHERE { VALUES ?derivation { <derivationIRI> }
		// ?derivation time:hasTime/time:inTimePosition/time:numericPosition
		// ?derivationTimestamp .
		// ?derivation derived:isDerivedFrom/derived:belongsTo? ?upstream .
		// ?upstream time:hasTime/time:inTimePosition/time:numericPosition
		// ?upstreamTimestamp .
		// OPTIONAL { ?upstream derived:hasStatus/a ?upsStatusType . }
		// FILTER NOT EXISTS { ?derivation derived:hasStatus ?derivationStatus . }
		// FILTER ( ( ?derivationTimestamp < ?upstreamTimestamp || ?upsStatusType =
		// derived:Requested || ?upsStatusType = derived:InProgress || ?upsStatusType =
		// derived:Finished ) ) }
		// } }

		SubSelect sub = GraphPatterns.select();
		ModifyQuery modify = Queries.MODIFY();

		Variable d = SparqlBuilder.var("derivation");
		Variable dTs = SparqlBuilder.var("derivationTimestamp");
		Variable dStatus = SparqlBuilder.var("derivationStatus");
		Variable upstream = SparqlBuilder.var("upstream");
		Variable upsTs = SparqlBuilder.var("upstreamTimestamp");
		Variable upsStatusType = SparqlBuilder.var("upsStatusType");

		// check if the derivation (outdated timestamp compared to its upstream
		// inputs/derivations || its upstream derivations have status)
		Expression<?> upstreamFilter = Expressions.or(
				// ?derivationTimestamp < ?upstreamTimestamp
				Expressions.lt(dTs, upsTs),
				// ?statusType IN (derived:Requested, derived:InProgress, derived:Finished)
				Expressions.equals(upsStatusType, Requested),
				Expressions.equals(upsStatusType, InProgress),
				Expressions.equals(upsStatusType, Finished));

		GraphPattern derivationIRIPattern = new ValuesPattern(d, Arrays.asList(iri(derivationIRI)));
		GraphPattern derivationTimePattern = d.has(PropertyPaths.path(hasTime, inTimePosition, numericPosition), dTs);
		GraphPattern upstreamPattern = d.has(PropertyPaths.path(isDerivedFrom, zeroOrOne(belongsTo)), upstream);
		GraphPattern upstreamTimePattern = upstream
				.has(PropertyPaths.path(hasTime, inTimePosition, numericPosition), upsTs);
		GraphPattern upstreamStatusTypePattern = GraphPatterns
				.and(upstream.has(PropertyPaths.path(hasStatus, RdfPredicate.a), upsStatusType)).optional();
		GraphPattern derivationStatusPattern = d.has(hasStatus, dStatus);

		sub.select(d).where(GraphPatterns.and(derivationIRIPattern, derivationTimePattern, upstreamPattern,
				upstreamTimePattern, upstreamStatusTypePattern).filter(upstreamFilter)
				.filterNotExists(derivationStatusPattern));

		String statusIRI = getNameSpace(derivationIRI) + "status_" + UUID.randomUUID().toString();

		TriplePattern insert_status = d.has(hasStatus, iri(statusIRI));
		TriplePattern insert_status_rdf_type = iri(statusIRI).isA(Requested);

		modify.prefix(p_time, p_derived).insert(insert_status, insert_status_rdf_type).where(sub);

		storeClient.executeUpdate(modify.getQueryString());
	}

	/**
	 * This is used at the stage to detect circular dependency
	 * if the input is part of a derived quantity, this will add the derived
	 * instance
	 * if the input is not a derived instance, it will add the input itself
	 * TODO SPARQL query string duplication with method getDerivations()
	 * TODO To be break down into smaller chunks
	 * 
	 * @param kbClient
	 * @param derivedQuantity
	 */
	List<String> getInputsAndDerived(String derived) {
		String inputQueryKey = "input";
		String derivedQueryKey = "derived";

		SelectQuery query = Queries.SELECT();

		Variable input = SparqlBuilder.var(inputQueryKey);
		Variable derivedOfInput = SparqlBuilder.var(derivedQueryKey); // derived instance for the input of entity

		// direct inputs to derive this
		GraphPattern inputPattern = GraphPatterns.and(iri(derived).has(isDerivedFrom, input));
		// some inputs may be a part of a derived instance
		// this is also added to the list to ensure that there are no circular
		// dependencies via a different entity within the same derived instance
		GraphPattern derivedPattern = input.has(belongsTo, derivedOfInput).optional();

		query.prefix(p_derived).where(inputPattern, derivedPattern).select(input, derivedOfInput);
		JSONArray queryResult = storeClient.executeQuery(query.getQueryString());

		List<String> inputsAndDerived = new ArrayList<>();

		for (int i = 0; i < queryResult.length(); i++) {
			// some inputs may be a derived quantity
			String derivedIRI = queryResult.getJSONObject(i).optString(derivedQueryKey);
			if (derivedIRI.length() > 0) {
				inputsAndDerived.add(derivedIRI);
			} else {
				inputsAndDerived.add(queryResult.getJSONObject(i).getString(inputQueryKey));
			}
		}

		return inputsAndDerived;
	}

	/**
	 * returns entities belonging to this derived instance, ?x <derived:belongsTo>
	 * <derivedIRI>
	 * 
	 * @param kbClient
	 * @param derivedIRI
	 * @return
	 */
	@Deprecated
	List<String> getDerivedEntities(String derivedIRI) {
		SelectQuery query = Queries.SELECT();
		String queryKey = "entity";
		Variable entity = SparqlBuilder.var(queryKey);
		GraphPattern queryPattern = entity.has(belongsTo, iri(derivedIRI));
		query.prefix(p_derived).select(entity).where(queryPattern);

		JSONArray queryResult = storeClient.executeQuery(query.getQueryString());

		List<String> entities = new ArrayList<>();
		for (int i = 0; i < queryResult.length(); i++) {
			entities.add(queryResult.getJSONObject(i).getString(queryKey));
		}

		return entities;
	}

	/**
	 * This method returns entities belonging to this derived instance
	 * ?x <derived:belongsTo> <derivation>.
	 * ?x <rdf:type> ?rdfType.
	 * ?downstreamDerivation <derived:isDerivedFrom> ?x.
	 * TODO SPARQL query string duplication with method getDerivations()
	 * TODO To be break down into smaller chunks
	 * 
	 * @param kbClient
	 * @param derivedIRI
	 * @return
	 */
	@Deprecated
	List<Entity> getDerivedEntitiesAndDownstreamDerivation(String derivation) {
		SelectQuery query = Queries.SELECT();
		String entityQueryKey = "entity";
		String rdfTypeQueryKey = "class";
		String downstreamDevQueryKey = "downstreamDerivation";
		String downsDevRdfTypeQueryKey = "downsDevClass";

		Variable entity = SparqlBuilder.var(entityQueryKey);
		Variable rdfType = SparqlBuilder.var(rdfTypeQueryKey);
		Variable downstreamDerivation = SparqlBuilder.var(downstreamDevQueryKey);
		Variable downsDevRdfType = SparqlBuilder.var(downsDevRdfTypeQueryKey);

		// ignore certain rdf:type
		Expression<?>[] entityFilters = new Expression<?>[classesToIgnore.size()];
		for (int j = 0; j < classesToIgnore.size(); j++) {
			entityFilters[j] = Expressions.notEquals(rdfType, classesToIgnore.get(j));
		}
		Expression<?>[] downsDevFilters = new Expression<?>[classesToIgnore.size()];
		for (int j = 0; j < classesToIgnore.size(); j++) {
			downsDevFilters[j] = Expressions.notEquals(downsDevRdfType, classesToIgnore.get(j));
		}

		GraphPattern queryPattern = entity.has(belongsTo, iri(derivation)).andIsA(rdfType)
				.filter(Expressions.and(entityFilters));
		GraphPattern downstreamDevPattern = downstreamDerivation.has(isDerivedFrom, entity).andIsA(downsDevRdfType)
				.filter(Expressions.and(downsDevFilters));
		query.prefix(p_derived).select(entity, rdfType, downstreamDerivation, downsDevRdfType).where(queryPattern,
				downstreamDevPattern);

		JSONArray queryResult = storeClient.executeQuery(query.getQueryString());

		Map<String, Entity> entityMap = new HashMap<>();
		for (int i = 0; i < queryResult.length(); i++) {
			String eiri = queryResult.getJSONObject(i).getString(entityQueryKey);
			if (!entityMap.containsKey(eiri)) {
				Entity e = new Entity(eiri);
				e.setRdfType(queryResult.getJSONObject(i).getString(rdfTypeQueryKey));
				e.setAsInput(new Derivation(queryResult.getJSONObject(i).getString(downstreamDevQueryKey),
					queryResult.getJSONObject(i).getString(downsDevRdfTypeQueryKey)));
				entityMap.put(eiri, e);
			} else {
				entityMap.get(eiri)
						.setAsInput(new Derivation(queryResult.getJSONObject(i).getString(downstreamDevQueryKey),
								queryResult.getJSONObject(i).getString(downsDevRdfTypeQueryKey)));
			}
		}

		return new ArrayList<Entity>(entityMap.values());
	}

	Map<String, String> getDownstreamDerivationForNewInfo(String derivation) {
		SelectQuery query = Queries.SELECT().distinct();
		Variable downstream = query.var();
		Variable downstreamType = query.var();
		Variable agentIRI = query.var();

		GraphPattern queryPattern = GraphPatterns.and(
			downstream.has(isDerivedFrom, iri(derivation))
			.andHas(RdfPredicate.a, downstreamType)
			.andHas(isDerivedUsing, agentIRI),
			new ValuesPattern(downstreamType, Arrays.asList(DerivationAsyn)));

		query.prefix(p_derived, p_agent).select(downstream, agentIRI).where(queryPattern);

		JSONArray queryResult = storeClient.executeQuery(query.getQueryString());
		Map<String, String> downstreamDerivations = new HashMap<>();
		for (int i = 0; i < queryResult.length(); i++) {
			downstreamDerivations.put(
				queryResult.getJSONObject(i).getString(downstream.getQueryString().substring(1)),
				queryResult.getJSONObject(i).getString(agentIRI.getQueryString().substring(1)));
		}
		return downstreamDerivations;
	}

	/**
	 * returns the derived instance, where the given entity is an input to it
	 * <derived> <derived:isDerivedFrom> <entity>
	 * 
	 * @param kbClient
	 * @param entities
	 */
	@Deprecated
	List<List<String>> getIsDerivedFromEntities(List<String> entities) {
		String derivedkey = "derived";
		String typeKey = "type";
		Variable derived = SparqlBuilder.var(derivedkey);
		Variable entityType = SparqlBuilder.var(typeKey);

		// ignore certain rdf:type (e.g. OWL.namedInvididual)
		Expression<?>[] filters = new Expression<?>[classesToIgnore.size()];
		for (int j = 0; j < classesToIgnore.size(); j++) {
			filters[j] = Expressions.notEquals(entityType, classesToIgnore.get(j));
		}

		List<List<String>> derivedAndEntityType = new ArrayList<>();
		List<String> derivediri = new ArrayList<>();
		List<String> typeiri = new ArrayList<>();

		for (String entity : entities) {
			SelectQuery query = Queries.SELECT();

			GraphPattern queryPattern = GraphPatterns
					.and(derived.has(isDerivedFrom, iri(entity)), iri(entity).isA(entityType))
					.filter(Expressions.and(filters)).optional();

			query.select(derived, entityType).where(queryPattern).prefix(p_derived);
			JSONArray queryResult = storeClient.executeQuery(query.getQueryString());

			// do not attempt to get value if this instance is not an input to any other
			// derived quantities
			if (!queryResult.getJSONObject(0).isEmpty()) {
				for (int j = 0; j < queryResult.length(); j++) {
					derivediri.add(queryResult.getJSONObject(j).getString(derivedkey));
					typeiri.add(queryResult.getJSONObject(j).getString(typeKey));
				}
			}
		}

		derivedAndEntityType.add(derivediri);
		derivedAndEntityType.add(typeiri);

		return derivedAndEntityType;
	}

	/**
	 * remove all triples connected to the given entities
	 * {<entity> ?x ?y} and {?x ?y <entity>}
	 * 
	 * @param kbClient
	 * @param entities
	 */
	@Deprecated
	void deleteInstances(List<String> entities) {
		for (String entity : entities) {
			SubSelect sub = GraphPatterns.select();

			Variable subject = sub.var();
			Variable pred1 = sub.var();
			Variable pred2 = sub.var();
			Variable object = sub.var();

			TriplePattern[] delete_tp = new TriplePattern[2];
			GraphPattern[] queryPattern = new GraphPattern[2];

			// case 1: entity is the object
			delete_tp[0] = subject.has(pred1, iri(entity));
			queryPattern[0] = delete_tp[0].optional();

			// case 2: entity is the subject
			delete_tp[1] = iri(entity).has(pred2, object);
			queryPattern[1] = delete_tp[1].optional();

			sub.select(subject, pred1, pred2, object).where(queryPattern);

			ModifyQuery modify = Queries.MODIFY();
			modify.delete(delete_tp).where(sub);

			storeClient.executeUpdate(modify.getQueryString());
		}
	}

	/**
	 * This methods deletes the status of a derivation instance.
	 * 
	 * @param kbClient
	 * @param instance
	 */
	@Deprecated
	void deleteStatus(String derivation) {
		SelectQuery query = Queries.SELECT();
		Variable status = query.var();
		Variable type = query.var();
		Variable newDerivedIRI = query.var();

		TriplePattern tp1 = iri(derivation).has(hasStatus, status);
		TriplePattern tp2 = status.isA(type);
		TriplePattern tp3 = status.has(hasNewDerivedIRI, newDerivedIRI);
		GraphPattern gp = status.has(hasNewDerivedIRI, newDerivedIRI).optional();

		ModifyQuery modify = Queries.MODIFY();
		modify.delete(tp1, tp2, tp3).where(tp1, tp2, gp).prefix(p_derived);

		storeClient.executeUpdate(modify.getQueryString());
	}

	/**
	 * This methods retrieves the timestamp of a derivation instance.
	 * TODO SPARQL query string duplication with method getDerivations()
	 * TODO To be break down into smaller chunks
	 * 
	 * @param kbClient
	 * @param instance
	 */
	long getTimestamp(String instance) {
		String queryKey = "timestamp";
		SelectQuery query = Queries.SELECT();
		Variable time = SparqlBuilder.var(queryKey);

		Iri instanceIRI = iri(instance);

		// here we try to match two triple patterns
		// type 1: this is an input with a time stamp directly attached to it
		// type 2: this is an input that is part of a derived quantity
		// instances with timestamps directly attached
		Iri[] predicates = { hasTime, inTimePosition, numericPosition };
		GraphPattern queryPattern = instanceIRI.has(PropertyPaths.path(predicates), time).optional();

		// instances that do not have time stamp directly attached, but belongs to a
		// derived instance
		Iri[] predicates2 = { belongsTo, hasTime, inTimePosition, numericPosition };
		GraphPattern queryPattern2 = instanceIRI.has(PropertyPaths.path(predicates2), time).optional();

		query.prefix(p_time, p_derived).where(queryPattern, queryPattern2).select(time);

		JSONArray queryResult = storeClient.executeQuery(query.getQueryString());

		if (queryResult.length() > 1) {
			throw new JPSRuntimeException(
					"DerivedQuantitySparql: More than 1 time instance associated with <" + instance + ">");
		}

		try {
			long timestamp = queryResult.getJSONObject(0).getLong(queryKey);
			return timestamp;
		} catch (JSONException e) {
			throw new JPSRuntimeException("No timestamp for <" + instance
					+ ">. This is probably an input and you should consider "
					+ "adding a timestamp using DerivationClient.addTimeInstance or create a derived instance using this instance");
		}
	}

	/**
	 * update time stamp for a given IRI of an instance
	 * 
	 * @param kbClient
	 * @param instance
	 */
	long updateTimeStamp(String instance) {
		long timestamp = Instant.now().getEpochSecond();

		// obtain time IRI through sub query
		SubSelect sub = GraphPatterns.select();

		Variable timeIRI = sub.var();
		Variable unixtimeIRI = SparqlBuilder.var("timeIRI");
		Variable oldvalue = SparqlBuilder.var("oldvalue");

		GraphPattern queryPattern = GraphPatterns.and(iri(instance).has(hasTime, timeIRI),
				timeIRI.has(inTimePosition, unixtimeIRI),
				unixtimeIRI.has(numericPosition, oldvalue));

		// triples to add and delete
		TriplePattern delete_tp = unixtimeIRI.has(numericPosition, oldvalue);
		TriplePattern insert_tp = unixtimeIRI.has(numericPosition, timestamp);

		sub.select(unixtimeIRI, oldvalue).where(queryPattern);

		ModifyQuery modify = Queries.MODIFY();
		modify.prefix(p_time).delete(delete_tp).insert(insert_tp).where(sub);

		storeClient.setQuery(modify.getQueryString());
		storeClient.executeUpdate();

		return timestamp;
	}

	/**
	 * returns rdf:type of the given instance, ignoring owl:NamedIndividual and
	 * owl:Thing
	 * when an instance is part of an input to another derived instance, the type is
	 * used to reconnect the appropriate instance
	 * 
	 * @param kbClient
	 * @param instance
	 * @return
	 */
	@Deprecated
	List<String> getInstanceClass(List<String> instances) {
		String queryKey = "class";

		List<String> classOfInstances = new ArrayList<>(instances.size());

		for (int i = 0; i < instances.size(); i++) {
			SelectQuery query = Queries.SELECT();
			Variable type = SparqlBuilder.var(queryKey);

			// ignore certain rdf:type
			Expression<?>[] filters = new Expression<?>[classesToIgnore.size()];
			for (int j = 0; j < classesToIgnore.size(); j++) {
				filters[j] = Expressions.notEquals(type, classesToIgnore.get(j));
			}
			GraphPattern queryPattern = iri(instances.get(i)).isA(type).filter(Expressions.and(filters));

			query.select(type).where(queryPattern);
			storeClient.setQuery(query.getQueryString());

			JSONArray queryResult = storeClient.executeQuery();
			// not having an rdf:type may be fine, but having more than 1 is an issue
			if (queryResult.length() > 1) {
				throw new JPSRuntimeException(
						"DerivedQuantitySparql.getInstanceClass: more than 1 rdf:type for " + instances.get(i));
			} else if (queryResult.length() == 1) {
				classOfInstances.add(i, queryResult.getJSONObject(0).getString(queryKey));
			} else {
				classOfInstances.add(i, "");
			}
		}
		return classOfInstances;
	}

	/**
	 * This method retrives a list of new entities generated by the DerivationAgent.
	 * 
	 * @param instances
	 * @return
	 */
	@Deprecated
	List<Entity> initialiseNewEntities(String derivationIRI) {
		// query rdf types of these new instances
		SelectQuery query = Queries.SELECT();
		Variable type = query.var();
		Variable instance = query.var();

		// ignore certain rdf:type
		Expression<?>[] filters = new Expression<?>[classesToIgnore.size()];
		for (int j = 0; j < classesToIgnore.size(); j++) {
			filters[j] = Expressions.notEquals(type, classesToIgnore.get(j));
		}

		GraphPattern instancePattern = GraphPatterns.and(instance.has(belongsTo, iri(derivationIRI)),
				instance.isA(type).filter(Expressions.and(filters)));

		query.prefix(p_derived).select(type, instance).where(instancePattern);

		JSONArray queryResult = storeClient.executeQuery(query.getQueryString());

		List<Entity> newEntities = new ArrayList<>();
		for (int i = 0; i < queryResult.length(); i++) {
			String iri = queryResult.getJSONObject(i).getString(instance.getQueryString().substring(1));

			if (newEntities.stream().anyMatch(e -> e.getIri().equals(iri))) {
				throw new JPSRuntimeException(
						"DerivedQuantitySparql.getInstanceClass: more than 1 rdf:type for " + iri);
			}

			Entity entity = new Entity(iri);
			if (queryResult.getJSONObject(i).has(type.getQueryString().substring(1))) {
				entity.setRdfType(queryResult.getJSONObject(i).getString(type.getQueryString().substring(1)));
			}
			newEntities.add(entity);
		}

		return newEntities;
	}

	/**
	 * this is used to reconnect a newly created instance to an existing derived
	 * instance
	 * 
	 * @param kbClient
	 * @param input
	 * @param derived
	 */
	@Deprecated
	void reconnectInputToDerived(String input, String derived) {
		ModifyQuery modify = Queries.MODIFY();

		TriplePattern insert_tp = iri(derived).has(isDerivedFrom, iri(input));

		modify.prefix(p_derived).insert(insert_tp);

		storeClient.executeUpdate(modify.getQueryString());
	}

	/**
	 * this is used to reconnect a newly created instance to an existing derived
	 * instance
	 * 
	 * @param kbClient
	 * @param input
	 * @param derived
	 */
	@Deprecated
	void reconnectInputToDerived(List<String> inputs, List<String> derivations) {
		if (inputs.size() != derivations.size()) {
			throw new JPSRuntimeException("reconnectInputToDerived has incorrect inputs");
		}

		ModifyQuery modify = Queries.MODIFY();

		for (int i = 0; i < inputs.size(); i++) {
			modify.insert(iri(derivations.get(i)).has(isDerivedFrom, iri(inputs.get(i))));
		}
		modify.prefix(p_derived);

		storeClient.executeUpdate(modify.getQueryString());
	}

	/**
	 * This method updates the timestamp of the given derivation with given value,
	 * also delete the status of the given derivation if the status exist.
	 * 
	 * @param inputs
	 * @param derivations
	 * @param derivation
	 * @param timestamp
	 */
	void updateTimestampDeleteStatus(String derivation, Long timestamp) {
		ModifyQuery modify = Queries.MODIFY();

		// obtain time IRI and status IRI through sub query
		SubSelect sub = GraphPatterns.select();

		Variable timeIRI = sub.var();
		Variable unixtimeIRI = SparqlBuilder.var("timeIRI");
		Variable oldvalue = SparqlBuilder.var("oldvalue");
		Variable status = SparqlBuilder.var("status");
		Variable type = SparqlBuilder.var("statusType");
		Variable newDerivedIRI = SparqlBuilder.var("newDerivedIRI");

		// timestamp query pattern
		GraphPattern tsQueryPattern = GraphPatterns.and(iri(derivation).has(hasTime, timeIRI),
				timeIRI.has(inTimePosition, unixtimeIRI),
				unixtimeIRI.has(numericPosition, oldvalue));
		// status-related patterns
		TriplePattern tp1 = iri(derivation).has(hasStatus, status);
		TriplePattern tp2 = status.isA(type);
		TriplePattern tp3 = status.has(hasNewDerivedIRI, newDerivedIRI);
		GraphPattern statusQueryPattern = GraphPatterns.and(tp1, tp2, tp3.optional());

		// timestamp-related triples to add and delete
		TriplePattern delete_tp = unixtimeIRI.has(numericPosition, oldvalue);
		TriplePattern insert_tp = unixtimeIRI.has(numericPosition, timestamp);

		sub.select(unixtimeIRI, oldvalue, status, type, newDerivedIRI).where(tsQueryPattern,
				statusQueryPattern.optional()); // statusQueryPattern is made optional

		modify.prefix(p_time, p_derived).delete(delete_tp, tp1, tp2, tp3).insert(insert_tp).where(sub);

		storeClient.setQuery(modify.getQueryString());
		storeClient.executeUpdate();
	}

	@Deprecated
	void deleteDirectConnectionBetweenDerivations(Map<String, String> derivationPairMap) {
		ModifyQuery modify = Queries.MODIFY();
		derivationPairMap.forEach((downstream, upstream) -> {
			modify.delete(iri(downstream).has(isDerivedFrom, iri(upstream)));
		});
		modify.prefix(p_derived);
		storeClient.executeUpdate(modify.getQueryString());
	}

	/**
	 * Returns true if it is a asynchronous derived quantity.
	 * 
	 * @param kbClient
	 * @param derived_iri
	 * @return
	 */
	boolean isDerivedAsynchronous(String derived_iri) {
		SelectQuery query = Queries.SELECT();
		Variable type = query.var();
		TriplePattern tp = iri(derived_iri).isA(type);
		Expression<?> constraint = Expressions.equals(type, DerivationAsyn);

		GraphPattern queryPattern = tp.filter(constraint);

		query.prefix(p_derived).select(type).where(queryPattern);
		if (storeClient.executeQuery(query.getQueryString()).length() == 1) {
			return true;
		} else {
			return false;
		}
	}

	/**
	 * add <newEntity> <belongsTo> <instance> triples
	 * 
	 * @param storeClient
	 * @param instance
	 * @param newEntities
	 */
	@Deprecated
	void addNewEntitiesToDerived(String instance, List<String> newEntities) {
		ModifyQuery modify = Queries.MODIFY();

		for (String newEntity : newEntities) {
			modify.insert(iri(newEntity).has(belongsTo, iri(instance)));
		}

		storeClient.executeUpdate(modify.prefix(p_derived).getQueryString());
	}

	/**
	 * This method gets the derivations that match the given root derivation IRI,
	 * target derivation rdf:type and its upstream property paths.
	 * 
	 * @param rootDerivationIRI
	 * @param targetDerivationTypeList
	 * @param upstreamPath
	 * @return
	 */
	List<Derivation> getDerivations(String rootDerivationIRI,
			List<String> targetDerivationTypeList, RdfPredicate upstreamPath) {
		List<Iri> targetDerivationTypeIriList = targetDerivationTypeList.stream().map(iri -> derivationToIri.get(iri))
				.collect(Collectors.toList());
		SelectQuery query = Queries.SELECT();
		Variable derivation = query.var();
		Variable input = query.var();
		Variable inputType = query.var();
		Variable entity = query.var();
		Variable entityType = query.var();
		Variable agentURL = query.var();
		Variable derivationTimestamp = query.var();
		Variable inputTimestamp = query.var();
		Variable derivationType = query.var();
		Variable status = query.var();
		Variable statusType = query.var();
		Variable newDerivedIRI = query.var();
		Variable newDerivedIRIRdfType = query.var();

		// ignore certain rdf:type (e.g. OWL.namedInvididual)
		Expression<?>[] entityTypeFilters = new Expression<?>[classesToIgnore.size()];
		for (int j = 0; j < classesToIgnore.size(); j++) {
			entityTypeFilters[j] = Expressions.notEquals(entityType, classesToIgnore.get(j));
		}

		// ignore certain rdf:type (e.g. OWL.namedInvididual)
		Expression<?>[] inputTypeFilters = new Expression<?>[classesToIgnore.size()];
		for (int j = 0; j < classesToIgnore.size(); j++) {
			inputTypeFilters[j] = Expressions.notEquals(inputType, classesToIgnore.get(j));
		}

		// ignore certain rdf:type (e.g. OWL.namedInvididual)
		Expression<?>[] newDerivedIRITypeFilters = new Expression<?>[classesToIgnore.size()];
		for (int j = 0; j < classesToIgnore.size(); j++) {
			newDerivedIRITypeFilters[j] = Expressions.notEquals(newDerivedIRIRdfType, classesToIgnore.get(j));
		}

		// compared to the function getDerivations(), ValuesPattern of derivationType is
		// added so that one may only query the specific types of derivation, e.g.
		// DerivationAsyn, the triple about retrievedInputsAt is also added
		GraphPattern derivationPattern = GraphPatterns.and(
				new ValuesPattern(derivationType, targetDerivationTypeIriList),
				derivation.has(isDerivedFrom, input)
						.andHas(PropertyPaths.path(isDerivedUsing, hasOperation, hasHttpUrl), agentURL)
						.andHas(PropertyPaths.path(hasTime, inTimePosition, numericPosition), derivationTimestamp)
						.andIsA(derivationType));
		// compared to the function getDerivations(), statusPattern is added so that the
		// status information of asynchronous derivation is also cached, this pattern is
		// optional to also accommodate the normal Derivation and
		// DerivationWithTimeSeries
		GraphPattern statusPattern = GraphPatterns.and(
				new ValuesPattern(statusType,
						DerivationSparql.statusType.stream().map(i -> iri(i)).collect(Collectors.toList())),
				derivation.has(hasStatus, status),
				status.isA(statusType),
				GraphPatterns.and(
						status.has(hasNewDerivedIRI, newDerivedIRI), newDerivedIRI.isA(newDerivedIRIRdfType)).optional()
						.filter(Expressions.and(newDerivedIRITypeFilters))
						.optional())
				.optional();
		// compared to the function getDerivations(), entityPattern is is combined with
		// entityTypePattern and made within one optional clause, this is to accommodate
		// the situation where derivations are created for new information so no outputs
		// are provided
		GraphPattern entityPattern = GraphPatterns.and(entity.has(belongsTo, derivation),
				entity.isA(entityType).optional().filter(Expressions.and(entityTypeFilters))).optional();
		GraphPattern inputTimestampPattern = input.has(
				PropertyPaths.path(hasTime, inTimePosition, numericPosition), inputTimestamp).optional();
		// compared to the function getDerivations(), the inputTypePattern is compulsory
		GraphPattern inputTypePattern = input.isA(inputType).filter(Expressions.and(inputTypeFilters));

		// compared to the function getDerivations(), this part is added to decide
		// whether to query ALL derivations in the KG or just those upstream of root
		// also here we alter the depth of the queried DAG by using upstreamPath
		if (!rootDerivationIRI.equals(PLACEHOLDER)) {
			if (!upstreamPath.equals(PLACEHOLDER_IRI)) {
				GraphPattern rootDerivationPattern = iri(rootDerivationIRI).has(upstreamPath, derivation);
				query.where(rootDerivationPattern);
			} else {
				ValuesPattern rootDerivationPattern = new ValuesPattern(derivation,
						Arrays.asList(iri(rootDerivationIRI)));
				query.where(rootDerivationPattern);
			}
		}

		query.select(derivation, input, entity, agentURL, derivationTimestamp, status, newDerivedIRI, inputTimestamp,
				derivationType, inputType, entityType, statusType, newDerivedIRIRdfType)
				.where(derivationPattern, statusPattern, entityPattern, inputTimestampPattern, inputTypePattern)
				.prefix(p_derived, p_time, p_agent);

		JSONArray queryResult = storeClient.executeQuery(query.getQueryString());
		LOGGER.debug(query.getQueryString());
		LOGGER.debug(queryResult);

		Map<String, Derivation> derivationsMap = new HashMap<>();
		List<Derivation> derivationList = new ArrayList<>();
		Map<String, Entity> entitiesMap = new HashMap<>();
		Map<String, Entity> newDerivedIRIMap = new HashMap<>();
		for (int i = 0; i < queryResult.length(); i++) {
			String derivationIRI = queryResult.getJSONObject(i).getString(derivation.getQueryString().substring(1));
			String inputIRI = queryResult.getJSONObject(i).getString(input.getQueryString().substring(1));
			String urlString = queryResult.getJSONObject(i).getString(agentURL.getQueryString().substring(1));
			String derivedType = queryResult.getJSONObject(i).getString(derivationType.getQueryString().substring(1));
			String input_rdf_type = queryResult.getJSONObject(i).getString(inputType.getQueryString().substring(1));
			long derivedTimestamp = queryResult.getJSONObject(i)
					.getLong(derivationTimestamp.getQueryString().substring(1));

			Derivation derived;
			if (derivationsMap.containsKey(derivationIRI)) {
				derived = derivationsMap.get(derivationIRI);
			} else {
				derived = new Derivation(derivationIRI, derivedType);
				derivationsMap.put(derivationIRI, derived);
				derivationList.add(derived);
			}

			// input of this derivation, it can be an entity or a derivation
			Entity input_entity;
			Derivation directUpstream;
			if (derivationTypes.contains(input_rdf_type)) { // the input might be a derivation
				if (derivationsMap.containsKey(inputIRI)) {
					directUpstream = derivationsMap.get(inputIRI);
				} else {
					directUpstream = new Derivation(inputIRI, input_rdf_type);
					derivationsMap.put(inputIRI, directUpstream);
					derivationList.add(directUpstream);
				}
				// make the connection between the derivation and the upstream derivation
				derived.setDirectedUpstreams(directUpstream);
			} else { // this is a normal entity
				if (entitiesMap.containsKey(inputIRI)) {
					input_entity = entitiesMap.get(inputIRI);
				} else {
					input_entity = new Entity(inputIRI);
					entitiesMap.put(inputIRI, input_entity);
				}
				input_entity.setRdfType(input_rdf_type);
				// if it's a pure input/derivation it will have a timestamp
				if (queryResult.getJSONObject(i).has(inputTimestamp.getQueryString().substring(1))) {
					long input_timestamp = queryResult.getJSONObject(i)
							.getLong(inputTimestamp.getQueryString().substring(1));
					input_entity.setTimestamp(input_timestamp);
				}
				derived.addInput(input_entity);
			}

			// compared to the function getDerivations(), the output entity is made
			// optional, thus, here we check first if output exists
			if (queryResult.getJSONObject(i).has(entity.getQueryString().substring(1))) {
				String entityIRI = queryResult.getJSONObject(i).getString(entity.getQueryString().substring(1));
				Entity entity_entity;
				if (entitiesMap.containsKey(entityIRI)) {
					entity_entity = entitiesMap.get(entityIRI);
				} else {
					entity_entity = new Entity(entityIRI);
					entitiesMap.put(entityIRI, entity_entity);
				}

				// if rdf type exists
				if (queryResult.getJSONObject(i).has(entityType.getQueryString().substring(1))) {
					entity_entity
							.setRdfType(
									queryResult.getJSONObject(i).getString(entityType.getQueryString().substring(1)));
				}
				derived.addEntity(entity_entity);
			}

			// compared to the function getDerivations(), we also cache the queried status
			// and new derived IRI
			if (queryResult.getJSONObject(i).has(status.getQueryString().substring(1))) {
				String statusIRI = queryResult.getJSONObject(i).getString(status.getQueryString().substring(1));
				String statusTypeIRI = queryResult.getJSONObject(i).getString(statusType.getQueryString().substring(1));
				if (derived.getStatus() == null) {
					derived.setStatus(statusIRI, statusTypeIRI);
				} else {
					if (!derived.getStatus().getStatusIri().equals(statusIRI)
							|| !derived.getStatus().getStatusRdfType().equals(statusTypeIRI)) {
						throw new JPSRuntimeException(
								"Multiple instances of OntoDerivation:Status were added to derivation <"
										+ derived.getIri() + ">: "
										+ Arrays.asList(derived.getStatus().getStatusIri(), statusIRI).toString());
					}
				}

				if (queryResult.getJSONObject(i).has(newDerivedIRI.getQueryString().substring(1))) {
					String newIRI = queryResult.getJSONObject(i).getString(newDerivedIRI.getQueryString().substring(1));
					Entity new_entity;
					if (newDerivedIRIMap.containsKey(newIRI)) {
						new_entity = newDerivedIRIMap.get(newIRI);
					} else {
						new_entity = new Entity(newIRI);
						newDerivedIRIMap.put(newIRI, new_entity);
					}

					// if rdf type exists
					if (queryResult.getJSONObject(i).has(newDerivedIRIRdfType.getQueryString().substring(1))) {
						new_entity.setRdfType(queryResult.getJSONObject(i)
								.getString(newDerivedIRIRdfType.getQueryString().substring(1)));
					}
					derived.getStatus().addNewDerivedIRI(new_entity);
				}
			}

			// set properties of derivation
			derived.setAgentURL(urlString);
			derived.setTimestamp(derivedTimestamp);
		}

		return derivationList;
	}

	/**
	 * This method retrieves a list of derivations including the root derivation and
	 * all its upstream derivations matching the given derivation rdf:type.
	 * NOTE that the functions assumes the rdf:type of root derivation is provided
	 * in the targetDerivationTypeList by developer.
	 * 
	 * @param rootDerivationIRI
	 * @param targetDerivationTypeList
	 * @return
	 */
	List<Derivation> getRootAndAllTargetUpstreamDerivations(String rootDerivationIRI,
			List<String> targetDerivationTypeList) {
		return getDerivations(rootDerivationIRI, targetDerivationTypeList,
				PropertyPaths.zeroOrMore(groupPropertyPath(PropertyPaths.path(isDerivedFrom, belongsTo))));
	}

	/**
	 * This method retrieves a list of immediate upstream derivations of the root
	 * derivation matching the given derivation rdf:type.
	 * 
	 * @param rootDerivationIRI
	 * @return
	 */
	List<Derivation> getAllImmediateUpstreamDerivations(String rootDerivationIRI) {
		return getDerivations(rootDerivationIRI, derivationTypes,
				groupPropertyPath(PropertyPaths.path(isDerivedFrom, belongsTo)));
	}

	/**
	 * This method retrieves the information about the given derivation IRI.
	 * 
	 * @param rootDerivationIRI
	 * @return
	 */
	Derivation getDerivation(String rootDerivationIRI) {
		List<Derivation> derivations = getDerivations(rootDerivationIRI, derivationTypes,
				PLACEHOLDER_IRI);
		Derivation derivation = derivations.stream().filter(d -> d.getIri().contentEquals(rootDerivationIRI))
				.findFirst().get();
		return derivation;
	}

	/**
	 * This method obtains all the derivations in the knowledge graph.
	 * 
	 * @return
	 */
	List<Derivation> getAllDerivationsInKG() {
		return getRootAndAllTargetUpstreamDerivations(PLACEHOLDER, derivationTypes);
	}

	/**
	 * This method retrieves the information about the given derivation IRI and its
	 * immediate downstream derivations, including the directed ones.
	 * 
	 * @param targetDerivationIRI
	 * @return
	 */
	Derivation getDerivationWithImmediateDownstream(String targetDerivationIRI) {
		// <targetDerivationIRI> ^(isDerivedFrom/belongsTo?)? ?derivation .
		// so that ?derivation will include targetDerivation and all its immediate
		// downstream derivations, including those connected via isDerivedFrom/belonsTo
		// and only isDerivedFrom (derivations for new information)
		List<Derivation> derivations = getDerivations(targetDerivationIRI, derivationTypes,
				zeroOrOne(inversePath(groupPropertyPath(PropertyPaths.path(isDerivedFrom, zeroOrOne(belongsTo))))));
		Derivation derivation = derivations.stream().filter(d -> d.getIri().contentEquals(targetDerivationIRI))
				.findFirst().get();
		return derivation;
	}

	/**
	 * this is used to obtain all the derivations in the kg
	 * NOTE: this method is marked as Deprecated as it's replaced by method
	 * getAllDerivationsInKG()
	 * 
	 * @return
	 */
	@Deprecated
	List<Derivation> getDerivations() {
		SelectQuery query = Queries.SELECT();

		Variable derivation = query.var();
		Variable input = query.var();
		Variable inputType = query.var();
		Variable entity = query.var();
		Variable entityType = query.var();
		Variable agentURL = query.var();
		Variable derivationTimestamp = query.var();
		Variable inputTimestamp = query.var();
		Variable derivationType = query.var();

		// ignore certain rdf:type (e.g. OWL.namedInvididual)
		Expression<?>[] entityTypeFilters = new Expression<?>[classesToIgnore.size()];
		for (int j = 0; j < classesToIgnore.size(); j++) {
			entityTypeFilters[j] = Expressions.notEquals(entityType, classesToIgnore.get(j));
		}

		// ignore certain rdf:type (e.g. OWL.namedInvididual)
		Expression<?>[] inputTypeFilters = new Expression<?>[classesToIgnore.size()];
		for (int j = 0; j < classesToIgnore.size(); j++) {
			inputTypeFilters[j] = Expressions.notEquals(inputType, classesToIgnore.get(j));
		}

		GraphPattern derivationPattern = derivation.has(isDerivedFrom, input)
				.andHas(PropertyPaths.path(isDerivedUsing, hasOperation, hasHttpUrl), agentURL)
				.andHas(PropertyPaths.path(hasTime, inTimePosition, numericPosition), derivationTimestamp)
				.andIsA(derivationType);
		GraphPattern entityPattern = entity.has(belongsTo, derivation);
		GraphPattern inputTimestampPattern = input.has(
				PropertyPaths.path(hasTime, inTimePosition, numericPosition), inputTimestamp).optional();
		GraphPattern inputTypePattern = input.isA(inputType).optional().filter(Expressions.and(inputTypeFilters));
		GraphPattern entityTypePattern = entity.isA(entityType).optional().filter(Expressions.and(entityTypeFilters));

		query.select(derivation, input, entity, agentURL, derivationTimestamp, inputTimestamp, derivationType,
				inputType, entityType)
				.where(derivationPattern, entityPattern, inputTimestampPattern, inputTypePattern, entityTypePattern)
				.prefix(p_derived, p_time, p_agent);

		JSONArray queryResult = storeClient.executeQuery(query.getQueryString());

		Map<String, Derivation> derivationsMap = new HashMap<>();
		List<Derivation> derivationList = new ArrayList<>();
		Map<String, Entity> entitiesMap = new HashMap<>();
		for (int i = 0; i < queryResult.length(); i++) {
			String derivationIRI = queryResult.getJSONObject(i).getString(derivation.getQueryString().substring(1));
			String inputIRI = queryResult.getJSONObject(i).getString(input.getQueryString().substring(1));
			String entityIRI = queryResult.getJSONObject(i).getString(entity.getQueryString().substring(1));
			String urlString = queryResult.getJSONObject(i).getString(agentURL.getQueryString().substring(1));
			String derivedType = queryResult.getJSONObject(i).getString(derivationType.getQueryString().substring(1));
			long derivedTimestamp = queryResult.getJSONObject(i)
					.getLong(derivationTimestamp.getQueryString().substring(1));

			Derivation derived;
			if (derivationsMap.containsKey(derivationIRI)) {
				derived = derivationsMap.get(derivationIRI);
			} else {
				derived = new Derivation(derivationIRI, derivedType);
				derivationsMap.put(derivationIRI, derived);
				derivationList.add(derived);
			}

			// input of this derivation
			Entity input_entity;
			if (entitiesMap.containsKey(inputIRI)) {
				input_entity = entitiesMap.get(inputIRI);
			} else {
				input_entity = new Entity(inputIRI);
				entitiesMap.put(inputIRI, input_entity);
			}

			// if rdf type exists
			if (queryResult.getJSONObject(i).has(inputType.getQueryString().substring(1))) {
				input_entity
						.setRdfType(queryResult.getJSONObject(i).getString(inputType.getQueryString().substring(1)));
			}

			// if it's a pure input it will have a timestamp
			if (queryResult.getJSONObject(i).has(inputTimestamp.getQueryString().substring(1))) {
				long input_timestamp = queryResult.getJSONObject(i)
						.getLong(inputTimestamp.getQueryString().substring(1));
				input_entity.setTimestamp(input_timestamp);
			}

			Entity entity_entity;
			if (entitiesMap.containsKey(entityIRI)) {
				entity_entity = entitiesMap.get(entityIRI);
			} else {
				entity_entity = new Entity(entityIRI);
				entitiesMap.put(entityIRI, entity_entity);
			}

			// if rdf type exists
			if (queryResult.getJSONObject(i).has(entityType.getQueryString().substring(1))) {
				entity_entity
						.setRdfType(queryResult.getJSONObject(i).getString(entityType.getQueryString().substring(1)));
			}

			// set properties of derivation
			derived.addEntity(entity_entity);
			derived.addInput(input_entity);
			derived.setAgentURL(urlString);
			derived.setTimestamp(derivedTimestamp);
		}

		return derivationList;
	}

	/**
	 * pure inputs with timestamps cannot be part of a derivation
	 */
	boolean validatePureInputs() {
		SelectQuery query = Queries.SELECT();
		Variable input = query.var();
		Variable derivation = query.var();
		Variable inputTimestamp = query.var();

		GraphPattern queryPattern = input.has(belongsTo, derivation)
				.andHas(PropertyPaths.path(hasTime, inTimePosition, numericPosition), inputTimestamp);

		query.prefix(p_time, p_derived).where(queryPattern);

		JSONArray queryResult = storeClient.executeQuery(query.getQueryString());

		if (queryResult.length() > 0) {
			return false;
		} else {
			return true;
		}
	}

	/**
	 * delete all entities of a given derivation
	 * 
	 * @param derivation
	 */
	@Deprecated
	void deleteBelongsTo(String derivation) {
		SelectQuery query = Queries.SELECT();
		Variable entity = query.var();
		Variable predicate1 = query.var();
		Variable predicate2 = query.var();
		Variable subject = query.var();
		Variable object = query.var();

		TriplePattern delete_tp1 = entity.has(predicate1, object);
		TriplePattern delete_tp2 = subject.has(predicate2, entity);

		ModifyQuery modify = Queries.MODIFY();
		modify.delete(delete_tp1, delete_tp2)
				.where(entity.has(belongsTo, iri(derivation)), delete_tp1, subject.has(predicate2, entity).optional())
				.prefix(p_derived);

		storeClient.executeUpdate(modify.getQueryString());
	}

	/**
	 * This method reconnects new derived IRIs when a synchronous derivation is
	 * updated, specifically, it does below five operations in one-go:
	 * (1) delete old outputs (entities) of the given derivation and its connections
	 * with downstream derivations;
	 * (2) delete status of the derivation if exist (applicable for outdated sync
	 * derivations in the mixed derivation DAGs);
	 * (3) replace timestamp of the given derivation with the new timestamp recorded
	 * when the derivation inputs were retrieved;
	 * (4) insert all new triples created by the DerivationAgent;
	 * (5) insert new derived IRIs to be connected with the given derivation using
	 * "belongsTo", also all its downstream derivations using "isDerivedFrom".
	 * 
	 * It should be noted that this SPARQL update will ONLY be executed when the
	 * given derivation is outdated - this helps with addressing the concurrent HTTP
	 * request issue as detailed in
	 * https://github.com/cambridge-cares/TheWorldAvatar/issues/184.
	 * 
	 * This method returns a boolean value to indicate if it is certain that the
	 * triples in the update endpoint are changed by the SPARQL update operation.
	 * This is currently implemented as checking the "mutationCount" in the response
	 * message of the HTTP POST for SPARQL update to blazegraph-backended endpoint,
	 * a true boolean will be returned IF AND ONLY IF the mutationCount is greater
	 * than 0 meaning the triples are changed.
	 * 
	 * For non-blazegraph-backended endpoint, the SPARQL update will be executed in
	 * the normal way and it is not clear enough to determine whether triples are
	 * changed. In this situation, a false boolean will be returned.
	 * 
	 * @param outputTriples
	 * @param newIriDownstreamDerivationMap
	 * @param derivation
	 * @param retrievedInputsAt
	 */
	boolean reconnectNewDerivedIRIs(List<TriplePattern> outputTriples,
			Map<String, List<String>> newIriDownstreamDerivationMap, String derivation,
			Long retrievedInputsAt) {
		ModifyQuery modify = Queries.MODIFY();
		SubSelect sub = GraphPatterns.select();

		// insert all output triples
		outputTriples.forEach(t -> modify.insert(t));

		// insert triples of new derived IRI and the derivations that it should be
		// connected to
		newIriDownstreamDerivationMap.forEach((newIRI, downstreamDerivations) -> {
			// add <newIRI> <belongsTo> <derivation> for each newIRI
			modify.insert(iri(newIRI).has(belongsTo, iri(derivation)));
			// if this <newIRI> is inputs to other derivations, add
			// <downstreamDerivation> <isDerivedFrom> <newIRI>
			if (!downstreamDerivations.isEmpty()) {
				downstreamDerivations.stream().forEach(dd -> modify.insert(iri(dd).has(isDerivedFrom, iri(newIRI))));
			}
		});

		// create variables for sub query
		Variable d = SparqlBuilder.var("d"); // this derivation
		Variable ups = SparqlBuilder.var("ups"); // upstream inputs/derivations

		// variables for old outputs (entities)
		Variable e = SparqlBuilder.var("e");
		Variable p1 = SparqlBuilder.var("p1");
		Variable o = SparqlBuilder.var("o");
		Variable s = SparqlBuilder.var("s");
		Variable p2 = SparqlBuilder.var("p2");

		// variables for timestamp
		Variable unixtimeIRI = SparqlBuilder.var("timeIRI");
		Variable dTs = SparqlBuilder.var("dTs"); // timestamp of this derivation
		Variable upsTs = SparqlBuilder.var("upsTs"); // timestamp of upstream

		// variables for status related concepts
		Variable status = SparqlBuilder.var("status");
		Variable statusType = SparqlBuilder.var("statusType");

		// filter ?derivationTimestamp < ?upstreamTimestamp
		Expression<?> tsFilter = Expressions.lt(dTs, upsTs);

		// this GraphPattern ensures ?d only exist if it's outdated, otherwise the
		// SPARQL update will not execute
		GraphPattern derivationPattern = GraphPatterns
				.and(new ValuesPattern(d, Arrays.asList(iri(derivation))),
						d.has(PropertyPaths.path(hasTime, inTimePosition), unixtimeIRI),
						unixtimeIRI.has(numericPosition, dTs),
						d.has(PropertyPaths.path(isDerivedFrom, zeroOrOne(belongsTo)), ups),
						ups.has(PropertyPaths.path(hasTime, inTimePosition, numericPosition), upsTs))
				.filter(tsFilter);
		// this GraphPattern queries all the old outputs (entities)
		GraphPattern entityPattern = GraphPatterns.and(e.has(belongsTo, d), e.has(p1, o), s.has(p2, e).optional());
		// these patterns query the status of this derivation if it exist, thus optional
		TriplePattern tp1 = d.has(hasStatus, status);
		TriplePattern tp2 = status.isA(statusType);
		GraphPattern statusQueryPattern = GraphPatterns.and(tp1, tp2).optional();

		// construct the sub query
		sub.select(d, unixtimeIRI, dTs, status, statusType, e, p1, o, s, p2)
				.where(derivationPattern, entityPattern, statusQueryPattern);

		// delete old outputs (entities) - basically delete this individual
		TriplePattern deleteEntityAsSubject = e.has(p1, o);
		TriplePattern deleteEntityAsObject = s.has(p2, e);

		// timestamp-related triples to add and delete
		TriplePattern deleteOldTimestamp = unixtimeIRI.has(numericPosition, dTs);
		TriplePattern insertNewTimestamp = unixtimeIRI.has(numericPosition, retrievedInputsAt);

		// construct the complete SPARQL update, note that some of the insert triples
		// have already been added at the beginning of this method
		modify.prefix(p_time, p_derived)
				.delete(deleteEntityAsSubject, deleteEntityAsObject, tp1, tp2, deleteOldTimestamp)
				.insert(insertNewTimestamp).where(sub);

		try {
			if (storeClient.getClass() == RemoteStoreClient.class) {
				if (((RemoteStoreClient) storeClient).isUpdateEndpointBlazegraphBackended()) {
					HttpResponse httpResponse = ((RemoteStoreClient) storeClient)
							.executeUpdateByPost(modify.getQueryString());
					if (httpResponse.getStatusLine().getStatusCode() != 204 && httpResponse.getEntity() != null) {
						String html = EntityUtils.toString(httpResponse.getEntity());
						Pattern pattern = Pattern.compile("mutationCount=(.*)</p");
						Matcher matcher = pattern.matcher(html);
						if (matcher.find() && Integer.parseInt(matcher.group(1)) > 0) {
							// only return true if the agent is able to parse "mutationCount=(.*)</p" and
							// the parsed value is greater than 0
							LOGGER.debug("SPARQL update (" + modify.getQueryString() + ") executed with mutationCount="
									+ matcher.group(1));
							return true;
						}
						LOGGER.debug("SPARQL update (" + modify.getQueryString() + ") executed with mutationCount="
								+ matcher.group(1));
					}
				} else {
					storeClient.executeUpdate(modify.getQueryString());
				}
			} else {
				storeClient.executeUpdate(modify.getQueryString());
			}
		} catch (ParseException | IOException exception) {
			throw new JPSRuntimeException(exception);
		}
		return false;
	}

	/**
	 * This method updates the asynchronous derivations at Finished status,
	 * specifically, it does below five operations in one-go:
	 * (1) if exist, delete old outputs (entities) of the given derivation and its
	 * connections with downstream derivations;
	 * (2) delete status of the derivation;
	 * (3) replace timestamp of the given derivation with the new timestamp recorded
	 * by data property OntoDerivation:retrievedInputsAt when the derivation inputs
	 * were retrieved, also delets the OntoDerivation:retrievedInputsAt record;
	 * (4) if exist, delete downstream derivations that directly connected to the
	 * given derivation via OntoDerivation:isDerivedFrom (applicable if this
	 * derivation and its downstreams were created for new information);
	 * (5) insert new derived IRIs to be connected with the given derivation using
	 * "belongsTo", also all its downstream derivations using "isDerivedFrom" - the
	 * mapping are provided as an argument to this method.
	 * 
	 * It should be noted that this SPARQL update will ONLY be executed when the
	 * given derivation is outdated - this prevents any potential faulty behaviour
	 * due to high frequency asynchronous derivation monitoring.
	 * 
	 * This method returns a boolean value to indicate if it is certain that the
	 * triples in the update endpoint are changed by the SPARQL update operation.
	 * This is currently implemented as checking the "mutationCount" in the response
	 * message of the HTTP POST for SPARQL update to blazegraph-backended endpoint,
	 * a true boolean will be returned IF AND ONLY IF the mutationCount is greater
	 * than 0 meaning the triples are changed.
	 * 
	 * For non-blazegraph-backended endpoint, the SPARQL update will be executed in
	 * the normal way and it is not clear enough to determine whether triples are
	 * changed. In this situation, a false boolean will be returned.
	 * 
	 * @param derivation
	 * @param newIriDownstreamDerivationMap
	 * @return
	 */
	boolean updateFinishedAsyncDerivation(String derivation, Map<String, List<String>> newIriDownstreamDerivationMap) {
		ModifyQuery modify = Queries.MODIFY();
		SubSelect sub = GraphPatterns.select();

		// insert triples of new derived IRI and the derivations that it should be
		// connected to
		newIriDownstreamDerivationMap.forEach((newIRI, downstreamDerivations) -> {
			// add <newIRI> <belongsTo> <derivation> for each newIRI
			modify.insert(iri(newIRI).has(belongsTo, iri(derivation)));
			// if this <newIRI> is inputs to other derivations, add
			// <downstreamDerivation> <isDerivedFrom> <newIRI>
			if (!downstreamDerivations.isEmpty()) {
				downstreamDerivations.stream().forEach(dd -> modify.insert(iri(dd).has(isDerivedFrom, iri(newIRI))));
			}
		});

		// create variables for sub query
		Variable d = SparqlBuilder.var("d"); // this derivation
		Variable ups = SparqlBuilder.var("ups"); // upstream inputs/derivations
		Variable downd = SparqlBuilder.var("dd"); // downstream derivations isDerivedFrom

		// variables for old outputs (entities)
		Variable e = SparqlBuilder.var("e");
		Variable p1 = SparqlBuilder.var("p1");
		Variable o = SparqlBuilder.var("o");
		Variable s = SparqlBuilder.var("s");
		Variable p2 = SparqlBuilder.var("p2");

		// variables for timestamp
		Variable unixtimeIRI = SparqlBuilder.var("timeIRI");
		Variable dTs = SparqlBuilder.var("dTs"); // timestamp of this derivation
		Variable upsTs = SparqlBuilder.var("upsTs"); // timestamp of upstream
		Variable retrievedInputsAtTs = SparqlBuilder.var("retrievedInputsAt"); // retrievedInputsAt

		// variables for status related concepts
		Variable status = SparqlBuilder.var("status");
		Variable statusType = SparqlBuilder.var("statusType");
		Variable sndIRI = SparqlBuilder.var("sndIRI");

		// filter ?derivationTimestamp < ?upstreamTimestamp
		Expression<?> tsFilter = Expressions.lt(dTs, upsTs);

		// this GraphPattern ensures ?d only exist if it's outdated, otherwise the
		// SPARQL update will not execute
		GraphPattern derivationTsPattern = GraphPatterns
				.and(new ValuesPattern(d, Arrays.asList(iri(derivation))),
						d.has(PropertyPaths.path(hasTime, inTimePosition), unixtimeIRI),
						unixtimeIRI.has(numericPosition, dTs),
						d.has(PropertyPaths.path(isDerivedFrom, zeroOrOne(belongsTo)), ups),
						ups.has(PropertyPaths.path(hasTime, inTimePosition, numericPosition), upsTs))
				.filter(tsFilter);
		// this GraphPattern queries the timestamp retrievedInputsAt, maybe we can also
		// filter it?
		GraphPattern retrievedInputsAtPattern = d.has(retrievedInputsAt, retrievedInputsAtTs);
		// these patterns query the status of this derivation
		TriplePattern tp1 = d.has(hasStatus, status);
		TriplePattern tp2 = status.isA(statusType);
		TriplePattern tp3 = status.has(hasNewDerivedIRI, sndIRI);
		GraphPattern statusQueryPattern = GraphPatterns.and(tp1, tp2, tp3);
		// this GraphPattern queries directed downstream derivation if exist, thus
		// optional
		GraphPattern directedDownstreamPattern = downd.has(isDerivedFrom, d).optional();
		// this GraphPattern queries all the old outputs (entities), it is optional to
		// accommodate the situation where no outputs were presented for async
		// derivation created for new information
		GraphPattern entityPattern = GraphPatterns.and(e.has(belongsTo, d), e.has(p1, o), s.has(p2, e).optional())
				.optional();

		// construct the sub query
		sub.select(d, unixtimeIRI, dTs, retrievedInputsAtTs, status, statusType, sndIRI, downd, e, p1, o, s, p2)
				.where(derivationTsPattern, retrievedInputsAtPattern, statusQueryPattern, directedDownstreamPattern,
						entityPattern);

		// delete old outputs (entities) - basically delete this individual
		TriplePattern deleteEntityAsSubject = e.has(p1, o);
		TriplePattern deleteEntityAsObject = s.has(p2, e);

		// delete directed downstream derivation if exist
		TriplePattern deleteDirectedDownstream = downd.has(isDerivedFrom, d);

		// timestamp-related triples to add and delete
		TriplePattern deleteOldTimestamp = unixtimeIRI.has(numericPosition, dTs);
		TriplePattern deleteRetrievedInputsAt = d.has(retrievedInputsAt, retrievedInputsAtTs);
		TriplePattern insertNewTimestamp = unixtimeIRI.has(numericPosition, retrievedInputsAtTs);

		// construct the complete SPARQL update, note that some of the insert triples
		// have already been added at the beginning of this method
		modify.prefix(p_time, p_derived)
				.delete(deleteEntityAsSubject, deleteEntityAsObject, tp1, tp2, tp3, deleteDirectedDownstream,
						deleteOldTimestamp, deleteRetrievedInputsAt)
				.insert(insertNewTimestamp).where(sub);

		try {
			if (storeClient.getClass() == RemoteStoreClient.class) {
				if (((RemoteStoreClient) storeClient).isUpdateEndpointBlazegraphBackended()) {
					HttpResponse httpResponse = ((RemoteStoreClient) storeClient)
							.executeUpdateByPost(modify.getQueryString());
					if (httpResponse.getStatusLine().getStatusCode() != 204 && httpResponse.getEntity() != null) {
						String html = EntityUtils.toString(httpResponse.getEntity());
						Pattern pattern = Pattern.compile("mutationCount=(.*)</p");
						Matcher matcher = pattern.matcher(html);
						if (matcher.find() && Integer.parseInt(matcher.group(1)) > 0) {
							// only return true if the agent is able to parse "mutationCount=(.*)</p" and
							// the parsed value is greater than 0
							LOGGER.debug("SPARQL update (" + modify.getQueryString() + ") executed with mutationCount="
								+ matcher.group(1));
							return true;
						}
						LOGGER.debug("SPARQL update (" + modify.getQueryString() + ") executed with mutationCount="
							+ matcher.group(1));
					}
				} else {
					storeClient.executeUpdate(modify.getQueryString());
				}
			} else {
				storeClient.executeUpdate(modify.getQueryString());
			}
		} catch (ParseException | IOException exception) {
			throw new JPSRuntimeException(exception);
		}
		return false;
	}

	/**
	 * works with the Derivation, DerivationWithTimeSeries, and DerivationAsyn
	 * does not remove timestamps of inputs (technically outside derivation)
	 * 
	 */
	void dropAllDerivations() {
		List<Iri> derivationTypes = Arrays.asList(Derivation, DerivationWithTimeSeries, DerivationAsyn);
		ModifyQuery modify = Queries.MODIFY();

		SubSelect query = GraphPatterns.select();
		Variable inputs = query.var();
		Variable entities = query.var();
		Variable derivation = query.var();
		Variable time = query.var();
		Variable time_unix_iri = query.var();
		Variable timestamp = query.var();
		Variable trs = query.var();
		Variable agent = query.var();
		Variable operation = query.var();
		Variable url = query.var();
		Variable derivationType = query.var();

		TriplePattern belongsToTp = entities.has(belongsTo, derivation);
		TriplePattern isDerivedFromTp = derivation.has(isDerivedFrom, inputs);
		TriplePattern derivationTypeTp = derivation.isA(derivationType);

		// timestamp
		TriplePattern timestampTp1 = derivation.has(hasTime, time);

		TriplePattern timeTpAll1 = time.isA(InstantClass).andHas(inTimePosition, time_unix_iri);
		TriplePattern timeTpAll2 = time_unix_iri.isA(TimePosition).andHas(numericPosition, timestamp).andHas(hasTRS,
				trs);

		// agent
		TriplePattern agentTp1 = derivation.has(isDerivedUsing, agent);
		// TODO we need to decide whether to delete these triples
		// TODO this is also relevant if we should write these triples using derivation
		// framework in the first place
		TriplePattern agentTp2 = agent.isA(Service).andHas(hasOperation, operation);
		TriplePattern agentTp3 = operation.isA(Operation).andHas(hasHttpUrl, url);

		// DerivationAsyn Status related variables and triples
		Variable status = query.var();
		Variable statusType = query.var();
		Variable newDerivedIRI = query.var();
		TriplePattern tp1 = derivation.has(hasStatus, status);
		TriplePattern tp2 = status.isA(statusType);
		TriplePattern tp3 = status.has(hasNewDerivedIRI, newDerivedIRI);
		GraphPattern gp = status.has(hasNewDerivedIRI, newDerivedIRI).optional();
		GraphPattern asyncStatusGP = GraphPatterns.and(tp1, tp2, gp).optional();

		// NOTE: belongsToTp is made optional to accommodate the situation where async
		// derivations are created for new info, so no outputs are generated yet at the
		// point we would like to drop all derivations
		// NOTE: agentTp2 and agentTp3 were made optional to relax the query and update
		// - this applies when async derivation were generated when no instances about
		// OntoAgent were written to the KG
		GraphPattern queryPattern = GraphPatterns.and(
				new ValuesPattern(derivationType, derivationTypes),
				isDerivedFromTp, timestampTp1, timeTpAll1, timeTpAll2,
				agentTp1, derivationTypeTp, agentTp2.optional(), agentTp3.optional(),
				belongsToTp.optional(), asyncStatusGP);

		modify.delete(belongsToTp, isDerivedFromTp,
				timestampTp1, timeTpAll1, timeTpAll2,
				agentTp1, agentTp2, agentTp3, derivationTypeTp, tp1, tp2, tp3).where(queryPattern)
				.prefix(p_time, p_derived, p_agent);

		storeClient.executeUpdate(modify.getQueryString());
	}

	/**
	 * works with the Derivation, DerivationWithTimeSeries, and DerivationAsyn
	 * does not remove timestamps of inputs (technically outside derivation)
	 * 
	 * NOTE: compared to method dropAllDerivations(), this method does NOT remove
	 * triples agent.isA(Service).andHas(hasOperation, operation) and
	 * operation.isA(Operation).andHas(hasHttpUrl, url) - these triples are part of
	 * OntoAgent instances that might be needed outside of derivation framework
	 * 
	 */
	void dropAllDerivationsNotOntoAgent() {
		List<Iri> derivationTypes = Arrays.asList(Derivation, DerivationWithTimeSeries, DerivationAsyn);
		ModifyQuery modify = Queries.MODIFY();

		SubSelect query = GraphPatterns.select();
		Variable inputs = query.var();
		Variable entities = query.var();
		Variable derivation = query.var();
		Variable time = query.var();
		Variable time_unix_iri = query.var();
		Variable timestamp = query.var();
		Variable trs = query.var();
		Variable agent = query.var();
		Variable derivationType = query.var();

		TriplePattern belongsToTp = entities.has(belongsTo, derivation);
		TriplePattern isDerivedFromTp = derivation.has(isDerivedFrom, inputs);
		TriplePattern derivationTypeTp = derivation.isA(derivationType);

		// timestamp
		TriplePattern timestampTp1 = derivation.has(hasTime, time);

		TriplePattern timeTpAll1 = time.isA(InstantClass).andHas(inTimePosition, time_unix_iri);
		TriplePattern timeTpAll2 = time_unix_iri.isA(TimePosition).andHas(numericPosition, timestamp).andHas(hasTRS,
				trs);

		// agent
		TriplePattern agentTp1 = derivation.has(isDerivedUsing, agent);

		// DerivationAsyn Status related variables and triples
		Variable status = query.var();
		Variable statusType = query.var();
		Variable newDerivedIRI = query.var();
		TriplePattern tp1 = derivation.has(hasStatus, status);
		TriplePattern tp2 = status.isA(statusType);
		TriplePattern tp3 = status.has(hasNewDerivedIRI, newDerivedIRI);
		GraphPattern gp = status.has(hasNewDerivedIRI, newDerivedIRI).optional();
		GraphPattern asyncStatusGP = GraphPatterns.and(tp1, tp2, gp).optional();

		// NOTE: belongsToTp is made optional to accommodate the situation where async
		// derivations are created for new info, so no outputs are generated yet at the
		// point we would like to drop all derivations

		GraphPattern queryPattern = GraphPatterns.and(
				new ValuesPattern(derivationType, derivationTypes),
				isDerivedFromTp, timestampTp1, timeTpAll1, timeTpAll2,
				agentTp1, derivationTypeTp, belongsToTp.optional(), asyncStatusGP);

		modify.delete(belongsToTp, isDerivedFromTp,
				timestampTp1, timeTpAll1, timeTpAll2,
				agentTp1, derivationTypeTp, tp1, tp2, tp3).where(queryPattern)
				.prefix(p_time, p_derived, p_agent);

		storeClient.executeUpdate(modify.getQueryString());
	}

	void dropAllTimestamps() {
		ModifyQuery modify = Queries.MODIFY();
		SelectQuery query = Queries.SELECT();

		Variable entity = query.var();
		Variable time = query.var();
		Variable time_unix_iri = query.var();
		Variable timestamp = query.var();
		Variable trs = query.var();

		TriplePattern tp1 = entity.has(hasTime, time);
		TriplePattern tp2 = time.isA(InstantClass).andHas(inTimePosition, time_unix_iri);
		TriplePattern tp3 = time_unix_iri.isA(TimePosition).andHas(numericPosition, timestamp).andHas(hasTRS, trs);

		modify.delete(tp1, tp2, tp3).where(tp1, tp2, tp3).prefix(p_time);

		storeClient.executeUpdate(modify.getQueryString());
	}

	/**
	 * Updates timestamps of the given instances in two stages
	 * Query 1: get the corresponding time IRI
	 * query 2: delete/insert appropriate triples
	 * 
	 * @param derivationTimestamp_map
	 */
	void updateTimestamps(Map<String, Long> instanceTimestamp_map) {
		List<String> instances = new ArrayList<>(instanceTimestamp_map.keySet());

		// query 1: get corresponding time IRI for each instance if it exists
		SelectQuery query = Queries.SELECT();
		Variable inst = query.var();
		ValuesPattern instValuesPattern = new ValuesPattern(inst,
				instances.stream().map(i -> iri(i)).collect(Collectors.toList()));

		Variable time_unix = query.var();

		GraphPattern gp1 = inst.has(PropertyPaths.path(hasTime, inTimePosition), time_unix);

		query.select(inst, time_unix).where(gp1, instValuesPattern).prefix(p_time);

		JSONArray queryResult = storeClient.executeQuery(query.getQueryString());

		Map<String, String> instance_timeiri_map = new HashMap<>();

		for (int i = 0; i < queryResult.length(); i++) {
			instance_timeiri_map.put(
					queryResult.getJSONObject(i).getString(inst.getQueryString().substring(1)),
					queryResult.getJSONObject(i).getString(time_unix.getQueryString().substring(1)));
		}

		// some instances provided by the user may not exist, update list here
		instances = new ArrayList<>(instance_timeiri_map.keySet());

		// query 2: update query, delete and insert appropriate triples
		ModifyQuery modify = Queries.MODIFY();

		Variable timestamp = query.var();

		TriplePattern[] insert_tp = new TriplePattern[instances.size()];
		List<Iri> timeIRIList = new ArrayList<>();

		for (int i = 0; i < instances.size(); i++) {
			String instance = instances.get(i);
			Iri timeIRI = iri(instance_timeiri_map.get(instance));
			insert_tp[i] = timeIRI.has(numericPosition, instanceTimestamp_map.get(instance));
			timeIRIList.add(timeIRI);
		}

		TriplePattern delete_tp = time_unix.has(numericPosition, timestamp);
		ValuesPattern timeValuesPattern = new ValuesPattern(time_unix, timeIRIList);

		modify.delete(delete_tp).where(timeValuesPattern, delete_tp).insert(insert_tp).prefix(p_time);

		storeClient.executeUpdate(modify.getQueryString());
	}

	Map<String, String> getDerivationsOf(List<String> entities) {
		SelectQuery query = Queries.SELECT();
		Variable entity = query.var();
		Variable derivation = query.var();

		GraphPattern queryPattern = entity.has(belongsTo, derivation);
		ValuesPattern valuesPattern = new ValuesPattern(entity,
				entities.stream().map(e -> iri(e)).collect(Collectors.toList()));

		query.select(entity, derivation).where(queryPattern, valuesPattern).prefix(p_derived);

		JSONArray queryResult = storeClient.executeQuery(query.getQueryString());

		Map<String, String> entityDerivationMap = new HashMap<>();
		for (int i = 0; i < queryResult.length(); i++) {
			entityDerivationMap.put(
					queryResult.getJSONObject(i).getString(entity.getQueryString().substring(1)),
					queryResult.getJSONObject(i).getString(derivation.getQueryString().substring(1)));
		}

		return entityDerivationMap;
	}

	/**
	 * This method retrieves the input read timestamp associated with the
	 * asynchronous derivation, also deletes the record after value retrieved.
	 * 
	 * @param derivation
	 * @return
	 */
	@Deprecated
	Map<String, Long> retrieveInputReadTimestamp(String derivation) {
		Map<String, Long> derivationTime_map = new HashMap<>();

		String queryKey = "timestamp";
		SelectQuery query = Queries.SELECT();
		Variable time = SparqlBuilder.var(queryKey);

		GraphPattern queryPattern = iri(derivation).has(retrievedInputsAt, time);

		query.prefix(p_derived).select(time).where(queryPattern);
		JSONArray queryResult = storeClient.executeQuery(query.getQueryString());

		if (queryResult.length() > 1) {
			throw new JPSRuntimeException(
					"DerivedQuantitySparql: More than 1 time instance recorded for reading derivation inputs of <"
							+ derivation + ">");
		}

		try {
			long inputReadTimestamp = queryResult.getJSONObject(0).getLong(queryKey);
			derivationTime_map.put(derivation, inputReadTimestamp);

			// delete triple {<derivation> <retrievedInputsAt> timestamp}
			ModifyQuery modify = Queries.MODIFY();
			TriplePattern delete_timeRecord = iri(derivation).has(retrievedInputsAt, inputReadTimestamp);
			modify.prefix(p_derived).delete(delete_timeRecord);
			storeClient.executeUpdate(modify.getQueryString());

			return derivationTime_map;
		} catch (JSONException e) {
			throw new JPSRuntimeException("No timestamp recorded for reading derivation inputs of <" + derivation
					+ ">. The derivation is probably not setup correctly.");
		}
	}

	/**
	 * This method chunks the given iri and returns its namespace.
	 * 
	 * @param iri
	 * @return
	 */
	private String getNameSpace(String iri) {
		iri = trimIRI(iri);
		if (iri.contains("#")) {
			iri = iri.substring(0, iri.lastIndexOf("#") + 1);
		} else if (iri.contains("/")) {
			iri = iri.substring(0, iri.lastIndexOf("/") + 1);
		}
		return iri;
	}

	/**
	 * This method trims the given iri by removing the "<" at the start and the ">"
	 * at the end.
	 * 
	 * @param iri
	 * @return
	 */
	private String trimIRI(String iri) {
		if (iri.startsWith("<")) {
			iri = iri.substring(1);
		}
		if (iri.endsWith(">")) {
			iri = iri.substring(0, iri.length() - 1);
		}
		return iri;
	}

	/**
	 * This method groups PropertyPath by adding parenthesis, for example:
	 * input: objProperty1/objProperty2
	 * output: (objProperty1/objProperty2)
	 * 
	 * This method is advised to be used together with other functions provided by
	 * org.eclipse.rdf4j.sparqlbuilder.core.PropertyPaths for more flexible KG
	 * operations.
	 * 
	 * @param aElement
	 * @return
	 */
	private RdfPredicate groupPropertyPath(RdfPredicate aElement) {
		return () -> "(" + aElement.getQueryString() + ")";
	}

	/**
	 * This method implements the zero or one path in SPARQL 1.1 Property Paths.
	 * see https://www.w3.org/TR/sparql11-property-paths/
	 * 
	 * @param aElement
	 * @return
	 */
	private RdfPredicate zeroOrOne(RdfPredicate aElement) {
		return () -> aElement.getQueryString() + "?";
	}

	/**
	 * This method implements the inverse path in SPARQL 1.1 Property Paths.
	 * see https://www.w3.org/TR/sparql11-property-paths/
	 * 
	 * @param aElement
	 * @return
	 */
	private RdfPredicate inversePath(RdfPredicate aElement) {
		return () -> "^" + aElement.getQueryString();
	}
}
