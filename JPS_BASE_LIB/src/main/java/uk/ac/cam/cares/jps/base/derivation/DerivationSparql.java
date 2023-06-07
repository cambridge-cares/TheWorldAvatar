package uk.ac.cam.cares.jps.base.derivation;

import static org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf.iri;

import java.io.IOException;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
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
import org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf;
import org.eclipse.rdf4j.sparqlbuilder.rdf.RdfPredicate;
import org.eclipse.rdf4j.model.vocabulary.OWL;
import org.eclipse.rdf4j.model.vocabulary.RDFS;
import org.eclipse.rdf4j.model.vocabulary.XSD;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;
import org.apache.http.ParseException;
import org.apache.http.client.methods.CloseableHttpResponse;
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

	public static String derivednamespace = "https://www.theworldavatar.com/kg/ontoderivation/";

	// placeholder string used by method getAllDerivations()
	private static final String PLACEHOLDER = "http://This_is_a_placeholder_string";
	// placeholder Iri used by method getDerivation(String rootDerivationIRI)
	private static final Iri PLACEHOLDER_IRI = iri(PLACEHOLDER);

	// status concepts
	private static String REQUESTED = "Requested";
	private static String INPROGRESS = "InProgress";
	private static String FINISHED = "Finished";
	private static String ERROR = "Error";

	// derivation types
	public static final String DERIVATION = "Derivation";
	public static final String DERIVATIONWITHTIMESERIES = "DerivationWithTimeSeries";
	public static final String DERIVATIONASYN = "DerivationAsyn";
	public static final String ONTODERIVATION_DERIVATION = derivednamespace + DERIVATION;
	public static final String ONTODERIVATION_DERIVATIONASYN = derivednamespace + DERIVATIONASYN;
	public static final String ONTODERIVATION_DERIVATIONWITHTIMESERIES = derivednamespace + DERIVATIONWITHTIMESERIES;

	// prefix/namespace
	private static Prefix prefixAgent = SparqlBuilder.prefix("agent",
			iri("http://www.theworldavatar.com/ontology/ontoagent/MSM.owl#"));
	private static Prefix prefixDerived = SparqlBuilder.prefix("derived", iri(derivednamespace));
	private static Prefix prefixTime = SparqlBuilder.prefix("time", iri("http://www.w3.org/2006/time#"));

	// classes
	private static Iri Service = prefixAgent.iri("Service");
	private static Iri Operation = prefixAgent.iri("Operation");
	private static Iri MessageContent = prefixAgent.iri("MessageContent");
	private static Iri MessagePart = prefixAgent.iri("MessagePart");
	private static Iri TimePosition = prefixTime.iri("TimePosition");
	private static Iri Derivation = prefixDerived.iri(DERIVATION);
	private static Iri DerivationWithTimeSeries = prefixDerived.iri(DERIVATIONWITHTIMESERIES);
	private static Iri DerivationAsyn = prefixDerived.iri(DERIVATIONASYN);
	private static Iri Status = prefixDerived.iri("Status");
	private static Iri Requested = prefixDerived.iri(REQUESTED);
	private static Iri InProgress = prefixDerived.iri(INPROGRESS);
	private static Iri Finished = prefixDerived.iri(FINISHED);
	private static Iri Error = prefixDerived.iri(ERROR);
	private static Iri InstantClass = prefixTime.iri("Instant");
	private static Iri UnixTime = iri("http://dbpedia.org/resource/Unix_time");

	// object properties
	private static Iri hasHttpUrl = prefixAgent.iri("hasHttpUrl");
	private static Iri hasOperation = prefixAgent.iri("hasOperation");
	private static Iri hasInput = prefixAgent.iri("hasInput");
	private static Iri hasOutput = prefixAgent.iri("hasOutput");
	private static Iri hasMandatoryPart = prefixAgent.iri("hasMandatoryPart");
	private static Iri hasType = prefixAgent.iri("hasType");
	private static Iri hasName = prefixAgent.iri("hasName");
	private static Iri isDerivedFrom = prefixDerived.iri("isDerivedFrom");
	private static Iri isDerivedUsing = prefixDerived.iri("isDerivedUsing");
	private static Iri belongsTo = prefixDerived.iri("belongsTo");
	private static Iri hasStatus = prefixDerived.iri("hasStatus");
	private static Iri hasNewDerivedIRI = prefixDerived.iri("hasNewDerivedIRI");
	private static Iri hasTime = prefixTime.iri("hasTime");
	private static Iri numericPosition = prefixTime.iri("numericPosition");
	private static Iri hasTRS = prefixTime.iri("hasTRS");
	private static Iri inTimePosition = prefixTime.iri("inTimePosition");

	// data properties
	private static Iri retrievedInputsAt = prefixDerived.iri("retrievedInputsAt");
	private static Iri uuidLock = prefixDerived.iri("uuidLock");

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
		statusMap.put(derivednamespace.concat(ERROR), StatusType.ERROR);
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

	//
	private static final Map<String, String> derivationToShortName;
	static {
		Map<String, String> derivationTypeShortNameMap = new HashMap<>();
		derivationTypeShortNameMap.put(derivednamespace.concat(DERIVATION), DERIVATION);
		derivationTypeShortNameMap.put(derivednamespace.concat(DERIVATIONWITHTIMESERIES), DERIVATIONWITHTIMESERIES);
		derivationTypeShortNameMap.put(derivednamespace.concat(DERIVATIONASYN), DERIVATIONASYN);
		derivationToShortName = derivationTypeShortNameMap;
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
	 * This method creates the OntoAgent instances in the KG given information about the agent I/O signature.
	 * @param ontoAgentServiceIRI
	 * @param ontoAgentOperationHttpUrl
	 * @param inputTypes
	 * @param outputTypes
	 */
	public void createOntoAgentInstance(String ontoAgentServiceIRI, String ontoAgentOperationHttpUrl, List<String> inputTypes, List<String> outputTypes) {
		String operationIRI = getNameSpace(ontoAgentServiceIRI) + "Operation_" + UUID.randomUUID().toString();
		String mcInputIRI = getNameSpace(ontoAgentServiceIRI) + "MessageContent_" + UUID.randomUUID().toString();
		String mcOutputIRI = getNameSpace(ontoAgentServiceIRI) + "MessageContent_" + UUID.randomUUID().toString();

		ModifyQuery modify = Queries.MODIFY();

		modify.insert(iri(ontoAgentServiceIRI).isA(Service).andHas(hasOperation, iri(operationIRI)));
		modify.insert(iri(operationIRI).isA(Operation)
				.andHas(hasInput, iri(mcInputIRI))
				.andHas(hasOutput, iri(mcOutputIRI))
				.andHas(hasHttpUrl, Rdf.literalOfType(ontoAgentOperationHttpUrl, XSD.ANYURI)));
		modify.insert(iri(mcInputIRI).isA(MessageContent));
		for (String input : inputTypes) {
			String mpInputIRI = getNameSpace(ontoAgentServiceIRI) + "MessagePart_" + UUID.randomUUID().toString();
			modify.insert(iri(mcInputIRI).has(hasMandatoryPart, iri(mpInputIRI)));
			modify.insert(iri(mpInputIRI).isA(MessagePart).andHas(hasType, iri(input)));
		}

		modify.insert(iri(mcOutputIRI).isA(MessageContent));
		for (String output : outputTypes) {
			String mpOutput = getNameSpace(ontoAgentServiceIRI) + "MessagePart_" + UUID.randomUUID().toString();
			modify.insert(iri(mcOutputIRI).has(hasMandatoryPart, iri(mpOutput)));
			modify.insert(iri(mpOutput).isA(MessagePart).andHas(hasType, iri(output)));
		}

		// SPARQL update by insert-where clause to ensure one agent service don't get duplicated entries in KG
		// NOTE this implies that ONE AGENT SERVICE ONLY HAS ONE ONTOAGENT:OPERATION
		// NOTE that below we are using a work-around to achieve the where clause
		// in practice, "WHERE { FILTER NOT EXISTS { <http://agent> a ontoagent:Service } }" should be sufficient
		// however, if the where clause is constructed using below line of code
		// modify.where(GraphPatterns.filterNotExists(iri(ontoAgentServiceIRI).isA(Service)));
		// one can only get "WHERE { <http://agent> a ontoagent:Service }" due to the implementation of SparqlBuilder
		// therefore, here we make it SPARQL update with sub query to determine if the agent service IRI already exist
		// the complete where clause looks like:
		// WHERE { { SELECT *
		// WHERE {  VALUES ( ?x0 )   { (<http://b6b6f047-8ee5-4ee8-8d6a-dd2c23a8c944>) }
		// FILTER NOT EXISTS { ?x0 a agent:Service . } }
		// } }
		SubSelect sub = GraphPatterns.select();
		Variable servicePlaceholder = sub.var();
		ValuesPattern serviceVP = new ValuesPattern(servicePlaceholder);
		serviceVP.addValuePairForMultipleVariables(iri(ontoAgentServiceIRI));
		sub.where(serviceVP, GraphPatterns.filterNotExists(servicePlaceholder.isA(Service)));
		modify.where(sub);

		storeClient.executeUpdate(modify.prefix(prefixAgent).getQueryString());
	}

	/**
	 * This method unifies all methods that create multiple derivations in one go.
	 * 
	 * @param entitiesList
	 * @param agentIRIList
	 * @param inputsList
	 * @param derivationTypeList
	 * @param forAsyncUpdateFlagList
	 * @return
	 */
	List<String> unifiedBulkCreateDerivations(List<List<String>> entitiesList, List<String> agentIRIList,
			List<List<String>> inputsList, List<String> derivationTypeList, List<Boolean> forAsyncUpdateFlagList) {
		ModifyQuery modify = Queries.MODIFY();

		if (entitiesList.size() != agentIRIList.size()) {
			String errmsg = "Size of entities list is different from agent IRI list";
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

		allowedAsDerivationOutputs(entitiesList.stream().flatMap(List::stream).collect(Collectors.toList()));

		List<String> derivations = new ArrayList<>();
		Map<String, List<String>> entityInput = new HashMap<>();

		for (int i = 0; i < entitiesList.size(); i++) {
			List<String> entities = entitiesList.get(i);
			List<String> inputs = inputsList.get(i);
			// throw error if any of the IRIs appear in both inputs and entities
			Set<String> intersection = entities.stream().distinct().filter(inputs::contains).collect(Collectors.toSet());
			if (!intersection.isEmpty()) {
				throw new JPSRuntimeException(
						"Intersection between inputs and outputs for the same derivation markup: " + intersection);
			}
			// add pair of output entity and input
			// this will help to detect same IRI been marked as belongsTo more than one derivation
			entities.stream().forEach(en -> {
				if (entityInput.containsKey(en)) {
					throw new JPSRuntimeException("Entity will be marked belongsTo more than one derivations: " +
							en + ". Inputs: " + entityInput.get(en) + "; and inputs: " + inputs);
				} else {
					entityInput.put(en, inputs);
				}
			});
			String agentIRI = agentIRIList.get(i);
			Boolean forUpdateFlag = forAsyncUpdateFlagList.get(i);
			String derivationType = derivationTypeList.get(i);
			// create a unique IRI for this new derived quantity
			String derivedQuantity = createDerivationIRI(derivationType);
			derivations.add(derivedQuantity);
			Iri derivedIri = iri(derivedQuantity);

			modify.insert(derivedIri.isA(derivationToIri.get(derivationType)));

			// add belongsTo
			for (String entity : entities) {
				modify.insert(iri(entity).has(belongsTo, derivedIri));
			}

			// link inputs
			for (String input : inputs) {
				modify.insert(derivedIri.has(isDerivedFrom, iri(input)));
			}

			// add status triples if it's async derivation for update
			if (forUpdateFlag) {
				Iri statusIri = iri(derivationInstanceBaseURL + "status_" + UUID.randomUUID().toString());
				modify.insert(derivedIri.has(hasStatus, statusIri));
				modify.insert(statusIri.isA(Requested));
			}

			// link to agent
			// here it is assumed that an agent only has one operation
			modify.insert(derivedIri.has(isDerivedUsing, iri(agentIRI)));
		}

		// put sub query to retrieve the pure inputs whose timestamp is missing and add timestamp in insert clause
		SubSelect sub = GraphPatterns.select();
		Variable pureInput = sub.var();
		Variable pureInputTimeInstant = sub.var();
		Variable pureInputTimePosition = sub.var();
		Variable existingTimestamp = sub.var();
		Variable anyDerivation = sub.var();
		long ts = Instant.now().getEpochSecond(); // get current epoch as timestamp
		modify.insert(pureInput.has(hasTime, pureInputTimeInstant));
		modify.insert(pureInputTimeInstant.isA(InstantClass).andHas(inTimePosition, pureInputTimePosition));
		modify.insert(pureInputTimePosition.isA(TimePosition).andHas(numericPosition, ts).andHas(hasTRS, UnixTime));

		// prepare values clause for the input timestamps
		// should only add timestamp to potential pure inputs, i.e. those in inputsList and not in entitiesList
		List<String> allOutputs = entitiesList.stream().flatMap(List::stream).collect(Collectors.toList());
		List<String> potentialPureInputs = inputsList.stream().flatMap(List::stream).collect(Collectors.toList())
				.stream().filter(e -> !allOutputs.contains(e)).collect(Collectors.toList());
		// if the potentialPureInputs is empty, then means all inputs/entities cancels out
		// this will lead to a circular dependency of the derivations to be created in bulk
		if (potentialPureInputs.isEmpty()) {
			throw new JPSRuntimeException(
				"All inputs are cancelled out by outputs of derivation, resulting a circular dependency, inputs: " +
				inputsList + "; outputs: " + entitiesList);
		}
		ValuesPattern pureInputTimestampVP = new ValuesPattern(pureInput, pureInputTimeInstant, pureInputTimePosition);
		potentialPureInputs.stream().forEach(pureInputIri -> {
			// create timestamp value pairs for the given entity
			pureInputTimestampVP.addValuePairForMultipleVariables(iri(pureInputIri), iri(createTimeIRI()), iri(createTimeIRI()));
		});
		// construct the sub query
		// NOTE that the whole graph pattern (GraphPatterns) in the where clause of sub query is made OPTIONAL
		// so that the sub query always return something even when no pure inputs are missing timestamp, i.e. [{}]
		// this ensures the above insert clause still proceed to add other triples of derivation
		sub.select(pureInput, pureInputTimeInstant, pureInputTimePosition)
				.where(GraphPatterns.and(pureInputTimestampVP,
						// filter out input as derived data
						GraphPatterns.filterNotExists(pureInput.has(belongsTo, anyDerivation)),
						 // filter out pure inputs already have timestamp
						GraphPatterns.filterNotExists(pureInput.has(PropertyPaths.path(hasTime, inTimePosition, numericPosition),
								existingTimestamp))).optional());

		// execute the update
		modify.prefix(prefixDerived, prefixAgent, prefixTime).where(sub);
		storeClient.executeUpdate(modify.getQueryString());
		return derivations;
	}

	/**
	 * This method creates a new instance of derived quantity, grouping the given
	 * entities under this instance,
	 * whenever this derived quantity gets updated, the provided entities will get
	 * deleted by the client.
	 * This method does NOT create statements about the OntoAgent:Operation and
	 * OntoAgent:hasHttpUrl. Rather, this method assumes the triples
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
		List<String> derivations = bulkCreateDerivations(
			Arrays.asList(entities), Arrays.asList(agentIRI), Arrays.asList(inputs));
		return derivations.get(0);
	}

	/**
	 * This method creates a new IRI of derivation instance.
	 * 
	 * @return
	 */
	String createDerivationIRI(String derivationType) {
		// create a unique IRI for this new derived quantity
		if (derivationTypes.contains(derivationType)) {
			return derivationInstanceBaseURL + derivationToShortName.get(derivationType) + "_" + UUID.randomUUID().toString();
		} else {
			String errmsg = "ERROR: derivation type " + derivationType + " is not supported";
			LOGGER.fatal(errmsg);
			throw new JPSRuntimeException(errmsg);
		}
	}

	/**
	 * This method creates a new IRI for timestamp related instances.
	 * @return
	 */
	String createTimeIRI() {
		return derivationInstanceBaseURL + "time_" + UUID.randomUUID().toString();
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

		query.select(url).where(queryPattern).prefix(prefixAgent);

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
		// first check if the entities are allowed to be marked as derivation outputs
		// (1) they do not already belongsTo other derivation
		// (2) they do not have timestamp, i.e. not pure inputs
		allowedAsDerivationOutputs(entities);
		for (String entity : entities) {
			modify.insert(iri(entity).has(belongsTo, iri(derivationIRI)));
		}

		// add <derivation> <isDerivedFrom> <input> for all inputs
		inputsIRI.forEach(input -> {
			modify.insert(iri(derivationIRI).has(isDerivedFrom, iri(input)));
		});

		// add <derivation> <isDerivedUsing> <agentIRI>
		modify.insert(iri(derivationIRI).has(isDerivedUsing, iri(agentIRI)));

		// add timestamp instance for the created derivation, unix time following the
		// w3c standard
		Iri timeInstantIri = iri(createTimeIRI());
		Iri timeUnixIri = iri(createTimeIRI());
		modify.insert(iri(derivationIRI).has(hasTime, timeInstantIri));
		modify.insert(timeInstantIri.isA(InstantClass).andHas(inTimePosition, timeUnixIri));
		modify.insert(timeUnixIri.isA(TimePosition).andHas(numericPosition, retrievedInputsAt).andHas(hasTRS, UnixTime));

		// execute SPARQL update
		storeClient.setQuery(modify.prefix(prefixTime, prefixDerived, prefixAgent).getQueryString());
		storeClient.executeUpdate();
	}

	/**
	 * same method as above but this creates multiple derivations in 1 go
	 * 
	 * @param entities
	 * @param agentIRI
	 * @param inputs
	 */
	List<String> bulkCreateDerivations(List<List<String>> entitiesList, List<String> agentIRIList, List<List<String>> inputsList) {
		List<String> derivationTypeList = IntStream.range(0, entitiesList.size()).mapToObj(i -> ONTODERIVATION_DERIVATION)
				.collect(Collectors.toList());
		List<Boolean> forAsyncUpdateFlagList = IntStream.range(0, entitiesList.size()).mapToObj(i -> false)
				.collect(Collectors.toList());
		return unifiedBulkCreateDerivations(entitiesList, agentIRIList, inputsList,
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
	 * @param inputs
	 */
	String createDerivationWithTimeSeries(List<String> entities, String agentIRI, List<String> inputs) {
		List<String> derivations = bulkCreateDerivationsWithTimeSeries(
				Arrays.asList(entities), Arrays.asList(agentIRI), Arrays.asList(inputs));
		return derivations.get(0);
	}

	/**
	 * same method as above but initialise a large number of derivations in 1 go
	 * 
	 * @param entitiesList
	 * @param agentIRIList
	 * @param inputsList
	 */
	List<String> bulkCreateDerivationsWithTimeSeries(List<List<String>> entitiesList, List<String> agentIRIList, List<List<String>> inputsList) {
		List<String> derivationTypeList = IntStream.range(0, entitiesList.size()).mapToObj(i -> ONTODERIVATION_DERIVATIONWITHTIMESERIES)
				.collect(Collectors.toList());
		List<Boolean> forAsyncUpdateFlagList = IntStream.range(0, entitiesList.size()).mapToObj(i -> false)
				.collect(Collectors.toList());
		return unifiedBulkCreateDerivations(entitiesList, agentIRIList, inputsList,
				derivationTypeList, forAsyncUpdateFlagList);
	}

	/**
	 * This method creates a new instance of asynchronous derived quantity, grouping
	 * the given entities under this instance,
	 * whenever this derived quantity gets updated, the provided entities will get
	 * deleted by the client.
	 * This method does NOT create statements about the OntoAgent:Operation and
	 * OntoAgent:hasHttpUrl. Rather, this method assumes the triples
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
		List<String> derivations = bulkCreateDerivationsAsync(
				Arrays.asList(entities), Arrays.asList(agentIRI), Arrays.asList(inputs), Arrays.asList(forUpdate));
		return derivations.get(0);
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
		List<String> derivationTypeList = IntStream.range(0, entitiesList.size()).mapToObj(i -> ONTODERIVATION_DERIVATIONASYN)
				.collect(Collectors.toList());
		return unifiedBulkCreateDerivations(entitiesList, agentIRIList, inputsList,
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
		return unifiedBulkCreateDerivations(entitiesList, agentIRIList, inputsList,
				derivationRdfTypeList, forAsyncUpdateFlagList);
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

		TriplePattern insertTp = iri(derivation).has(hasStatus, iri(statusIRI));
		TriplePattern insertTpRdfType = iri(statusIRI).isA(Requested);

		modify.prefix(prefixDerived).insert(insertTp);
		modify.prefix(prefixDerived).insert(insertTpRdfType);

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
		GraphPattern queryGp = GraphPatterns.and(
			iri(derivation).has(hasStatus, status), status.isA(statusType));
		TriplePattern deleteTp = status.isA(statusType);
		TriplePattern insertTpRdfType = status.isA(InProgress);

		// record timestamp at the point the derivation status is marked as InProgress
		// <derivation> <retrievedInputsAt> timestamp.
		long retrievedInputsAtTimestamp = Instant.now().getEpochSecond();
		TriplePattern insertTpRetrievedInputsAt = iri(derivation).has(retrievedInputsAt,
				retrievedInputsAtTimestamp);
		// add uuidLock to the derivation, so that the agent can query if the SPARQL update is successful
		// this is to avoid the case where concurrent updates are made to the same derivation by different agent threads
		String uuid = UUID.randomUUID().toString();
		TriplePattern insertTpUuidLock = iri(derivation).has(uuidLock, uuid);
		// the retrievedInputsAt data property should only be added when there's no such
		// data property already - this will prevent duplicate timestamp in case of
		// super fast monitoring
		TriplePattern existingRetrievedInputsAtPattern = iri(derivation).has(retrievedInputsAt, existingTimestamp);
		modify.delete(deleteTp).insert(insertTpRdfType, insertTpRetrievedInputsAt, insertTpUuidLock)
				.where(GraphPatterns.and(queryGp.filterNotExists(existingRetrievedInputsAtPattern)))
				.prefix(prefixDerived);
		storeClient.executeUpdate(modify.getQueryString());

		// check if the uuid added to the derivation is the same one
		query = Queries.SELECT();
		Variable addedUUID = query.var();
		query.prefix(prefixDerived).select(addedUUID).where(iri(derivation).has(uuidLock, addedUUID));
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
	 * This method adds a uuidLock to the derivation at Finished status. This is to
	 * avoid the case where concurrent cleaning up are made to the same derivation
	 * by different agent threads.
	 * 
	 * @param derivation
	 * @return
	 */
	boolean addUuidLockToFinishedStatus(String derivation) {
		SelectQuery query = Queries.SELECT();
		ModifyQuery modify = Queries.MODIFY();
		Variable status = query.var();
		Variable existingUuidLock = query.var();

		// query the status and statusType (Finished)
		GraphPattern queryGp = GraphPatterns.and(
			iri(derivation).has(hasStatus, status), status.isA(Finished));

		// add uuidLock to the derivation, so that the agent can query if the SPARQL update is successful
		// this is to avoid the case where concurrent cleaning up are made to the same derivation by different agent threads
		String uuid = UUID.randomUUID().toString();
		TriplePattern insertTpUuidLock = iri(derivation).has(uuidLock, uuid);
		// the uuidLock data property should only be added when there's no such
		// data property already - as this indicates there is already another
		// thread that is processing the derivation
		TriplePattern existingUuidLockPattern = iri(derivation).has(uuidLock, existingUuidLock);
		modify.insert(insertTpUuidLock)
				.where(GraphPatterns.and(queryGp.filterNotExists(existingUuidLockPattern)))
				.prefix(prefixDerived);
		storeClient.executeUpdate(modify.getQueryString());

		// query if the uuidLock was added successfully
		query = Queries.SELECT();
		Variable addedUUID = query.var();
		query.prefix(prefixDerived).select(addedUUID).where(iri(derivation).has(uuidLock, addedUUID));
		JSONArray queryResult = storeClient.executeQuery(query.getQueryString());
		if (queryResult.isEmpty()) {
			// if the query result is empty, it means the uuidLock was not added successfully
			// therefore another thread was already processing the derivation and cleaned up
			return false;
		} else {
			JSONObject queryResultObject = queryResult.getJSONObject(0);
			String addedUUIDString = queryResultObject.getString(addedUUID.getQueryString().substring(1));
			if (addedUUIDString.equals(uuid)) {
				return true;
			} else {
				// if the uuidLock added is not the same as the one generated by this thread,
				// it means another thread was already processing the derivation and still cleaning up
				return false;
			}
		}
	}

	/**
	 * This method marks the status of the derivation as "Error" and writes
	 * the exception stack trace to triple store. It should be called if the
	 * agent ran into exception during handling the derivation.
	 *
	 * @param derivationIRI
	 * @param exc
	 * @return
	 */
	String markAsError(String derivationIRI, Exception exc) {
		SelectQuery query = Queries.SELECT();
		ModifyQuery modify = Queries.MODIFY();
		Variable status = query.var();
		Variable statusType = query.var();
		Variable uuid = query.var();

		GraphPattern queryGp = GraphPatterns.and(
			iri(derivationIRI).has(hasStatus, status), status.isA(statusType),
			iri(derivationIRI).has(uuidLock, uuid).optional());
		TriplePattern deleteTp = status.isA(statusType);
		TriplePattern deleteUuidLock = iri(derivationIRI).has(uuidLock, uuid);
		TriplePattern insertTpTdfType = status.isA(Error);

		modify.delete(deleteTp, deleteUuidLock).where(queryGp).prefix(prefixDerived);
		modify.insert(insertTpTdfType);

		// add stack trace to triple store
		StringBuilder bld = new StringBuilder();
		bld.append(exc.getClass().toString() + ": \n");
		bld.append(exc.getMessage() + "\n");
		for (StackTraceElement ste : exc.getStackTrace()) {
			bld.append(ste.toString() + "\n");
		}

		String excComment = escapeSequences(bld.toString());
		modify.insert(status.has(iri(RDFS.COMMENT.toString()), excComment));

		storeClient.executeUpdate(modify.getQueryString());
		return excComment;
	}

	/**
	 * This method retrieves a mapped list of derivations that <isDerivedUsing> a
	 * given <agentIRI> and their error message is they are in Error status.
	 *
	 * @param agentIRI
	 * @return
	 */
	List<Derivation> getDerivationsInErrorStatus(String agentIRI) {
		SelectQuery query = Queries.SELECT();
		Variable derivation = query.var();
		Variable derivationType = query.var();
		Variable errMsg = query.var();

		ValuesPattern derivationTypeVP = new ValuesPattern(derivationType,
				derivationTypes.stream().map(i -> derivationToIri.get(i)).collect(Collectors.toList()));
		query.prefix(prefixDerived).select(derivation, derivationType, errMsg)
			.where(derivationTypeVP, derivation.has(isDerivedUsing, iri(agentIRI)).andIsA(derivationType),
				derivation.has(PropertyPaths.path(hasStatus, iri(RDFS.COMMENT.toString())), errMsg));
		JSONArray queryResult = storeClient.executeQuery(query.getQueryString());

		List<Derivation> derivations = new ArrayList<>();
		for (int i = 0; i < queryResult.length(); i++) {
			Derivation d = new Derivation(
					queryResult.getJSONObject(i).getString(derivation.getQueryString().substring(1)),
					queryResult.getJSONObject(i).getString(derivationType.getQueryString().substring(1)));
			d.setErrMsg(queryResult.getJSONObject(i).getString(errMsg.getQueryString().substring(1)));
			derivations.add(d);
		}

		return derivations;
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

		GraphPattern queryGp = GraphPatterns.and(
			iri(derivation).has(hasStatus, status), status.isA(statusType),
			iri(derivation).has(uuidLock, uuid));
		TriplePattern deleteTp = status.isA(statusType);
		TriplePattern deleteUuidLock = iri(derivation).has(uuidLock, uuid);
		TriplePattern insertTpRdfType = status.isA(Finished);

		modify.delete(deleteTp, deleteUuidLock).where(queryGp).prefix(prefixDerived);
		modify.insert(insertTpRdfType);

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

		query.prefix(prefixDerived).where(queryPattern, queryPattern2).select(status, statusType);

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
		query.prefix(prefixDerived).where(queryPattern);

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

			query.prefix(prefixDerived).where(derivedPattern).select(newDerivedIRI);
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
		addTimeInstance(Arrays.asList(entity));
	}

	/**
	 * This method adds timestamp to the given entities in bulk. It skips entities
	 * who already have a timestamp or is a derived data.
	 * 
	 * @param entities
	 */
	void addTimeInstance(List<String> entities) {
		Map<String, Long> entitiesTimestamp = new HashMap<>();
		entities.stream().forEach(en -> {
			if (!entitiesTimestamp.containsKey(en)) {
				entitiesTimestamp.put(en, (long) 0);
			}
		});
		addTimeInstance(entitiesTimestamp);
	}

	/**
	 * This method adds current timestamp to the given entities in bulk. It skips entities
	 * who already have a timestamp or is a derived data.
	 *
	 * @param entities
	 */
	void addTimeInstanceCurrentTimestamp(List<String> entities) {
		Map<String, Long> entitiesTimestamp = new HashMap<>();
		long timestamp = Instant.now().getEpochSecond();
		entities.stream().forEach(en -> {
			if (!entitiesTimestamp.containsKey(en)) {
				entitiesTimestamp.put(en, timestamp);
			}
		});
		addTimeInstance(entitiesTimestamp);
	}

	/**
	 * This method adds timestamp to the given entities in bulk. It skips entities
	 * who already have a timestamp or is a derived data.
	 *
	 * @param entities
	 */
	private void addTimeInstance(Map<String, Long> entitiesTimestamp) {
		// example complete SPARQL update string for two entities
		// PREFIX derived:
		// <https://www.theworldavatar.com/kg/ontoderivation/>
		// PREFIX time: <http://www.w3.org/2006/time#>
		// INSERT { ?instance time:hasTime ?timeInstant .
		// ?timeInstant a time:Instant ;
		// 	time:inTimePosition ?timeUnix .
		// ?timeUnix a time:TimePosition ;
		// 	time:numericPosition ?timestamp ;
		// 	time:hasTRS <http://dbpedia.org/resource/Unix_time> . }
		// WHERE { { SELECT ?instance ?timeInstant ?timeUnix ?timestamp
		// WHERE {  VALUES ( ?instance ?timeInstant ?timeUnix ?timestamp )
		//	{ (<http://entity1> <http://time_uuid1> <http://time_uuid2> 0)
		//	(<http://entity2> <http://time_uuid3> <http://time_uuid4> 0) }
		// FILTER NOT EXISTS { ?instance derived:belongsTo ?anyDerivation . } }
		// FILTER NOT EXISTS { ?instance time:hasTime/time:inTimePosition/time:numericPosition ?existingTime . } }
		// } }
		ModifyQuery modify = Queries.MODIFY();
		SubSelect sub = GraphPatterns.select();
		Variable instance = SparqlBuilder.var("instance");
		Variable timeInstant = SparqlBuilder.var("timeInstant");
		Variable timeUnix = SparqlBuilder.var("timeUnix");
		Variable timestamp = SparqlBuilder.var("timestamp");

		modify.insert(instance.has(hasTime, timeInstant));
		modify.insert(timeInstant.isA(InstantClass).andHas(inTimePosition, timeUnix));
		modify.insert(timeUnix.isA(TimePosition).andHas(numericPosition, timestamp).andHas(hasTRS, UnixTime));
		ValuesPattern vp = new ValuesPattern(instance, timeInstant, timeUnix, timestamp);
		entitiesTimestamp.forEach((en, ts) -> {
			// create timestamp value pairs for the given entity
			vp.addValuePairForMultipleVariables(iri(en), iri(createTimeIRI()), iri(createTimeIRI()), Rdf.literalOf(ts));
		});
		GraphPattern belongsToAnyDerivationGP = instance.has(belongsTo, SparqlBuilder.var("anyDerivation"));
		GraphPattern existTimestampGP = instance.has(
				PropertyPaths.path(hasTime, inTimePosition, numericPosition),
				SparqlBuilder.var("existingTime"));
		sub.select(instance, timeInstant, timeUnix, timestamp)
				.where(GraphPatterns.and(vp,
						GraphPatterns.filterNotExists(belongsToAnyDerivationGP),
						GraphPatterns.filterNotExists(existTimestampGP)));
		modify.prefix(prefixDerived, prefixTime).where(sub);

		storeClient.executeUpdate(modify.getQueryString());
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

		query.select(url).where(queryPattern).prefix(prefixAgent, prefixDerived);

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

		query.prefix(prefixDerived).where(queryPattern).select(input);
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

		query.prefix(prefixDerived, prefixAgent).where(agentTypePattern, derivationInputPattern, mappingPattern).select(input,
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

		query.prefix(prefixDerived, prefixAgent).where(agentTypePattern, inputValuesPattern, mappingPattern).select(input,
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

		query.prefix(prefixDerived, prefixAgent).where(agentTypePattern, derivationOutputPattern, mappingPattern).select(output,
				type);
		storeClient.setQuery(query.getQueryString());
		JSONArray queryResult = storeClient.executeQuery();
		List<String> matchingInstances = new ArrayList<>();
		for (int i = 0; i < queryResult.length(); i++) {
			matchingInstances.add(queryResult.getJSONObject(i).getString(outputKey));
		}
		return matchingInstances;
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

		query.prefix(prefixDerived, prefixAgent).select(derivation).where(queryPattern);
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

		query.prefix(prefixDerived, prefixAgent).select(derivation, statusType).where(queryPattern, optionalPattern);
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
		// <https://www.theworldavatar.com/kg/ontoderivation/>
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

		query.prefix(prefixDerived, prefixTime).select(upstreamDerivation, upstreamDerivationType)
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
		// <https://www.theworldavatar.com/kg/ontoderivation/>
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

		TriplePattern insertStatus = d.has(hasStatus, iri(statusIRI));
		TriplePattern insertStatusRdfType = iri(statusIRI).isA(Requested);

		modify.prefix(prefixTime, prefixDerived).insert(insertStatus, insertStatusRdfType).where(sub);

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

		query.prefix(prefixDerived).where(inputPattern, derivedPattern).select(input, derivedOfInput);
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
		query.prefix(prefixDerived).select(entity).where(queryPattern);

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
		query.prefix(prefixDerived).select(entity, rdfType, downstreamDerivation, downsDevRdfType).where(queryPattern,
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

		query.prefix(prefixDerived, prefixAgent).select(downstream, agentIRI).where(queryPattern);

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

			query.select(derived, entityType).where(queryPattern).prefix(prefixDerived);
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

			TriplePattern[] deleteTp = new TriplePattern[2];
			GraphPattern[] queryPattern = new GraphPattern[2];

			// case 1: entity is the object
			deleteTp[0] = subject.has(pred1, iri(entity));
			queryPattern[0] = deleteTp[0].optional();

			// case 2: entity is the subject
			deleteTp[1] = iri(entity).has(pred2, object);
			queryPattern[1] = deleteTp[1].optional();

			sub.select(subject, pred1, pred2, object).where(queryPattern);

			ModifyQuery modify = Queries.MODIFY();
			modify.delete(deleteTp).where(sub);

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
		modify.delete(tp1, tp2, tp3).where(tp1, tp2, gp).prefix(prefixDerived);

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

		query.prefix(prefixTime, prefixDerived).where(queryPattern, queryPattern2).select(time);

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
		TriplePattern deleteTp = unixtimeIRI.has(numericPosition, oldvalue);
		TriplePattern insertTp = unixtimeIRI.has(numericPosition, timestamp);

		sub.select(unixtimeIRI, oldvalue).where(queryPattern);

		ModifyQuery modify = Queries.MODIFY();
		modify.prefix(prefixTime).delete(deleteTp).insert(insertTp).where(sub);

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

		query.prefix(prefixDerived).select(type, instance).where(instancePattern);

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

		TriplePattern insertTp = iri(derived).has(isDerivedFrom, iri(input));

		modify.prefix(prefixDerived).insert(insertTp);

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
		modify.prefix(prefixDerived);

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
		TriplePattern deleteTp = unixtimeIRI.has(numericPosition, oldvalue);
		TriplePattern insertTp = unixtimeIRI.has(numericPosition, timestamp);

		sub.select(unixtimeIRI, oldvalue, status, type, newDerivedIRI).where(tsQueryPattern,
				statusQueryPattern.optional()); // statusQueryPattern is made optional

		modify.prefix(prefixTime, prefixDerived).delete(deleteTp, tp1, tp2, tp3).insert(insertTp).where(sub);

		storeClient.setQuery(modify.getQueryString());
		storeClient.executeUpdate();
	}

	@Deprecated
	void deleteDirectConnectionBetweenDerivations(Map<String, String> derivationPairMap) {
		ModifyQuery modify = Queries.MODIFY();
		derivationPairMap.forEach((downstream, upstream) -> {
			modify.delete(iri(downstream).has(isDerivedFrom, iri(upstream)));
		});
		modify.prefix(prefixDerived);
		storeClient.executeUpdate(modify.getQueryString());
	}

	/**
	 * Returns true if it is a asynchronous derived quantity.
	 * 
	 * @param kbClient
	 * @param derivedIri
	 * @return
	 */
	boolean isDerivedAsynchronous(String derivedIri) {
		SelectQuery query = Queries.SELECT();
		Variable type = query.var();
		TriplePattern tp = iri(derivedIri).isA(type);
		Expression<?> constraint = Expressions.equals(type, DerivationAsyn);

		GraphPattern queryPattern = tp.filter(constraint);

		query.prefix(prefixDerived).select(type).where(queryPattern);
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

		storeClient.executeUpdate(modify.prefix(prefixDerived).getQueryString());
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
		Variable derivationPreBind = query.var();
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
				new ValuesPattern(derivationType,
						// somehow we need to stream and collect derivationTypes
						// otherwise reporting constructor not found error for ValuesPattern
						targetDerivationTypeIriList.stream().map(i -> i).collect(Collectors.toList())),
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
				GraphPattern rootDerivationPattern = iri(rootDerivationIRI).has(upstreamPath, derivationPreBind);
				// NOTE here we use a BIND clause to restrict the derivation instance to be queried in the following
				// query, this is to avoid the situation where the inputs of the derivation are too many thus the triple
				// store execute the two blocks of query separately and then join them together, which is observed to
				// crash the triple store
				GraphPattern derivationBindPattern = new Bind(derivationPreBind, derivation);
				query.where(rootDerivationPattern, derivationBindPattern);
			} else {
				ValuesPattern rootDerivationPattern = new ValuesPattern(derivation,
						Arrays.asList(iri(rootDerivationIRI)));
				query.where(rootDerivationPattern);
			}
		}

		query.select(derivation, input, entity, agentURL, derivationTimestamp, status, newDerivedIRI, inputTimestamp,
				derivationType, inputType, entityType, statusType, newDerivedIRIRdfType)
				.where(derivationPattern, statusPattern, entityPattern, inputTimestampPattern, inputTypePattern)
				.prefix(prefixDerived, prefixTime, prefixAgent);

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
			String inputRdfType = queryResult.getJSONObject(i).getString(inputType.getQueryString().substring(1));
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
			Entity inputEntity;
			Derivation directUpstream;
			if (derivationTypes.contains(inputRdfType)) { // the input might be a derivation
				if (derivationsMap.containsKey(inputIRI)) {
					directUpstream = derivationsMap.get(inputIRI);
				} else {
					directUpstream = new Derivation(inputIRI, inputRdfType);
					derivationsMap.put(inputIRI, directUpstream);
					derivationList.add(directUpstream);
				}
				// make the connection between the derivation and the upstream derivation
				derived.setDirectedUpstreams(directUpstream);
			} else { // this is a normal entity
				if (entitiesMap.containsKey(inputIRI)) {
					inputEntity = entitiesMap.get(inputIRI);
				} else {
					inputEntity = new Entity(inputIRI);
					entitiesMap.put(inputIRI, inputEntity);
				}
				inputEntity.setRdfType(inputRdfType);
				// if it's a pure input/derivation it will have a timestamp
				if (queryResult.getJSONObject(i).has(inputTimestamp.getQueryString().substring(1))) {
					inputEntity.setTimestamp(queryResult.getJSONObject(i).getLong(inputTimestamp.getQueryString().substring(1)));
				}
				derived.addInput(inputEntity);
			}

			// compared to the function getDerivations(), the output entity is made
			// optional, thus, here we check first if output exists
			if (queryResult.getJSONObject(i).has(entity.getQueryString().substring(1))) {
				String entityIRI = queryResult.getJSONObject(i).getString(entity.getQueryString().substring(1));
				Entity entityEntity;
				if (entitiesMap.containsKey(entityIRI)) {
					entityEntity = entitiesMap.get(entityIRI);
				} else {
					entityEntity = new Entity(entityIRI);
					entitiesMap.put(entityIRI, entityEntity);
				}

				// if rdf type exists
				if (queryResult.getJSONObject(i).has(entityType.getQueryString().substring(1))) {
					entityEntity
							.setRdfType(
									queryResult.getJSONObject(i).getString(entityType.getQueryString().substring(1)));
				}
				derived.addEntity(entityEntity);
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
					Entity newEntity;
					if (newDerivedIRIMap.containsKey(newIRI)) {
						newEntity = newDerivedIRIMap.get(newIRI);
					} else {
						newEntity = new Entity(newIRI);
						newDerivedIRIMap.put(newIRI, newEntity);
					}

					// if rdf type exists
					if (queryResult.getJSONObject(i).has(newDerivedIRIRdfType.getQueryString().substring(1))) {
						newEntity.setRdfType(queryResult.getJSONObject(i)
								.getString(newDerivedIRIRdfType.getQueryString().substring(1)));
					}
					derived.getStatus().addNewDerivedIRI(newEntity);
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
				.prefix(prefixDerived, prefixTime, prefixAgent);

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
			Entity inputEntity;
			if (entitiesMap.containsKey(inputIRI)) {
				inputEntity = entitiesMap.get(inputIRI);
			} else {
				inputEntity = new Entity(inputIRI);
				entitiesMap.put(inputIRI, inputEntity);
			}

			// if rdf type exists
			if (queryResult.getJSONObject(i).has(inputType.getQueryString().substring(1))) {
				inputEntity
						.setRdfType(queryResult.getJSONObject(i).getString(inputType.getQueryString().substring(1)));
			}

			// if it's a pure input it will have a timestamp
			if (queryResult.getJSONObject(i).has(inputTimestamp.getQueryString().substring(1))) {
				inputEntity.setTimestamp(queryResult.getJSONObject(i).getLong(inputTimestamp.getQueryString().substring(1)));
			}

			Entity entityEntity;
			if (entitiesMap.containsKey(entityIRI)) {
				entityEntity = entitiesMap.get(entityIRI);
			} else {
				entityEntity = new Entity(entityIRI);
				entitiesMap.put(entityIRI, entityEntity);
			}

			// if rdf type exists
			if (queryResult.getJSONObject(i).has(entityType.getQueryString().substring(1))) {
				entityEntity
						.setRdfType(queryResult.getJSONObject(i).getString(entityType.getQueryString().substring(1)));
			}

			// set properties of derivation
			derived.addEntity(entityEntity);
			derived.addInput(inputEntity);
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

		query.prefix(prefixTime, prefixDerived).where(queryPattern);

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

		TriplePattern deleteTp1 = entity.has(predicate1, object);
		TriplePattern deleteTp2 = subject.has(predicate2, entity);

		ModifyQuery modify = Queries.MODIFY();
		modify.delete(deleteTp1, deleteTp2)
				.where(entity.has(belongsTo, iri(derivation)), deleteTp1, subject.has(predicate2, entity).optional())
				.prefix(prefixDerived);

		storeClient.executeUpdate(modify.getQueryString());
	}

	/**
	 * This method cleans up the "Finished" async derivation in the knowledge graph by
	 * deleting all old instances, reconnecting the new generated derived IRI with
	 * derivations, deleting all status, and updating timestamp in one-go. This
	 * method is thread-safe.
	 * 
	 * @param derivation
	 */
	void cleanUpAsyncDerivation(String derivation) {
		// if another agent thread is cleaning up the same derivation concurrently
		// and succeeded before this thread, then this method will return false
		if (addUuidLockToFinishedStatus(derivation)) {
			Map<String, List<String>> connectionMap = mapNewOutputsToDownstream(derivation, true, null);
			reconnectAsyncDerivation(derivation, connectionMap);
		}
	}

	/**
	 * This method maps the new outputs of a derivation to its downstream derivation.
	 * It relies on the input signature specified in the OntoAgent instances that the
	 * downstream derivations "isDerivedUsing". The function returns a map that indicates
	 * the connection of new derived IRIs (if any) and downstream derivations. The map
	 * also includes the derivation as a key, and the corresponding value is a list of
	 * downstream derivations whose input signature does not match any of the new outputs
	 * of the derivation (this could be the case due to if some outputs are not generated).
	 * 
	 * @param derivation
	 * @param isAsync
	 * @param outputs
	 * @return
	 */
	Map<String, List<String>> mapNewOutputsToDownstream(String derivation, boolean isAsync, List<String> outputs) {
		SelectQuery query = Queries.SELECT().distinct();
		Variable d = SparqlBuilder.var("d");
		Variable entity = query.var();
		Variable existingDerivation = query.var();
		Variable timestamp = query.var();
		Variable downstreamDerivation = query.var();
		Variable inputType = query.var();

		// VALUES ?d { <derivation> }
		GraphPattern derivVP = new ValuesPattern(d, iri(derivation));
		GraphPattern outputsPattern;
		if (isAsync) {
			// ?d derived:hasStatus/derived:hasNewDerivedIRI ?x0 .
			outputsPattern = d.has(PropertyPaths.path(hasStatus, hasNewDerivedIRI), entity);
		} else {
			// VALUES ?x0 { <output1> <output2> ... }
			outputsPattern = new ValuesPattern(entity,
					outputs.stream().map(e -> iri(e)).collect(Collectors.toList()));
		}
		// ?x3 derived:isDerivedFrom/derived:belongsTo? ?d .
		GraphPattern downstreamGP = downstreamDerivation.has(PropertyPaths.path(isDerivedFrom, zeroOrOne(belongsTo)), d);
		// ?x3 derived:isDerivedUsing/agent:hasOperation/agent:hasInput/agent:hasMandatoryPart/agent:hasType ?x4 .
		// ?x0 a*/<http://www.w3.org/2000/01/rdf-schema#subClassOf>* ?x4 .
		GraphPattern mappingGP = GraphPatterns.and(
				downstreamDerivation.has(PropertyPaths.path(isDerivedUsing, hasOperation, hasInput, hasMandatoryPart, hasType), inputType),
				entity.has(PropertyPaths.path(PropertyPaths.zeroOrMore(RdfPredicate.a), PropertyPaths.zeroOrMore(iri(RDFS.SUBCLASSOF.toString()))), inputType));
		// OPTIONAL { ?x0 derived:belongsTo ?x1 . }
		GraphPattern belongsToDerivationGP = entity.has(belongsTo, existingDerivation).optional();
		// OPTIONAL { ?x0 time:hasTime/time:inTimePosition/time:numericPosition ?x2 . }
		GraphPattern tsGP = entity.has(PropertyPaths.path(hasTime, inTimePosition, numericPosition), timestamp).optional();

		query.prefix(prefixDerived, prefixTime, prefixAgent).select(entity, downstreamDerivation, existingDerivation, timestamp);
		query.where(derivVP, GraphPatterns.union(
				GraphPatterns.and(outputsPattern, GraphPatterns.and(downstreamGP, mappingGP).optional(), belongsToDerivationGP, tsGP),
						GraphPatterns.and(downstreamGP, GraphPatterns.filterNotExists(outputsPattern, mappingGP))));

		JSONArray queryResult = storeClient.executeQuery(query.getQueryString());

		Map<String, List<String>> connectionMap = new HashMap<>();
		connectionMap.put(derivation, new ArrayList<>());
		Map<String, String> enMap = new HashMap<>();
		List<String> enList = new ArrayList<>();
		for (int i = 0; i < queryResult.length(); i++) {
			if (queryResult.getJSONObject(i).has(entity.getQueryString().substring(1))) {
				String entityIRI = queryResult.getJSONObject(i).getString(entity.getQueryString().substring(1));
				if (!connectionMap.containsKey(entityIRI)) {
					connectionMap.put(entityIRI, new ArrayList<>());
				}
				if (queryResult.getJSONObject(i).has(downstreamDerivation.getQueryString().substring(1))) {
					String downstreamIRI = queryResult.getJSONObject(i).getString(downstreamDerivation.getQueryString().substring(1));
					connectionMap.get(entityIRI).add(downstreamIRI);
				}
				if (queryResult.getJSONObject(i).has(existingDerivation.getQueryString().substring(1))) {
					enMap.put(queryResult.getJSONObject(i).getString(entity.getQueryString().substring(1)),
						queryResult.getJSONObject(i).getString(existingDerivation.getQueryString().substring(1)));
				}
				if (queryResult.getJSONObject(i).has(timestamp.getQueryString().substring(1))) {
					enList.add(queryResult.getJSONObject(i).getString(entity.getQueryString().substring(1)));
				}
			} else if (queryResult.getJSONObject(i).has(downstreamDerivation.getQueryString().substring(1))) {
				connectionMap.get(derivation).add(queryResult.getJSONObject(i).getString(downstreamDerivation.getQueryString().substring(1)));
			}
		}

		checkIfAllowOutputs(enMap, enList);
		return connectionMap;
	}

	/**
	 * This method is a helper function for reconnecting new derived IRIs of sync
	 * derivations. It wraps around DerivationSparql::reconnectNewDerivedIRIs.
	 * 
	 * @param derivation
	 * @param connectionMap
	 * @param outputTriples
	 * @param retrievedInputsAtTs
	 * @return
	 */
	boolean reconnectSyncDerivation(String derivation, Map<String, List<String>> connectionMap,
			List<TriplePattern> outputTriples, Long retrievedInputsAtTs) {
		return reconnectNewDerivedIRIs(derivation, connectionMap, outputTriples, retrievedInputsAtTs, false);
	}

	/**
	 * This method is a helper function for reconnecting new derived IRIs of async
	 * derivations. It wraps around DerivationSparql::reconnectNewDerivedIRIs.
	 * 
	 * @param derivation
	 * @param connectionMap
	 * @return
	 */
	boolean reconnectAsyncDerivation(String derivation, Map<String, List<String>> connectionMap) {
		return reconnectNewDerivedIRIs(derivation, connectionMap, null, null, true);
	}

	/**
	 * This method reconnects new derived IRIs when a derivation is updated,
	 * (supporting both a-/sync), specifically, it does below five operations in one-go:
	 * (1) if exists, delete old outputs (entities) of the given derivation and its
	 * connections with downstream derivations;
	 * (2) delete status of the derivation if exist (applicable for outdated sync
	 * derivations in the mixed derivation DAGs and async derivations);
	 * (3) replace timestamp of the given derivation with the new timestamp recorded
	 * when the derivation inputs were retrieved (for sync this will be provided as
	 * as argument, for async this will be retrieved from the data property
	 * OntoDerivation:retrievedInputsAt when the derivation inputs were retrieved and
	 * the OntoDerivation:retrievedInputsAt will be deleted);
	 * (4) insert all new triples;
	 * (5) insert new derived IRIs to be connected with the given derivation using
	 * "belongsTo", also all its downstream derivations using "isDerivedFrom";
	 * (6) for those downstream derivations that no new derived IRIs are identified as
	 * mapped, connect them using "isDerivedFrom" directly with the derivation - the
	 * mapping are provided as an argument to this method.
	 * 
	 * It should be noted that this SPARQL update will ONLY be executed when the
	 * given derivation is outdated - this helps with addressing the concurrent HTTP
	 * request issue as detailed in
	 * https://github.com/cambridge-cares/TheWorldAvatar/issues/184, also prevents
	 * any potential faulty behaviour due to high frequency asynchronous derivation
	 * monitoring.
	 * 
	 * This method returns a boolean value to indicate if it is certain that the
	 * triples in the update endpoint are changed by the SPARQL update operation.
	 * For more details, see DerivationSparql::updateDerivationIfHttpPostAvailable.
	 * 
	 * @param derivation
	 * @param connectionMap
	 * @param outputTriples
	 * @param retrievedInputsAtTs
	 * @param isAsync
	 * @return
	 */
	private boolean reconnectNewDerivedIRIs(String derivation, Map<String, List<String>> connectionMap,
			List<TriplePattern> outputTriples, Long retrievedInputsAtTs, boolean isAsync) {
		ModifyQuery modify = Queries.MODIFY();
		SubSelect sub = GraphPatterns.select().distinct();

		if (!isAsync) {
			// insert all output triples if this is for sync derivations
			// if this is for async derivations, the output triples should already be written to KG
			outputTriples.forEach(t -> modify.insert(t));
		}

		// add triples of directed downstream derivations to be added
		if (connectionMap.containsKey(derivation)) {
			connectionMap.remove(derivation).forEach(d -> modify.insert(iri(d).has(isDerivedFrom, iri(derivation))));
		}

		// insert triples of new derived IRI and the derivations that it should be
		// connected to
		connectionMap.forEach((newIRI, downstreamDerivations) -> {
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
		Variable retrivVar = SparqlBuilder.var("retrievedInputsAt"); // retrievedInputsAt

		// variables for status related concepts
		Variable status = SparqlBuilder.var("status");
		Variable statusType = SparqlBuilder.var("statusType");
		Variable sndIRI = SparqlBuilder.var("sndIRI");

		// variables for uuidLock
		Variable uuid = SparqlBuilder.var("uuid");

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

		// this GraphPattern queries all the old outputs (entities), it is optional to
		// accommodate the situation where no outputs were presented for async
		// derivation created for new information
		GraphPattern entityPattern = GraphPatterns.and(e.has(belongsTo, d), e.has(p1, o), s.has(p2, e).optional())
				.optional();

		// below patterns query the status of this derivation if it exist, thus optional
		TriplePattern tp1 = d.has(hasStatus, status);
		TriplePattern tp2 = status.isA(statusType);
		// tp3 is optional in query for two reasons:
		// (1) to accommodate both sync and async derivations
		// (2) to accommodate the situation where no new outputs for async
		// but this mandatory in update so it still gets deleted
		TriplePattern tp3 = status.has(hasNewDerivedIRI, sndIRI);
		GraphPattern statusQueryPattern = isAsync ? GraphPatterns.and(
				tp1, tp2, tp3.optional()) : GraphPatterns.and(tp1, tp2, tp3.optional()).optional();

		// this GraphPattern queries directed downstream derivation if exist, thus optional
		GraphPattern directedDownstreamPattern = downd.has(isDerivedFrom, d).optional();

		// specific pattern for a-/sync
		// for async:
		// (1) query the uuidLock of this derivation
		// (2) query the timestamp retrievedInputsAt
		// for sync:
		// (1) assign timestamp retrievedInputsAt with values clause
		GraphPattern aOrSyncPattern = isAsync ? GraphPatterns.and(d.has(retrievedInputsAt, retrivVar),
				d.has(uuidLock, uuid)) : new ValuesPattern(retrivVar, Rdf.literalOf(retrievedInputsAtTs));

		// construct the sub query
		sub.select(d, unixtimeIRI, dTs, retrivVar, status, statusType, sndIRI, downd, e, p1, o, s, p2, uuid)
				.where(derivationPattern, entityPattern, statusQueryPattern, directedDownstreamPattern, aOrSyncPattern);

		// delete old outputs (entities) - basically delete this individual
		TriplePattern deleteEntityAsSubject = e.has(p1, o);
		TriplePattern deleteEntityAsObject = s.has(p2, e);

		// delete directed downstream derivation if exist
		TriplePattern deleteDirectedDownstream = downd.has(isDerivedFrom, d);

		// timestamp-related triples to add and delete
		TriplePattern deleteOldTimestamp = unixtimeIRI.has(numericPosition, dTs);
		TriplePattern insertNewTimestamp = unixtimeIRI.has(numericPosition, retrivVar);

		// construct the complete SPARQL update, note that some of the insert triples
		// have already been added at the beginning of this method
		modify.prefix(prefixTime, prefixDerived)
				.delete(deleteEntityAsSubject, deleteEntityAsObject, tp1, tp2, deleteDirectedDownstream,
						deleteOldTimestamp)
				.insert(insertNewTimestamp).where(sub);

		// NOTE below two triples are only for async, they are not added when handling sync derivations
		if (isAsync) {
			// delete retrievedInputsAt
			TriplePattern deleteRetrievedInputsAt = d.has(retrievedInputsAt, retrivVar);
			// delete uuidLock
			TriplePattern deleteUuidLock = d.has(uuidLock, uuid);
			modify.delete(tp3, deleteRetrievedInputsAt, deleteUuidLock);
		}

		return updateDerivationIfHttpPostAvailable(modify.getQueryString());
	}

	/**
	 * This method performs the SPARQL update when reconnecting derivation outputs
	 * via HTTP POST if blazegraph backended.
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
	 * @param sparqlUpdate
	 * @return
	 */
	boolean updateDerivationIfHttpPostAvailable(String sparqlUpdate) {
		try {
			if (storeClient.getClass() == RemoteStoreClient.class) {
				if (((RemoteStoreClient) storeClient).isUpdateEndpointBlazegraphBackended()) {
					try (CloseableHttpResponse httpResponse = ((RemoteStoreClient) storeClient)
							.executeUpdateByPost(sparqlUpdate)) {
						if (httpResponse.getStatusLine().getStatusCode() != 204 && httpResponse.getEntity() != null) {
							String html = EntityUtils.toString(httpResponse.getEntity());
							Pattern pattern = Pattern.compile("mutationCount=([0-9]+)");
							Matcher matcher = pattern.matcher(html);
							if (matcher.find() && Integer.parseInt(matcher.group(1)) > 0) {
								// only return true if the agent is able to parse "mutationCount=([0-9]+)" and
								// the parsed value is greater than 0
								LOGGER.debug("SPARQL update (" + sparqlUpdate + ") executed with mutationCount="
									+ matcher.group(1));
								return true;
							}
							LOGGER.debug("SPARQL update (" + sparqlUpdate + ") executed with mutationCount="
								+ matcher.group(1));
						}
					}
				} else {
					storeClient.executeUpdate(sparqlUpdate);
				}
			} else {
				storeClient.executeUpdate(sparqlUpdate);
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
	 * NOTE: this method does NOT remove triples about OntoAgent instances as
	 * they might be needed outside of the derivation framework
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
		Variable timeUnixIri = query.var();
		Variable timestamp = query.var();
		Variable trs = query.var();
		Variable agent = query.var();
		Variable derivationType = query.var();

		TriplePattern belongsToTp = entities.has(belongsTo, derivation);
		TriplePattern isDerivedFromTp = derivation.has(isDerivedFrom, inputs);
		TriplePattern derivationTypeTp = derivation.isA(derivationType);

		// timestamp
		TriplePattern timestampTp1 = derivation.has(hasTime, time);

		TriplePattern timeTpAll1 = time.isA(InstantClass).andHas(inTimePosition, timeUnixIri);
		TriplePattern timeTpAll2 = timeUnixIri.isA(TimePosition).andHas(numericPosition, timestamp).andHas(hasTRS,
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
				new ValuesPattern(derivationType,
						// somehow we need to stream and collect derivationTypes
						// otherwise reporting constructor not found error for ValuesPattern
						derivationTypes.stream().map(i -> i).collect(Collectors.toList())),
				isDerivedFromTp, timestampTp1, timeTpAll1, timeTpAll2,
				agentTp1, derivationTypeTp, belongsToTp.optional(), asyncStatusGP);

		modify.delete(belongsToTp, isDerivedFromTp,
				timestampTp1, timeTpAll1, timeTpAll2,
				agentTp1, derivationTypeTp, tp1, tp2, tp3).where(queryPattern)
				.prefix(prefixTime, prefixDerived, prefixAgent);

		storeClient.executeUpdate(modify.getQueryString());
	}

	void dropAllTimestamps() {
		ModifyQuery modify = Queries.MODIFY();
		SelectQuery query = Queries.SELECT();

		Variable entity = query.var();
		Variable time = query.var();
		Variable timeUnixIri = query.var();
		Variable timestamp = query.var();
		Variable trs = query.var();

		TriplePattern tp1 = entity.has(hasTime, time);
		TriplePattern tp2 = time.isA(InstantClass).andHas(inTimePosition, timeUnixIri);
		TriplePattern tp3 = timeUnixIri.isA(TimePosition).andHas(numericPosition, timestamp).andHas(hasTRS, trs);

		modify.delete(tp1, tp2, tp3).where(tp1, tp2, tp3).prefix(prefixTime);

		storeClient.executeUpdate(modify.getQueryString());
	}

	void dropTimestampsOf(List<String> entities) {
		ModifyQuery modify = Queries.MODIFY();
		SelectQuery query = Queries.SELECT();

		Variable entity = query.var();
		Variable time = query.var();
		Variable timeUnixIri = query.var();
		Variable timestamp = query.var();
		Variable trs = query.var();

		ValuesPattern entityVP = new ValuesPattern(entity, entities.stream().map(e -> iri(e)).collect(Collectors.toList()));
		TriplePattern tp1 = entity.has(hasTime, time);
		TriplePattern tp2 = time.isA(InstantClass).andHas(inTimePosition, timeUnixIri);
		TriplePattern tp3 = timeUnixIri.isA(TimePosition).andHas(numericPosition, timestamp).andHas(hasTRS, trs);

		modify.delete(tp1, tp2, tp3).where(entityVP, tp1, tp2, tp3).prefix(prefixTime);

		storeClient.executeUpdate(modify.getQueryString());
	}

	/**
	 * Updates timestamps of the given instances in one-go via SPARQL update with sub-query.
	 * Nothing happen to the instances that do not have timestamp.
	 * 
	 * @param instanceTimestampMap
	 */
	void updateTimestamps(Map<String, Long> instanceTimestampMap) {
		ModifyQuery modify = Queries.MODIFY();
		SubSelect sub = GraphPatterns.select();

		// complete SPARQL update with sub query string (assuming three instances in the Map):
		// PREFIX time: <http://www.w3.org/2006/time#>
		// DELETE { ?x1 time:numericPosition ?x2 . }
		// INSERT { ?x1 time:numericPosition ?x3 . }
		// WHERE { { SELECT ?x1 ?x2 ?x3
		// WHERE {  VALUES ( ?x0 ?x3 )
		//	{ (<http://instance1> 1666969105) (<http://instance2> 1666969105) (<http://instance3> 1666969105) } 
		// ?x0 time:hasTime/time:inTimePosition ?x1 .
		// ?x1 time:numericPosition ?x2 . }
		// } }

		Variable entity = sub.var();
		Variable timePosition = sub.var();
		Variable oldTimestamp = sub.var();
		Variable newTimestamp = sub.var();
		ValuesPattern entityNewTimestampVP = new ValuesPattern(entity, newTimestamp);
		// only update instances if the new timestamps are provided
		if (!instanceTimestampMap.isEmpty()) {
			instanceTimestampMap.forEach((en, ts) -> entityNewTimestampVP.addValuePairForMultipleVariables(
					iri(en), Rdf.literalOf(ts)));
			sub.select(timePosition, oldTimestamp, newTimestamp).where(entityNewTimestampVP,
					entity.has(PropertyPaths.path(hasTime, inTimePosition), timePosition),
					timePosition.has(numericPosition, oldTimestamp));
			modify.delete(timePosition.has(numericPosition, oldTimestamp));
			modify.insert(timePosition.has(numericPosition, newTimestamp));
			modify.where(sub);
			modify.prefix(prefixTime);

			storeClient.executeUpdate(modify.getQueryString());
		}
	}

	/**
	 * This method checks if the provided list of entities are allowed to be marked as derivation outputs.
	 * The performed checks ensure no circular dependency can be created.
	 * It throws an exception if any of the entities:
	 * (1) already belongsTo another derivation;
	 * (2) has timestamp, i.e. is pure inputs
	 *
	 * @param entities
	 */
	void allowedAsDerivationOutputs(List<String> entities) {
		SelectQuery query = Queries.SELECT();
		Variable entity = query.var();
		Variable derivation = query.var();
		Variable timestamp = query.var();

		ValuesPattern valuesPattern = new ValuesPattern(entity,
				entities.stream().map(e -> iri(e)).collect(Collectors.toList()));
		// OPTIONAL { ?x0 derived:belongsTo ?x1 . }
		GraphPattern belongsToDerivationGP = entity.has(belongsTo, derivation).optional();
		// OPTIONAL { ?x0 time:hasTime/time:inTimePosition/time:numericPosition ?x2 . }
		GraphPattern tsGP = entity.has(PropertyPaths.path(hasTime, inTimePosition, numericPosition), timestamp).optional();

		query.prefix(prefixDerived, prefixTime).select(entity, derivation, timestamp)
				.where(valuesPattern, belongsToDerivationGP, tsGP);

		JSONArray queryResult = storeClient.executeQuery(query.getQueryString());

		Map<String, String> enMap = new HashMap<>();
		List<String> enList = new ArrayList<>();
		for (int i = 0; i < queryResult.length(); i++) {
			if (queryResult.getJSONObject(i).has(derivation.getQueryString().substring(1))) {
				enMap.put(queryResult.getJSONObject(i).getString(entity.getQueryString().substring(1)),
					queryResult.getJSONObject(i).getString(derivation.getQueryString().substring(1)));
			}
			if (queryResult.getJSONObject(i).has(timestamp.getQueryString().substring(1))) {
				enList.add(queryResult.getJSONObject(i).getString(entity.getQueryString().substring(1)));
			}
		}

		checkIfAllowOutputs(enMap, enList);
	}

	Map<String, String> getDerivationsOf(List<String> entities) {
		SelectQuery query = Queries.SELECT();
		Variable entity = query.var();
		Variable derivation = query.var();

		GraphPattern queryPattern = entity.has(belongsTo, derivation);
		ValuesPattern valuesPattern = new ValuesPattern(entity,
				entities.stream().map(e -> iri(e)).collect(Collectors.toList()));

		query.select(entity, derivation).where(queryPattern, valuesPattern).prefix(prefixDerived);

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
		Map<String, Long> derivationTimeMap = new HashMap<>();

		String queryKey = "timestamp";
		SelectQuery query = Queries.SELECT();
		Variable time = SparqlBuilder.var(queryKey);

		GraphPattern queryPattern = iri(derivation).has(retrievedInputsAt, time);

		query.prefix(prefixDerived).select(time).where(queryPattern);
		JSONArray queryResult = storeClient.executeQuery(query.getQueryString());

		if (queryResult.length() > 1) {
			throw new JPSRuntimeException(
					"DerivedQuantitySparql: More than 1 time instance recorded for reading derivation inputs of <"
							+ derivation + ">");
		}

		try {
			long inputReadTimestamp = queryResult.getJSONObject(0).getLong(queryKey);
			derivationTimeMap.put(derivation, inputReadTimestamp);

			// delete triple {<derivation> <retrievedInputsAt> timestamp}
			ModifyQuery modify = Queries.MODIFY();
			TriplePattern deleteTimeRecord = iri(derivation).has(retrievedInputsAt, inputReadTimestamp);
			modify.prefix(prefixDerived).delete(deleteTimeRecord);
			storeClient.executeUpdate(modify.getQueryString());

			return derivationTimeMap;
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

	/**
	 * Escape sequence characters as defined in SPARQL 1.1.
	 * See https://www.w3.org/TR/2013/REC-sparql11-query-20130321/#grammarEscapes
	 *
	 * @param str
	 * @return
	 */
	public static String escapeSequences(String str) {
		return str.replace("\\", "\\\\")
				.replace("\t", "\\t")
				.replace("\n", "\\n")
				.replace("\r", "\\r")
				.replace("\b", "\\b")
				.replace("\f", "\\f")
				.replace("\"", "\\\"")
				.replace("'", "\\'");
	}

	void checkIfAllowOutputs(Map<String, String> enMap, List<String> enList) {
		// (1) they do not already belongsTo other derivation
		if (!enMap.isEmpty()) {
			String errmsg = "ERROR: some entities are already part of another derivation" + enMap.toString();
			LOGGER.fatal(errmsg);
			throw new JPSRuntimeException(errmsg);
		}

		// (2) they do not have timestamp, i.e. not pure inputs
		if (!enList.isEmpty()) {
			String errmsg = "ERROR: some entities have time instances and cannot be marked as derivation outputs" + enList.toString();
			LOGGER.fatal(errmsg);
			throw new JPSRuntimeException(errmsg);
		}
	}
}
