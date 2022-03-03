package uk.ac.cam.cares.jps.base.derivation;

import static org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf.iri;

import java.time.Instant;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.stream.Collectors;

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
import org.eclipse.rdf4j.model.vocabulary.RDF;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.interfaces.StoreClientInterface;

/**
 * SPARQL queries/updates for instances related to derived quantities
 * These functions are called by the DerivationClient class
 * @author Kok Foong Lee
 * @author Jiaru Bai
 *
 */
public class DerivationSparql{
	private StoreClientInterface storeClient;
	private String derivationInstanceBaseURL; // an example of this can be "https://www.example.com/triplestore/repository/"
	
	public static String derivednamespace = "https://github.com/cambridge-cares/TheWorldAvatar/blob/develop/JPS_Ontology/ontology/ontoderivation/OntoDerivation.owl#";
	
	// status concepts
	private static String PENDINGUPDATE = "PendingUpdate";
	private static String REQUESTED = "Requested";
	private static String INPROGRESS = "InProgress";
	private static String FINISHED = "Finished";
	
	// prefix/namespace
	private static Prefix p_agent = SparqlBuilder.prefix("agent",iri("http://www.theworldavatar.com/ontology/ontoagent/MSM.owl#"));
	private static Prefix p_derived = SparqlBuilder.prefix("derived",iri(derivednamespace));
	private static Prefix p_time = SparqlBuilder.prefix("time", iri("http://www.w3.org/2006/time#"));
	
	// classes
	private static Iri Service = p_agent.iri("Service");
	private static Iri Operation = p_agent.iri("Operation");
	private static Iri TimePosition = p_time.iri("TimePosition");
	private static Iri Derivation = p_derived.iri("Derivation");
	private static Iri DerivationWithTimeSeries = p_derived.iri("DerivationWithTimeSeries");
	private static Iri DerivationAsyn = p_derived.iri("DerivationAsyn");
	private static Iri Status = p_derived.iri("Status");
	private static Iri PendingUpdate = p_derived.iri(PENDINGUPDATE);
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
	
	// the derived quantity client relies on matching rdf:type to figure out which old instances to delete
	// if your instances have more than 1 rdf:type, you must add them to this list so that the client can figure out which to use
	private static List<Iri> classesToIgnore = Arrays.asList(iri(OWL.THING),iri(OWL.NAMEDINDIVIDUAL));
	
	// 
	private static final Map<String, StatusType> statusToType;
	static {
		Map<String, StatusType> statusMap = new HashMap<>();
		statusMap.put(derivednamespace.concat(PENDINGUPDATE), StatusType.PENDINGUPDATE);
		statusMap.put(derivednamespace.concat(REQUESTED), StatusType.REQUESTED);
		statusMap.put(derivednamespace.concat(INPROGRESS), StatusType.INPROGRESS);
		statusMap.put(derivednamespace.concat(FINISHED), StatusType.FINISHED);
		statusToType = statusMap;
	}
	
	private static final Logger LOGGER = LogManager.getLogger(DerivationSparql.class);
	
	/**
	 * This constructor is tagged as @Deprecated as ideally user should provide based URL when creating derivation instances.
	 * @param storeClient
	 */
	@Deprecated
	public DerivationSparql(StoreClientInterface storeClient) {
		this.storeClient = storeClient;
		this.derivationInstanceBaseURL = derivednamespace; // this line is added to the original constructor so that the current codes will not break
	}
	
	/**
	 * This constructor should be used to enable customised derivation instance base URL.
	 * @param storeClient
	 * @param derivationInstanceBaseURL
	 */
	public DerivationSparql(StoreClientInterface storeClient, String derivationInstanceBaseURL) {
		this.storeClient = storeClient;
		this.derivationInstanceBaseURL = derivationInstanceBaseURL;
	}
	
	/**
	 * creates a new instance of derived quantity, grouping the given entities under this instance
	 * whenever this derived quantity gets updated, the provided entities will get deleted by the client
	 * @param kbClient
	 * @param entities
	 * @param agentIRI
	 * @param agentURL
	 * @param inputs
	 */
	String createDerivation(List<String> entities, String agentIRI, String agentURL, List<String> inputs) {
		ModifyQuery modify = Queries.MODIFY();
		
		// create a unique IRI for this new derived quantity
		String derivedQuantity = derivationInstanceBaseURL + "derived" + UUID.randomUUID().toString();
		
		Iri derived_iri = iri(derivedQuantity);
		
		modify.insert(derived_iri.isA(Derivation));
		
		for (String entity : entities) {
			// ensure that given entity is not part of another derived quantity
			if (!hasBelongsTo(entity)) {
				modify.insert(iri(entity).has(belongsTo, derived_iri));
			} else {
				String errmsg = "<" + entity + "> is already part of another derivation";
				LOGGER.fatal(errmsg);
				throw new JPSRuntimeException(errmsg);
			}
		}
		
		// link to agent
		// here it is assumed that an agent only has one operation
		modify.insert(derived_iri.has(isDerivedUsing,iri(agentIRI)));
		String operation_iri = derivationInstanceBaseURL + UUID.randomUUID().toString();
		// add agent url
		modify.insert(iri(agentIRI).isA(Service).andHas(hasOperation, iri(operation_iri)));
		modify.insert(iri(operation_iri).isA(Operation).andHas(hasHttpUrl, iri(agentURL)));
		
		// link to each input
		for (String input : inputs) {
			modify.insert(derived_iri.has(isDerivedFrom, iri(input)));
		}
		
		storeClient.setQuery(modify.prefix(p_time,p_derived,p_agent).getQueryString());
		storeClient.executeUpdate();
		
		return derivedQuantity;
	}

	/**
	 * This method creates a new instance of derived quantity, grouping the given entities under this instance,
	 * whenever this derived quantity gets updated, the provided entities will get deleted by the client.
	 * This method primarily follows createDerivation(StoreClientInterface kbClient, List<String> entities,
	 * String agentIRI, String agentURL, List<String> inputs), except that this method does NOT create statements
	 * about the OntoAgent:Operation and OntoAgent:hasHttpUrl. Rather, this method assumes the triples
	 * {<Agent> <msm:hasOperation> <Operation>} and {<Operation> <msm:hasHttpUrl> <URL>}
	 * already exist in respective OntoAgent instances.
	 * @param kbClient
	 * @param entities
	 * @param agentIRI
	 * @param inputs
	 * @return
	 */
	String createDerivation(List<String> entities, String agentIRI, List<String> inputs) {
		ModifyQuery modify = Queries.MODIFY();

		// create a unique IRI for this new derived quantity
		String derivedQuantity = derivationInstanceBaseURL + "derived" + UUID.randomUUID().toString();

		Iri derived_iri = iri(derivedQuantity);

		modify.insert(derived_iri.isA(Derivation));

		for (String entity : entities) {
			// ensure that given entity is not part of another derived quantity
			if (!hasBelongsTo(entity)) {
				modify.insert(iri(entity).has(belongsTo, derived_iri));
			} else {
				String errmsg = "<" + entity + "> is already part of another derivation";
				LOGGER.fatal(errmsg);
				throw new JPSRuntimeException(errmsg);
			}
		}

		// link to agent
		// here it is assumed that an agent only has one operation
		modify.insert(derived_iri.has(isDerivedUsing,iri(agentIRI)));
		
		// link to each input
		for (String input : inputs) {
			modify.insert(derived_iri.has(isDerivedFrom, iri(input)));
		}
		
		modify.prefix(p_time,p_derived,p_agent);
		
		storeClient.setQuery(modify.prefix(p_time,p_derived,p_agent).getQueryString());
		storeClient.executeUpdate();
		return derivedQuantity;
	}
	
	/**
	 * same method as above but this creates multiple derivations in 1 go
	 * @param entities
	 * @param agentIRI
	 * @param agentURL
	 * @param inputs
	 */
	List<String> bulkCreateDerivations(List<List<String>> entitiesList, List<String> agentIRIList, List<String> agentURLList, List<List<String>> inputsList) {
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
		
		List<String> derivations = new ArrayList<>();
		
		for (int i = 0; i < entitiesList.size(); i++) {
			List<String> entities = entitiesList.get(i);
			List<String> inputs = inputsList.get(i);
			String agentIRI = agentIRIList.get(i);
			String agentURL = agentURLList.get(i);
			// create a unique IRI for this new derived quantity
			String derivedQuantity = derivationInstanceBaseURL + "derived" + UUID.randomUUID().toString();
			derivations.add(derivedQuantity);
			Iri derived_iri = iri(derivedQuantity);
			
			modify.insert(derived_iri.isA(Derivation));
			
			// add belongsTo
			for (String entity : entities) {
				modify.insert(iri(entity).has(belongsTo, derived_iri));
			}
			
			// link inputs
			for (String input : inputs) {
				modify.insert(derived_iri.has(isDerivedFrom, iri(input)));
			}
			
			// link to agent
			// here it is assumed that an agent only has one operation
			modify.insert(derived_iri.has(isDerivedUsing,iri(agentIRI)));
			String operation_iri = derivationInstanceBaseURL + UUID.randomUUID().toString();
			// add agent url
			modify.insert(iri(agentIRI).isA(Service).andHas(hasOperation, iri(operation_iri)));
			modify.insert(iri(operation_iri).isA(Operation).andHas(hasHttpUrl, iri(agentURL)));
		}
		
		modify.prefix(p_derived,p_agent);
		storeClient.executeUpdate(modify.getQueryString());
		return derivations;
	}
	
	/**
	 * same method as above, but for instances with time series and these instances do not get deleted
	 * therefore there is no need to provide a list of entities for this to annotate
	 * @param kbClient
	 * @param entities
	 * @param agentIRI
	 * @param agentURL
	 * @param inputs
	 */
	String createDerivationWithTimeSeries(List<String> entities, String agentIRI, String agentURL, List<String> inputs) {
		ModifyQuery modify = Queries.MODIFY();
		
		// create a unique IRI for this new derived quantity
		String derivedQuantity = derivationInstanceBaseURL + "derived" + UUID.randomUUID().toString();
		
		Iri derived_iri = iri(derivedQuantity);
		
		modify.insert(derived_iri.isA(DerivationWithTimeSeries));
		
		for (String entity : entities) {
			// ensure that given entity is not part of another derived quantity
			if (!hasBelongsTo(entity)) {
				modify.insert(iri(entity).has(belongsTo, derived_iri));
			} else {
				String errmsg = "<" + entity + "> is already part of another derivation";
				LOGGER.fatal(errmsg);
				throw new JPSRuntimeException(errmsg);
			}
		}
		
		// link to agent
		modify.insert(derived_iri.has(isDerivedUsing,iri(agentIRI)));
		String operation_iri = derivationInstanceBaseURL + UUID.randomUUID().toString();
		// add agent url
		modify.insert(iri(agentIRI).isA(Service).andHas(hasOperation, iri(operation_iri)));
		modify.insert(iri(operation_iri).isA(Operation).andHas(hasHttpUrl, iri(agentURL)));
		
		// link to each input
		for (String input : inputs) {
			modify.insert(derived_iri.has(isDerivedFrom, iri(input)));
		}
		
		modify.prefix(p_time,p_derived,p_agent);
		
		storeClient.setQuery(modify.prefix(p_time,p_derived,p_agent).getQueryString());
		storeClient.executeUpdate();
		
		return derivedQuantity;
	}
	
	/**
	 * same method as above but initialise a large number of derivations in 1 go
	 * @param entitiesList
	 * @param agentIRIList
	 * @param agentURLList
	 * @param inputsList
	 */
	List<String> bulkCreateDerivationsWithTimeSeries(List<List<String>> entitiesList, List<String> agentIRIList, List<String> agentURLList, List<List<String>> inputsList) {
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
		
		List<String> derivations = new ArrayList<>();
		
		for (int i = 0; i < entitiesList.size(); i++) {
			List<String> entities = entitiesList.get(i);
			List<String> inputs = inputsList.get(i);
			String agentIRI = agentIRIList.get(i);
			String agentURL = agentURLList.get(i);
			// create a unique IRI for this new derived quantity
			String derivedQuantity = derivationInstanceBaseURL + "derived" + UUID.randomUUID().toString();
			derivations.add(derivedQuantity);
			Iri derived_iri = iri(derivedQuantity);
			
			modify.insert(derived_iri.isA(DerivationWithTimeSeries));
			
			// add belongsTo
			for (String entity : entities) {
				modify.insert(iri(entity).has(belongsTo, derived_iri));
			}
			
			// link inputs
			for (String input : inputs) {
				modify.insert(derived_iri.has(isDerivedFrom, iri(input)));
			}
			
			// link to agent
			// here it is assumed that an agent only has one operation
			modify.insert(derived_iri.has(isDerivedUsing,iri(agentIRI)));
			String operation_iri = derivationInstanceBaseURL + UUID.randomUUID().toString();
			// add agent url
			modify.insert(iri(agentIRI).isA(Service).andHas(hasOperation, iri(operation_iri)));
			modify.insert(iri(operation_iri).isA(Operation).andHas(hasHttpUrl, iri(agentURL)));
		}
		
		modify.prefix(p_derived,p_agent);
		storeClient.executeUpdate(modify.getQueryString());
		return derivations;
	}
	
	/**
	 * This method creates a new instance of asynchronous derived quantity, grouping the given entities under this instance,
	 * whenever this derived quantity gets updated, the provided entities will get deleted by the client.
	 * This method primarily follows createDerivation(StoreClientInterface kbClient, List<String> entities,
	 * String agentIRI, String agentURL, List<String> inputs), except that this method does NOT create statements
	 * about the OntoAgent:Operation and OntoAgent:hasHttpUrl. Rather, this method assumes the triples
	 * {<Agent> <msm:hasOperation> <Operation>} and {<Operation> <msm:hasHttpUrl> <URL>}
	 * already exist in respective OntoAgent instances.
	 * @param kbClient
	 * @param entities
	 * @param agentIRI
	 * @param inputs
	 * @return
	 */
	String createDerivationAsyn(List<String> entities, String agentIRI, List<String> inputs) {
		ModifyQuery modify = Queries.MODIFY();

		// create a unique IRI for this new derived quantity
		String derivedQuantity = derivationInstanceBaseURL + "derivedAsyn_" + UUID.randomUUID().toString();

		Iri derived_iri = iri(derivedQuantity);

		modify.insert(derived_iri.isA(DerivationAsyn));

		for (String entity : entities) {
			// ensure that given entity is not part of another derived quantity
			if (!hasBelongsTo(entity)) {
				modify.insert(iri(entity).has(belongsTo, derived_iri));
			} else {
				String errmsg = "<" + entity + "> is already part of another derivation";
				LOGGER.fatal(errmsg);
				throw new JPSRuntimeException(errmsg);
			}
		}

		// link to agent
		// here it is assumed that an agent only has one operation
		modify.insert(derived_iri.has(isDerivedUsing,iri(agentIRI)));
		
		// link to each input
		for (String input : inputs) {
			modify.insert(derived_iri.has(isDerivedFrom, iri(input)));
		}
		
		modify.prefix(p_time,p_derived,p_agent);
		
		storeClient.setQuery(modify.prefix(p_time,p_derived,p_agent).getQueryString());
		storeClient.executeUpdate();
		return derivedQuantity;
	}
	
	/**
	 * check that the entity is part of another derived quantity, this is not allowed
	 * query triple - <entity> <belongsTo> ?x
	 * @param storeClient
	 * @param entity
	 * @return
	 */
	boolean hasBelongsTo(String entity) {
		SelectQuery query = Queries.SELECT();
		
		TriplePattern queryPattern = iri(entity).has(belongsTo, query.var());
		query.prefix(p_derived).where(queryPattern);
		
		JSONArray queryResult = storeClient.executeQuery(query.getQueryString());
		
		if (queryResult.isEmpty()) {
			return false;
		} else {
			return true;
		}
	}
	
	/**
	 * This method marks the status of the derivation as "PendingUpdate".
	 * @param derivation
	 */
	void markAsPendingUpdate(String derivation) {
		deleteStatus(derivation);
		ModifyQuery modify = Queries.MODIFY();
		
		String statusIRI = getNameSpace(derivation) + "status_" + UUID.randomUUID().toString();
		
		TriplePattern insert_tp = iri(derivation).has(hasStatus, iri(statusIRI));
		TriplePattern insert_tp_rdf_type = iri(statusIRI).isA(PendingUpdate);
		
		modify.prefix(p_derived).insert(insert_tp);
		modify.prefix(p_derived).insert(insert_tp_rdf_type);
		
		storeClient.executeUpdate(modify.getQueryString());
	}
	
	/**
	 * This method marks the status of the derivation as "Requested".
	 * @param storeClient
	 * @param derivation
	 */
	void markAsRequested(String derivation) {
		deleteStatus(derivation);
		ModifyQuery modify = Queries.MODIFY();
		
		String statusIRI = getNameSpace(derivation) + "status_" + UUID.randomUUID().toString();
		
		TriplePattern insert_tp = iri(derivation).has(hasStatus, iri(statusIRI));
		TriplePattern insert_tp_rdf_type = iri(statusIRI).isA(Requested);
		
		modify.prefix(p_derived).insert(insert_tp);
		modify.prefix(p_derived).insert(insert_tp_rdf_type);
		
		storeClient.executeUpdate(modify.getQueryString());
	}
	
	/**
	 * This method marks the status of the derivation as "InProgress", 
	 * also records the timestamp at the point the derivation status is marked as InProgress:
	 * <derivation> <retrievedInputsAt> timestamp.
	 * @param storeClient
	 * @param derivation
	 */
	void updateStatusBeforeSetupJob(String derivation) {
		deleteStatus(derivation);
		ModifyQuery modify = Queries.MODIFY();
		
		String statusIRI = getNameSpace(derivation) + "status_" + UUID.randomUUID().toString();
		
		TriplePattern insert_tp = iri(derivation).has(hasStatus, iri(statusIRI));
		TriplePattern insert_tp_rdf_type = iri(statusIRI).isA(InProgress);
		
		modify.prefix(p_derived).insert(insert_tp);
		modify.prefix(p_derived).insert(insert_tp_rdf_type);
		
		// record timestamp at the point the derivation status is marked as InProgress
		// <derivation> <retrievedInputsAt> timestamp.
		long retrievedInputsAtTimestamp = Instant.now().getEpochSecond();
		TriplePattern insert_tp_retrieved_inputs_at = iri(derivation).has(retrievedInputsAt, retrievedInputsAtTimestamp);
		modify.prefix(p_derived).insert(insert_tp_retrieved_inputs_at);
		
		storeClient.executeUpdate(modify.getQueryString());
	}
	
	/**
	 * This method updates the status and job completion.
	 * @param derivation
	 * @param newDerivedIRI
	 */
	void updateStatusAtJobCompletion(String derivation, List<String> newDerivedIRI) {
		deleteStatus(derivation);
		ModifyQuery modify = Queries.MODIFY();
		
		String statusIRI = getNameSpace(derivation) + "status_" + UUID.randomUUID().toString();
		
		TriplePattern insert_tp = iri(derivation).has(hasStatus, iri(statusIRI));
		TriplePattern insert_tp_rdf_type = iri(statusIRI).isA(Finished);
		
		modify.prefix(p_derived).insert(insert_tp);
		modify.prefix(p_derived).insert(insert_tp_rdf_type);
		
		for (String newIRI : newDerivedIRI) {
			modify.insert(iri(statusIRI).has(hasNewDerivedIRI, iri(newIRI)));
		}
		
		storeClient.executeUpdate(modify.getQueryString());
	}
	
	/**
	 * This method retrieves the rdf:type of the status of the given derivation instance.
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
		
		query.prefix(p_derived).where(queryPattern,queryPattern2).select(status,statusType);
		
		JSONArray queryResult = storeClient.executeQuery(query.getQueryString());
		
		if (queryResult.isEmpty()) {
			return StatusType.NOSTATUS;
		} else {
			return statusToType.get(queryResult.getJSONObject(0).getString(statusTypeQueryKey));
		}
	}
	
	/**
	 * This method checks if the derivation has any status assigned to it.
	 * @param storeClient
	 * @param derivation
	 * @return
	 */
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
	 * This method retrieves the new derived IRI of a derivation after the job completed.
	 * @param storeClient
	 * @param derivation
	 * @return
	 */
	List<String> getNewDerivedIRI(String derivation) {
		if (getStatusType(derivation) == StatusType.FINISHED) {
			String derivedQueryKey = "newDerivedIRI";
			
			SelectQuery query = Queries.SELECT();
			
			Variable newDerivedIRI = SparqlBuilder.var(derivedQueryKey);
			
			GraphPattern derivedPattern = iri(derivation).has(PropertyPaths.path(hasStatus,hasNewDerivedIRI), newDerivedIRI);
			
			query.prefix(p_derived).where(derivedPattern).select(newDerivedIRI);
			JSONArray queryResult = storeClient.executeQuery(query.getQueryString());
			
			List<String> newDerived = new ArrayList<>();
			
			for (int i = 0; i < queryResult.length(); i++) {
				newDerived.add(queryResult.getJSONObject(i).getString(derivedQueryKey));
			}
			
			return newDerived;			
		} else {
			throw new JPSRuntimeException("Unable to retrieve the new derived IRI as derivation <" + derivation + "> is not finished.");
		}
	}
	
	/**
	 * returns true of given instance exists
	 * @param storeClient
	 * @param instance
	 * @return
	 */
	@Deprecated
	private boolean checkInstanceExists(String instance) {
		SelectQuery query = Queries.SELECT();
		
		// includes both cases where the instance is a subject and object
		// {<instance> ?x0 ?x1} {?x2 ?x3 <instance>}
		GraphPattern queryPattern = GraphPatterns.and(iri(instance).has(query.var(),query.var()).optional(),
				query.var().has(query.var(),iri(instance)).optional());
		
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
		modify.insert(iri(entity).has(hasTime,time_instant_iri));
		modify.insert(time_instant_iri.isA(InstantClass).andHas(inTimePosition, time_unix_iri));
		modify.insert(time_unix_iri.isA(TimePosition).andHas(numericPosition, timestamp).andHas(hasTRS, iri("http://dbpedia.org/resource/Unix_time")));
				
		storeClient.setQuery(modify.prefix(p_time).getQueryString());
		storeClient.executeUpdate();
	}
	
	/**
	 * same method as above, but update in bulk
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
			modify.insert(iri(entity).has(hasTime,time_instant_iri));
			modify.insert(time_instant_iri.isA(InstantClass).andHas(inTimePosition, time_unix_iri));
			modify.insert(time_unix_iri.isA(TimePosition).andHas(numericPosition, timestamp).andHas(hasTRS, iri("http://dbpedia.org/resource/Unix_time")));
		}

		storeClient.executeUpdate(modify.prefix(p_time).getQueryString());
	}
	
	/**
	 * returns the url of the agent used to calculate the given derived quantity
	 * @param kbClient
	 * @param derivedQuantity
	 * @return
	 */
	String getAgentUrl(String derivedQuantity) {
		SelectQuery query = Queries.SELECT();
		
		String queryKey = "url";
		Variable url = SparqlBuilder.var(queryKey);
		
		Iri derivedQuantityIRI = iri(derivedQuantity);
		
		GraphPattern queryPattern = derivedQuantityIRI.has(PropertyPaths.path(isDerivedUsing,hasOperation,hasHttpUrl), url);
		
		query.select(url).where(queryPattern).prefix(p_agent,p_derived);
		
		storeClient.setQuery(query.getQueryString());
		
		String queryResult = storeClient.executeQuery().getJSONObject(0).getString(queryKey);
		
		return queryResult;
	}
	
	/** 
	 * query the list of inputs for the given derived quantity
	 * TODO SPARQL query string duplication with method getDerivations()
	 * TODO To be break down into smaller chunks
	 * @param kbClient
	 * @param derivedQuantity
	 * @return
	 */
	List<String> getInputs(String derivedQuantity) {
		String queryKey = "input";
		Variable input = SparqlBuilder.var(queryKey);
		GraphPattern queryPattern = iri(derivedQuantity).has(isDerivedFrom,input);
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
	 * This method retrieves the agent inputs given the IRI of OntoDerivation:Derivation instance, 
	 * and maps those inputs against the I/O signature declared in the OntoAgent instance of the agent.
	 * The inputs are finally structured as a JSONObject to be feed into the agent for execution.
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
		GraphPattern agentTypePattern = iri(agentIRI).has(PropertyPaths.path(hasOperation,hasInput,hasMandatoryPart,hasType),type);
		GraphPattern derivationInputPattern = iri(derivedQuantity).has(isDerivedFrom, input);
		GraphPattern mappingPattern = input.has(PropertyPaths.path(PropertyPaths.oneOrMore(RdfPredicate.a)), type);
		SelectQuery query = Queries.SELECT();
		
		query.prefix(p_derived,p_agent).where(agentTypePattern,derivationInputPattern,mappingPattern).select(input,type);
		storeClient.setQuery(query.getQueryString());
		JSONArray queryResult = storeClient.executeQuery();
		
		// construct the JSONObject for agent input
		JSONObject agentInputs = new JSONObject();
		for (int i = 0; i < queryResult.length(); i++) {
			if (agentInputs.has(queryResult.getJSONObject(i).getString(typeKey))) {
				if (agentInputs.get(queryResult.getJSONObject(i).getString(typeKey)) instanceof JSONArray) {
					agentInputs.getJSONArray(queryResult.getJSONObject(i).getString(typeKey)).put(queryResult.getJSONObject(i).getString(inputKey));
				} else {
					agentInputs.put(queryResult.getJSONObject(i).getString(typeKey), new JSONArray().put(agentInputs.get(queryResult.getJSONObject(i).getString(typeKey))));
					agentInputs.getJSONArray(queryResult.getJSONObject(i).getString(typeKey)).put(queryResult.getJSONObject(i).getString(inputKey));
				}
			} else {
				agentInputs.put(queryResult.getJSONObject(i).getString(typeKey), queryResult.getJSONObject(i).getString(inputKey));
			}
		}
		
		return agentInputs;
	}
	
	/**
	 * This method retrieves a list of derivations that <isDerivedUsing> a given <agentIRI>. 
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
	 * This method retrieves a mapped list of derivations that <isDerivedUsing> a given <agentIRI> and their statusType.
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
		GraphPatternNotTriples optionalPattern = GraphPatterns.optional(GraphPatterns.and(derivation.has(hasStatus, status), status.isA(statusType).filter(Expressions.and(entityFilters))));
		
		SelectQuery query = Queries.SELECT();
		
		query.prefix(p_derived, p_agent).select(derivation,statusType).where(queryPattern,optionalPattern);
		storeClient.setQuery(query.getQueryString());
		JSONArray queryResult = storeClient.executeQuery();
		
		Map<String, StatusType> derivationsAndStatusType = new HashMap<>();
		for (int i = 0; i < queryResult.length(); i++) {
			if (queryResult.getJSONObject(i).has(statusTypeQueryKey)) {
				derivationsAndStatusType.put(queryResult.getJSONObject(i).getString(queryKey), statusToType.get(queryResult.getJSONObject(i).getString(statusTypeQueryKey)));
			} else {
				derivationsAndStatusType.put(queryResult.getJSONObject(i).getString(queryKey), StatusType.NOSTATUS);				
			}
		}
		
		return derivationsAndStatusType;
	}
	
	/**
	 * This method retrieves a list of upstream derivations that directly linked with the given derivation in the chain and need an update.
	 * @param derivation
	 * @return
	 */
	List<String> getUpstreamDerivationsNeedUpdate(String derivation) {
		String upsDevQueryKey = "upstreamDerivation";
		String upsDevTimeQueryKey = "upstreamDerivationTimestamp";
		String statusQueryKey = "status";
		String statusTypeQueryKey = "statusType";
		String pureInputTimeQueryKey = "pureInputTimestamp";
		String inputsBelongingToDevTimeQueryKey = "inputsBelongingToDerivationTimestamp";
		
		SelectQuery query = Queries.SELECT().distinct();
		
		Variable upstreamDerivation = SparqlBuilder.var(upsDevQueryKey);
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
		
		// check if the upstreamDerivation (outdated timestamp compared to pure input || outdated timestamp compared to its own upstream derivations || has status)
		Expression<?> upstreamDerivationFilter = Expressions.or(Expressions.lt(upstreamDerivationTimestamp, pureInputTimestamp), // ?upstreamDerivationTimestamp < ?pureInputTimestamp
				Expressions.lt(upstreamDerivationTimestamp, inputsBelongingToDerivationTimestamp), // ?upstreamDerivationTimestamp < ?inputsBelongingToDerivationTimestamp
				Expressions.equals(statusType, PendingUpdate), // ?statusType IN (derived:PendingUpdate, derived:Requested, derived:InProgress, derived:Finished)
				Expressions.equals(statusType, Requested),
				Expressions.equals(statusType, InProgress),
				Expressions.equals(statusType, Finished));
		
		GraphPattern upstreamDerivationPattern = iri(derivation).has(PropertyPaths.path(isDerivedFrom,belongsTo), upstreamDerivation);
		GraphPattern upDevTimePattern = upstreamDerivation.has(PropertyPaths.path(hasTime,inTimePosition,numericPosition), upstreamDerivationTimestamp);
		GraphPattern upDevStatusTypePattern = GraphPatterns.optional(GraphPatterns.and(upstreamDerivation.has(hasStatus, status), status.isA(statusType).filter(Expressions.and(entityFilters))));
		GraphPattern upDevPureInputTimePattern = upstreamDerivation.has(PropertyPaths.path(isDerivedFrom,hasTime,inTimePosition,numericPosition), pureInputTimestamp).optional();
		GraphPattern inputsBelongsToDevTimePattern = upstreamDerivation.has(PropertyPaths.path(isDerivedFrom,belongsTo,hasTime,inTimePosition,numericPosition), inputsBelongingToDerivationTimestamp).optional();
		
		// Complete query string:
		// PREFIX derived: <https://github.com/cambridge-cares/TheWorldAvatar/blob/develop/JPS_Ontology/ontology/ontoderivation/OntoDerivation.owl#>
		// PREFIX time: <http://www.w3.org/2006/time#>
		// SELECT DISTINCT ?upstreamDerivation
		// WHERE {
		//   <derivation> derived:isDerivedFrom/derived:belongsTo ?upstreamDerivation.
		//   ?upstreamDerivation time:hasTime/time:inTimePosition/time:numericPosition ?upstreamDerivationTimestamp.
		//   OPTIONAL{?upstreamDerivation derived:hasStatus ?status .
		//			  {?status a ?statusType .
		//			   FILTER((?statusType != <http://www.w3.org/2002/07/owl#Thing> && ?statusType != <http://www.w3.org/2002/07/owl#NamedIndividual>))
		//			  }
		//		   }
		//   OPTIONAL{?upstreamDerivation derived:isDerivedFrom/time:hasTime/time:inTimePosition/time:numericPosition ?pureInputTimestamp}
		//   OPTIONAL{?upstreamDerivation derived:isDerivedFrom/derived:belongsTo/time:hasTime/time:inTimePosition/time:numericPosition ?inputsBelongingToDerivationTimestamp}
		//   FILTER((?upstreamDerivationTimestamp < ?pureInputTimestamp || ?upstreamDerivationTimestamp < ?inputsBelongingToDerivationTimestamp || ?statusType = derived:PendingUpdate || ?statusType = derived:Requested || ?statusType = derived:InProgress || ?statusType = derived:Finished))
		// }
		
		// it should be noted that the final FILTER has a simplified version but was not implemented as IN operator was not found in SparqlBuilder (to the best of the author's knowledge):
		// FILTER(?upstreamDerivationTimestamp < ?pureInputTimestamp || ?upstreamDerivationTimestamp < ?inputsBelongingToDerivationTimestamp || ?statusType IN (derived:PendingUpdate, derived:Requested, derived:InProgress, derived:Finished))
		
		query.prefix(p_derived,p_time).select(upstreamDerivation)
		.where(GraphPatterns.and(upstreamDerivationPattern,upDevTimePattern,upDevStatusTypePattern,upDevPureInputTimePattern,inputsBelongsToDevTimePattern).filter(upstreamDerivationFilter));
		
		JSONArray queryResult = storeClient.executeQuery(query.getQueryString());
		
		List<String> listOfUpstreamDerivation = new ArrayList<>();
		
		for (int i = 0; i < queryResult.length(); i++) {
			String derivedIRI = queryResult.getJSONObject(i).getString(upsDevQueryKey);
			listOfUpstreamDerivation.add(derivedIRI);
		}
		
		return listOfUpstreamDerivation;
	}
	
	/**
	 * This is used at the stage to detect circular dependency
	 * if the input is part of a derived quantity, this will add the derived instance
	 * if the input is not a derived instance, it will add the input itself
	 * TODO SPARQL query string duplication with method getDerivations()
	 * TODO To be break down into smaller chunks
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
		GraphPattern inputPattern = GraphPatterns.and(iri(derived).has(isDerivedFrom,input));
		// some inputs may be a part of a derived instance
		// this is also added to the list to ensure that there are no circular dependencies via a different entity within the same derived instance
		GraphPattern derivedPattern = input.has(belongsTo, derivedOfInput).optional();
		
		query.prefix(p_derived).where(inputPattern,derivedPattern).select(input,derivedOfInput);
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
	 * returns entities belonging to this derived instance, ?x <derived:belongsTo> <derivedIRI>
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
	 * @param kbClient
	 * @param derivedIRI
	 * @return
	 */
	List<Entity> getDerivedEntitiesAndDownstreamDerivation(String derivation) {
		SelectQuery query = Queries.SELECT();
		String entityQueryKey = "entity";
		String rdfTypeQueryKey = "class";
		String downstreamDevQueryKey = "downstreamDerivation";
		String downsDevRdfTypeQueryKey = "downsDevClass";
		
		Variable entity = SparqlBuilder.var(entityQueryKey);
		Variable rdfType = SparqlBuilder.var(rdfTypeQueryKey);
		Variable downstreamDerivation = SparqlBuilder.var(downstreamDevQueryKey);
		Variable downsDevRdfType= SparqlBuilder.var(downsDevRdfTypeQueryKey);
		
		// ignore certain rdf:type
		Expression<?>[] entityFilters = new Expression<?>[classesToIgnore.size()];
		for (int j = 0; j < classesToIgnore.size(); j++) {
			entityFilters[j] = Expressions.notEquals(rdfType, classesToIgnore.get(j));
		}
		Expression<?>[] downsDevFilters = new Expression<?>[classesToIgnore.size()];
		for (int j = 0; j < classesToIgnore.size(); j++) {
			downsDevFilters[j] = Expressions.notEquals(downsDevRdfType, classesToIgnore.get(j));
		}
		
		GraphPattern queryPattern = entity.has(belongsTo, iri(derivation)).andIsA(rdfType).filter(Expressions.and(entityFilters));
		GraphPattern downstreamDevPattern = downstreamDerivation.has(isDerivedFrom, entity).andIsA(downsDevRdfType).filter(Expressions.and(downsDevFilters));
		query.prefix(p_derived).select(entity,rdfType,downstreamDerivation,downsDevRdfType).where(queryPattern,downstreamDevPattern);
		
		JSONArray queryResult = storeClient.executeQuery(query.getQueryString());
		
		List<Entity> entities = new ArrayList<>();
		for (int i = 0; i < queryResult.length(); i++) {
			Entity e = new Entity(queryResult.getJSONObject(i).getString(entityQueryKey));
			e.setRdfType(queryResult.getJSONObject(i).getString(rdfTypeQueryKey));
			e.setAsInput(new Derivation(queryResult.getJSONObject(i).getString(downstreamDevQueryKey), queryResult.getJSONObject(i).getString(downsDevRdfTypeQueryKey)));
			entities.add(e);
		}
		
		return entities;
	}
	
	/**
	 * returns the derived instance, where the given entity is an input to it
	 * <derived> <derived:isDerivedFrom> <entity>
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
			
			GraphPattern queryPattern = GraphPatterns.and(derived.has(isDerivedFrom,iri(entity)), iri(entity).isA(entityType))
					.filter(Expressions.and(filters)).optional();
			
			query.select(derived, entityType).where(queryPattern).prefix(p_derived);
			JSONArray queryResult = storeClient.executeQuery(query.getQueryString());
			
			// do not attempt to get value if this instance is not an input to any other derived quantities
			if(!queryResult.getJSONObject(0).isEmpty()) {
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
			
			sub.select(subject,pred1,pred2,object).where(queryPattern);
			
			ModifyQuery modify = Queries.MODIFY();
			modify.delete(delete_tp).where(sub);
			
			storeClient.executeUpdate(modify.getQueryString());
		}
	}
	
	/**
	 * This methods deletes the status of a derivation instance. 
	 * @param kbClient
	 * @param instance
	 */
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
		modify.delete(tp1,tp2,tp3).where(tp1,tp2,gp).prefix(p_derived);
		
		storeClient.executeUpdate(modify.getQueryString());
	}
	
	/**
	 * This methods retrieves the timestamp of a derivation instance. 
	 * TODO SPARQL query string duplication with method getDerivations()
	 * TODO To be break down into smaller chunks
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
		Iri[] predicates = {hasTime,inTimePosition,numericPosition};
		GraphPattern queryPattern = instanceIRI.has(PropertyPaths.path(predicates),time).optional();

		// instances that do not have time stamp directly attached, but belongs to a derived instance
		Iri[] predicates2 = {belongsTo, hasTime, inTimePosition, numericPosition};
		GraphPattern queryPattern2 = instanceIRI.has(PropertyPaths.path(predicates2),time).optional();
		
		query.prefix(p_time,p_derived).where(queryPattern, queryPattern2).select(time);
		
		JSONArray queryResult = storeClient.executeQuery(query.getQueryString());
		
		if (queryResult.length() > 1) {
			throw new JPSRuntimeException("DerivedQuantitySparql: More than 1 time instance associated with <" +instance + ">");
		}
		
		try {
			long timestamp = queryResult.getJSONObject(0).getLong(queryKey);
			return timestamp;
		}
		catch (JSONException e) {
			throw new JPSRuntimeException("No timestamp for <" + instance + ">. This is probably an input and you should consider "
					+ "adding a timestamp using DerivationClient.addTimeInstance or create a derived instance using this instance");
		}
	}
	
	/**
	 * update time stamp for a given IRI of an instance
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
		
		GraphPattern queryPattern = GraphPatterns.and(iri(instance).has(hasTime,timeIRI), 
				timeIRI.has(inTimePosition,unixtimeIRI),
				unixtimeIRI.has(numericPosition,oldvalue));
		
		// triples to add and delete
		TriplePattern delete_tp = unixtimeIRI.has(numericPosition,oldvalue);
		TriplePattern insert_tp = unixtimeIRI.has(numericPosition, timestamp);
		
		sub.select(unixtimeIRI,oldvalue).where(queryPattern);
		
		ModifyQuery modify = Queries.MODIFY();
		modify.prefix(p_time).delete(delete_tp).insert(insert_tp).where(sub);
		
		storeClient.setQuery(modify.getQueryString());
		storeClient.executeUpdate();
		
		return timestamp;
	}
	
	/** 
	 * returns rdf:type of the given instance, ignoring owl:NamedIndividual and owl:Thing
	 * when an instance is part of an input to another derived instance, the type is used to reconnect the appropriate instance
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
				throw new JPSRuntimeException("DerivedQuantitySparql.getInstanceClass: more than 1 rdf:type for " + instances.get(i));
			} else if (queryResult.length() == 1) {
				classOfInstances.add(i, queryResult.getJSONObject(0).getString(queryKey));
			} else {
				classOfInstances.add(i, "");
			}
		}
		return classOfInstances;
	}
	
	/**
	 * used when an agent produces a list of new entities
	 * @param instances
	 * @return
	 */
	List<Entity> initialiseNewEntities(List<String> instances) {
		// query rdf types of these new instances
		SelectQuery query = Queries.SELECT();
		Variable type = query.var();
		Variable instance = query.var();
		
		List<Iri> instances_iri = instances.stream().map(i -> iri(i)).collect(Collectors.toList());
		
		// ignore certain rdf:type
		Expression<?>[] filters = new Expression<?>[classesToIgnore.size()];
		for (int j = 0; j < classesToIgnore.size(); j++) {
			filters[j] = Expressions.notEquals(type, classesToIgnore.get(j));
		}
		
		ValuesPattern valuePattern = new ValuesPattern(instance, instances_iri);
		GraphPattern typePattern = instance.isA(type).filter(Expressions.and(filters));
		
		query.select(type,instance).where(valuePattern,typePattern);
		
		JSONArray queryResult = storeClient.executeQuery(query.getQueryString());
		
		List<Entity> newEntities = new ArrayList<>();
		for (int i = 0; i < queryResult.length(); i++) {
			String iri = queryResult.getJSONObject(i).getString(instance.getQueryString().substring(1));
			
			if (newEntities.stream().anyMatch(e -> e.getIri().equals(iri))) {
				throw new JPSRuntimeException("DerivedQuantitySparql.getInstanceClass: more than 1 rdf:type for " + iri);
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
	 * this is used to reconnect a newly created instance to an existing derived instance
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
	 * this is used to reconnect a newly created instance to an existing derived instance
	 * @param kbClient
	 * @param input
	 * @param derived
	 */
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
	 * Returns true if it is a asynchronous derived quantity.
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
	 * @param storeClient
	 * @param instance
	 * @param newEntities
	 */
	void addNewEntitiesToDerived(String instance, List<String> newEntities) {
		ModifyQuery modify = Queries.MODIFY();
		
		for (String newEntity : newEntities) {
			modify.insert(iri(newEntity).has(belongsTo, iri(instance)));
		}
		
		storeClient.executeUpdate(modify.prefix(p_derived).getQueryString());
	}
	
	/**
	 * this is used to obtain all the derivations in the kg
	 * @return
	 */
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
				.andHas(PropertyPaths.path(isDerivedUsing,hasOperation,hasHttpUrl), agentURL)
				.andHas(PropertyPaths.path(hasTime, inTimePosition, numericPosition), derivationTimestamp)
				.andIsA(derivationType);
		GraphPattern entityPattern = entity.has(belongsTo, derivation);
		GraphPattern inputTimestampPattern = input.has(
				PropertyPaths.path(hasTime, inTimePosition, numericPosition), inputTimestamp).optional();
		GraphPattern inputTypePattern = input.isA(inputType).optional().filter(Expressions.and(inputTypeFilters));
		GraphPattern entityTypePattern = entity.isA(entityType).optional().filter(Expressions.and(entityTypeFilters));
		
		query.select(derivation,input,entity,agentURL,derivationTimestamp,inputTimestamp,derivationType,inputType,entityType)
		.where(derivationPattern,entityPattern,inputTimestampPattern,inputTypePattern,entityTypePattern)
		.prefix(p_derived,p_time,p_agent);
		
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
			long derivedTimestamp = queryResult.getJSONObject(i).getLong(derivationTimestamp.getQueryString().substring(1));
			
			Derivation derived;
			if (derivationsMap.containsKey(derivationIRI)) {
				derived = derivationsMap.get(derivationIRI);
			} else {
				derived = new Derivation(derivationIRI,derivedType);
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
				input_entity.setRdfType(queryResult.getJSONObject(i).getString(inputType.getQueryString().substring(1)));
			}
			
			// if it's a pure input it will have a timestamp
			if (queryResult.getJSONObject(i).has(inputTimestamp.getQueryString().substring(1))) {
				long input_timestamp = queryResult.getJSONObject(i).getLong(inputTimestamp.getQueryString().substring(1));
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
				entity_entity.setRdfType(queryResult.getJSONObject(i).getString(entityType.getQueryString().substring(1)));
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
		
		query.prefix(p_time,p_derived).where(queryPattern);
		
		JSONArray queryResult = storeClient.executeQuery(query.getQueryString());
		
		if (queryResult.length() > 0) {
			return false;
		} else {
			return true;
		}
	}
	
	/**
	 * delete all entities of a given derivation
	 * @param derivation
	 */
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
		modify.delete(delete_tp1,delete_tp2).where(entity.has(belongsTo, iri(derivation)), delete_tp1, subject.has(predicate2, entity).optional()).prefix(p_derived);
		
		storeClient.executeUpdate(modify.getQueryString());
	}
	
	/**
	 * only works with the standard Derivation type and DerivationWithTimeSeries
	 * does not remove timestamps of inputs (technically outside derivation)
	 * needs modification to work with asynchronous derivations
	 * @param storeClient
	 */
	void dropAllDerivations() {
		List<Iri> derivationTypes = Arrays.asList(Derivation, DerivationWithTimeSeries);
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
		
		TriplePattern timeTpAll1 = time.isA(InstantClass).andHas(inTimePosition,time_unix_iri);
		TriplePattern timeTpAll2 = time_unix_iri.isA(TimePosition).andHas(numericPosition, timestamp).andHas(hasTRS, trs);
		
		// agent
		TriplePattern agentTp1 = derivation.has(isDerivedUsing,agent);
		TriplePattern agentTp2 = agent.isA(Service).andHas(hasOperation, operation);
		TriplePattern agentTp3 = operation.isA(Operation).andHas(hasHttpUrl, url);
		
		GraphPattern queryPattern = GraphPatterns.and(belongsToTp,isDerivedFromTp,
				timestampTp1, timeTpAll1,timeTpAll2,
				agentTp1,agentTp2,agentTp3, new ValuesPattern(derivationType,derivationTypes));
		
		modify.delete(belongsToTp,isDerivedFromTp,
				timestampTp1, timeTpAll1,timeTpAll2,
				agentTp1,agentTp2,agentTp3,derivationTypeTp).where(queryPattern)
				.prefix(p_time,p_derived,p_agent);
		
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
		TriplePattern tp2 = time.isA(InstantClass).andHas(inTimePosition,time_unix_iri);
		TriplePattern tp3 = time_unix_iri.isA(TimePosition).andHas(numericPosition, timestamp).andHas(hasTRS, trs);
		
		modify.delete(tp1,tp2,tp3).where(tp1,tp2,tp3).prefix(p_time);
		
		storeClient.executeUpdate(modify.getQueryString());
	}
	
	/**
	 * Updates timestamps of the given instances in two stages
	 * Query 1: get the corresponding time IRI
	 * query 2: delete/insert appropriate triples
	 * @param derivationTimestamp_map
	 */
	void updateTimestamps(Map<String,Long> instanceTimestamp_map) {
		List<String> instances = new ArrayList<>(instanceTimestamp_map.keySet());
		
		// query 1: get corresponding time IRI for each instance if it exists
		SelectQuery query = Queries.SELECT();
		Variable inst = query.var();
		ValuesPattern instValuesPattern = new ValuesPattern(inst, instances.stream().map(i -> iri(i)).collect(Collectors.toList()));
				
		Variable time_unix = query.var();
		
		GraphPattern gp1 = inst.has(PropertyPaths.path(hasTime, inTimePosition), time_unix);
		
		query.select(inst,time_unix).where(gp1,instValuesPattern).prefix(p_time);
		
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
			String instance  = instances.get(i);
			Iri timeIRI = iri(instance_timeiri_map.get(instance));
			insert_tp[i] = timeIRI.has(numericPosition, instanceTimestamp_map.get(instance));
			timeIRIList.add(timeIRI);
		}
		
		TriplePattern delete_tp = time_unix.has(numericPosition, timestamp);
		ValuesPattern timeValuesPattern = new ValuesPattern(time_unix, timeIRIList);
		
		modify.delete(delete_tp).where(timeValuesPattern,delete_tp).insert(insert_tp).prefix(p_time);
		
		storeClient.executeUpdate(modify.getQueryString());
	}
	
	Map<String,String> getDerivationsOf(List<String> entities) {
		SelectQuery query = Queries.SELECT();
		Variable entity = query.var();
		Variable derivation = query.var();
		
		GraphPattern queryPattern = entity.has(belongsTo, derivation);
		ValuesPattern valuesPattern = new ValuesPattern(entity, 
				entities.stream().map(e -> iri(e)).collect(Collectors.toList()));
		
		query.select(entity, derivation).where(queryPattern, valuesPattern).prefix(p_derived);
		
		JSONArray queryResult = storeClient.executeQuery(query.getQueryString());
		
		Map<String,String> entityDerivationMap = new HashMap<>();
		for (int i = 0; i < queryResult.length(); i++) {
			entityDerivationMap.put(
					queryResult.getJSONObject(i).getString(entity.getQueryString().substring(1)), 
					queryResult.getJSONObject(i).getString(derivation.getQueryString().substring(1)));
		}
		
		return entityDerivationMap;
	}
	
	/**
	 * This method retrieves the input read timestamp associated with the asynchronous derivation, also deletes the record after value retrieved.
	 * @param derivation
	 * @return
	 */
	Map<String, Long> retrieveInputReadTimestamp(String derivation) {
		Map<String, Long> derivationTime_map = new HashMap<>();
		
		String queryKey = "timestamp";
		SelectQuery query = Queries.SELECT();
		Variable time = SparqlBuilder.var(queryKey);
		
		GraphPattern queryPattern = iri(derivation).has(retrievedInputsAt, time);
		
		query.prefix(p_derived).select(time).where(queryPattern);
		JSONArray queryResult = storeClient.executeQuery(query.getQueryString());
		
		if (queryResult.length() > 1) {
			throw new JPSRuntimeException("DerivedQuantitySparql: More than 1 time instance recorded for reading derivation inputs of <" + derivation + ">");
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
		}
		catch (JSONException e) {
			throw new JPSRuntimeException("No timestamp recorded for reading derivation inputs of <" + derivation + ">. The derivation is probably not setup correctly.");
		}
	}
	
	/**
	 * This method chunks the given iri and returns its namespace. 
	 * @param iri
	 * @return
	 */
	private String getNameSpace(String iri) {
		iri = trimIRI(iri);
		if (iri.contains("#")) {
			iri = iri.substring(0, iri.lastIndexOf("#")+1);
		} else if (iri.contains("/")) {
			iri = iri.substring(0, iri.lastIndexOf("/")+1);
		}
		return iri;
	}
	
	/**
	 * This method trims the given iri by removing the "<" at the start and the ">" at the end.
	 * @param iri
	 * @return
	 */
	private String trimIRI(String iri) {
		if (iri.startsWith("<")) {
			iri = iri.substring(1);
		}
		if (iri.endsWith(">")) {
			iri = iri.substring(0, iri.length()-1);
		}
		return iri;
	}
}
