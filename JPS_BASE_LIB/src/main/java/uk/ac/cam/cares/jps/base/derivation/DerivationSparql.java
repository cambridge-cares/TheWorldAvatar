package uk.ac.cam.cares.jps.base.derivation;

import static org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf.iri;

import java.time.Instant;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.UUID;

import org.eclipse.rdf4j.sparqlbuilder.constraint.Expression;
import org.eclipse.rdf4j.sparqlbuilder.constraint.Expressions;
import org.eclipse.rdf4j.sparqlbuilder.core.Prefix;
import org.eclipse.rdf4j.sparqlbuilder.core.SparqlBuilder;
import org.eclipse.rdf4j.sparqlbuilder.core.Variable;
import org.eclipse.rdf4j.sparqlbuilder.core.query.ModifyQuery;
import org.eclipse.rdf4j.sparqlbuilder.core.query.Queries;
import org.eclipse.rdf4j.sparqlbuilder.core.query.SelectQuery;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.GraphPattern;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.GraphPatterns;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.SubSelect;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.TriplePattern;
import org.eclipse.rdf4j.sparqlbuilder.rdf.Iri;
import org.eclipse.rdf4j.model.vocabulary.OWL;
import org.json.JSONArray;
import org.json.JSONException;
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
	public static String derivednamespace = "https://github.com/cambridge-cares/TheWorldAvatar/blob/develop/JPS_Ontology/ontology/ontoderivation/OntoDerivation.owl#";
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
    private static Iri Status = p_derived.iri("Status");
    private static Iri Requested = p_derived.iri("Requested");
    private static Iri InProgress = p_derived.iri("InProgress");
    private static Iri Finished = p_derived.iri("Finished");
    private static Iri InstantClass = p_time.iri("Instant");
	
	// object properties
	private static Iri hasHttpUrl = p_agent.iri("hasHttpUrl");
	private static Iri hasOperation = p_agent.iri("hasOperation");
	private static Iri isDerivedFrom = p_derived.iri("isDerivedFrom");
	private static Iri isDerivedUsing = p_derived.iri("isDerivedUsing");
	private static Iri belongsTo = p_derived.iri("belongsTo");
	private static Iri hasStatus = p_derived.iri("hasStatus");
	private static Iri hasTime = p_time.iri("hasTime");
	private static Iri numericPosition = p_time.iri("numericPosition");
	private static Iri hasTRS = p_time.iri("hasTRS");
	private static Iri inTimePosition = p_time.iri("inTimePosition");
	
	// data properties
	private static Iri hasNewDerivedIRI = p_derived.iri("hasNewDerivedIRI");
	
	// the derived quantity client relies on matching rdf:type to figure out which old instances to delete
	// if your instances have more than 1 rdf:type, you must add them to this list so that the client can figure out which to use
	private static List<Iri> classesToIgnore = Arrays.asList(iri(OWL.THING),iri(OWL.NAMEDINDIVIDUAL));
	
	private static final Logger LOGGER = LogManager.getLogger(DerivationSparql.class);
	
	/**
	 * creates a new instance of derived quantity, grouping the given entities under this instance
	 * whenever this derived quantity gets updated, the provided entities will get deleted by the client
	 * @param kbClient
	 * @param entities
	 * @param agentIRI
	 * @param agentURL
	 * @param inputs
	 */
	static String createDerivation(StoreClientInterface kbClient, 
			List<String> entities, String agentIRI, String agentURL, List<String> inputs) {
	    ModifyQuery modify = Queries.MODIFY();
		
		// create a unique IRI for this new derived quantity
		String derivedQuantity = derivednamespace + "derived" + UUID.randomUUID().toString();
		while (checkInstanceExists(kbClient, derivedQuantity)) {
			derivedQuantity = derivednamespace + "derived" + UUID.randomUUID().toString();
		}
		
		Iri derived_iri = iri(derivedQuantity);
		
		modify.insert(derived_iri.isA(Derivation));
		
		for (String entity : entities) {
			// ensure that given entity is not part of another derived quantity
			if (!hasBelongsTo(kbClient, entity)) {
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
		String operation_iri = derivednamespace + UUID.randomUUID().toString();
		// add agent url
		modify.insert(iri(agentIRI).isA(Service).andHas(hasOperation, iri(operation_iri)));
		modify.insert(iri(operation_iri).isA(Operation).andHas(hasHttpUrl, iri(agentURL)));
	    
	    // link to each input
	    for (String input : inputs) {
	    	modify.insert(derived_iri.has(isDerivedFrom, iri(input)));
	    }
	    
	    modify.prefix(p_time,p_derived,p_agent);
	    
	    kbClient.setQuery(modify.prefix(p_time,p_derived,p_agent).getQueryString());
	    kbClient.executeUpdate();
	    
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
	static String createDerivation(StoreClientInterface kbClient, List<String> entities, String agentIRI, List<String> inputs) {
		ModifyQuery modify = Queries.MODIFY();

		// create a unique IRI for this new derived quantity
		String derivedQuantity = derivednamespace + "derived" + UUID.randomUUID().toString();
		while (checkInstanceExists(kbClient, derivedQuantity)) {
			derivedQuantity = derivednamespace + "derived" + UUID.randomUUID().toString();
		}

		Iri derived_iri = iri(derivedQuantity);

		modify.insert(derived_iri.isA(Derivation));

		for (String entity : entities) {
			// ensure that given entity is not part of another derived quantity
			if (!hasBelongsTo(kbClient, entity)) {
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
		
		kbClient.setQuery(modify.prefix(p_time,p_derived,p_agent).getQueryString());
		kbClient.executeUpdate();
		return derivedQuantity;
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
	static String createDerivationWithTimeSeries(StoreClientInterface kbClient, 
			List<String> entities, String agentIRI, String agentURL, List<String> inputs) {
	    ModifyQuery modify = Queries.MODIFY();
		
		// create a unique IRI for this new derived quantity
		String derivedQuantity = derivednamespace + "derived" + UUID.randomUUID().toString();
		while (checkInstanceExists(kbClient, derivedQuantity)) {
			derivedQuantity = derivednamespace + "derived" + UUID.randomUUID().toString();
		}
		
		Iri derived_iri = iri(derivedQuantity);
		
		modify.insert(derived_iri.isA(DerivationWithTimeSeries));
		
		for (String entity : entities) {
			// ensure that given entity is not part of another derived quantity
			if (!hasBelongsTo(kbClient, entity)) {
				modify.insert(iri(entity).has(belongsTo, derived_iri));
			} else {
				String errmsg = "<" + entity + "> is already part of another derivation";
				LOGGER.fatal(errmsg);
				throw new JPSRuntimeException(errmsg);
			}
		}
		
		// link to agent
		modify.insert(derived_iri.has(isDerivedUsing,iri(agentIRI)));
		String operation_iri = derivednamespace + UUID.randomUUID().toString();
		// add agent url
		modify.insert(iri(agentIRI).isA(Service).andHas(hasOperation, iri(operation_iri)));
		modify.insert(iri(operation_iri).isA(Operation).andHas(hasHttpUrl, iri(agentURL)));
	    
	    // link to each input
	    for (String input : inputs) {
	    	modify.insert(derived_iri.has(isDerivedFrom, iri(input)));
	    }
	    
	    modify.prefix(p_time,p_derived,p_agent);
	    
	    kbClient.setQuery(modify.prefix(p_time,p_derived,p_agent).getQueryString());
	    kbClient.executeUpdate();
	    
	    return derivedQuantity;
	}
	
	/**
	 * check that the entity is part of another derived quantity, this is not allowed
	 * query triple - <entity> <belongsTo> ?x
	 * @param storeClient
	 * @param entity
	 * @return
	 */
	private static boolean hasBelongsTo(StoreClientInterface storeClient, String entity) {
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
	 * This method checks if the status of the derivation is marked as "Requested".
	 * @param storeClient
	 * @param derivation
	 * @return
	 */
	static boolean isRequested(StoreClientInterface storeClient, String derivation) {
		String statusQueryKey = "status";
		Variable status = SparqlBuilder.var(statusQueryKey);
		SelectQuery query = Queries.SELECT();
		
		GraphPattern queryPattern = iri(derivation).has(hasStatus, status);
		GraphPattern queryPattern2 = status.isA(Requested);
		query.prefix(p_derived).where(queryPattern, queryPattern2);
		
		JSONArray queryResult = storeClient.executeQuery(query.getQueryString());
		
		if (queryResult.isEmpty()) {
			return false;
		} else {
			return true;
		}
	}

	/**
	 * This method checks if the status of the derivation is marked as "InProgress".
	 * @param storeClient
	 * @param derivation
	 * @return
	 */
	static boolean isInProgress(StoreClientInterface storeClient, String derivation) {
		String statusQueryKey = "status";
		Variable status = SparqlBuilder.var(statusQueryKey);
		SelectQuery query = Queries.SELECT();
		
		GraphPattern queryPattern = iri(derivation).has(hasStatus, status);
		GraphPattern queryPattern2 = status.isA(InProgress);
		query.prefix(p_derived).where(queryPattern, queryPattern2);
		
		JSONArray queryResult = storeClient.executeQuery(query.getQueryString());
		
		if (queryResult.isEmpty()) {
			return false;
		} else {
			return true;
		}
	}
	
	/**
	 * This method checks if the status of the derivation is marked as "Finished".
	 * @param storeClient
	 * @param derivation
	 * @return
	 */
	static boolean isFinished(StoreClientInterface storeClient, String derivation) {
		String statusQueryKey = "status";
		Variable status = SparqlBuilder.var(statusQueryKey);
		SelectQuery query = Queries.SELECT();
		
		GraphPattern queryPattern = iri(derivation).has(hasStatus, status);
		GraphPattern queryPattern2 = status.isA(Finished);
		query.prefix(p_derived).where(queryPattern, queryPattern2);
		
		JSONArray queryResult = storeClient.executeQuery(query.getQueryString());
		
		if (queryResult.isEmpty()) {
			return false;
		} else {
			return true;
		}
	}
	
	/**
	 * This method marks the status of the derivation as "Requested".
	 * @param storeClient
	 * @param derivation
	 */
	static void markAsRequested(StoreClientInterface storeClient, String derivation) {
		ModifyQuery modify = Queries.MODIFY();
		
		String statusIRI = getNameSpace(derivation) + "status_" + UUID.randomUUID().toString();
		while (checkInstanceExists(storeClient, statusIRI)) {
			statusIRI = getNameSpace(derivation) + "status_" + UUID.randomUUID().toString();
		}
		TriplePattern insert_tp = iri(derivation).has(hasStatus, iri(statusIRI));
		TriplePattern insert_tp_rdf_type = iri(statusIRI).isA(Requested);
		
		modify.prefix(p_derived).insert(insert_tp);
		modify.prefix(p_derived).insert(insert_tp_rdf_type);
		
		storeClient.executeUpdate(modify.getQueryString());
	}
	
	/**
	 * This method marks the status of the derivation as "InProgress".
	 * @param storeClient
	 * @param derivation
	 */
	static void markAsInProgress(StoreClientInterface storeClient, String derivation) {
		deleteStatus(storeClient, derivation);
		ModifyQuery modify = Queries.MODIFY();
		
		String statusIRI = getNameSpace(derivation) + "status_" + UUID.randomUUID().toString();
		while (checkInstanceExists(storeClient, statusIRI)) {
			statusIRI = getNameSpace(derivation) + "status_" + UUID.randomUUID().toString();
		}
		TriplePattern insert_tp = iri(derivation).has(hasStatus, iri(statusIRI));
		TriplePattern insert_tp_rdf_type = iri(statusIRI).isA(InProgress);
		
		modify.prefix(p_derived).insert(insert_tp);
		modify.prefix(p_derived).insert(insert_tp_rdf_type);
		
		storeClient.executeUpdate(modify.getQueryString());
	}
	
	/**
	 * This method marks the status of the derivation as "Finished".
	 * @param storeClient
	 * @param derivation
	 * @return
	 */
	static String markAsFinished(StoreClientInterface storeClient, String derivation) {
		deleteStatus(storeClient, derivation);
		ModifyQuery modify = Queries.MODIFY();
		
		String statusIRI = getNameSpace(derivation) + "status_" + UUID.randomUUID().toString();
		while (checkInstanceExists(storeClient, statusIRI)) {
			statusIRI = getNameSpace(derivation) + "status_" + UUID.randomUUID().toString();
		}
		TriplePattern insert_tp = iri(derivation).has(hasStatus, iri(statusIRI));
		TriplePattern insert_tp_rdf_type = iri(statusIRI).isA(Finished);
		
		modify.prefix(p_derived).insert(insert_tp);
		modify.prefix(p_derived).insert(insert_tp_rdf_type);
		
		storeClient.executeUpdate(modify.getQueryString());
		return statusIRI;
	}
	
	/**
	 * This method checks if the derivation has any status assigned to it.
	 * @param storeClient
	 * @param derivation
	 * @return
	 */
	static boolean hasStatus(StoreClientInterface storeClient, String derivation) {
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
	 * This method retrieves the status of a derivation.
	 * @param storeClient
	 * @param derivation
	 * @return
	 */
	static String getStatus(StoreClientInterface storeClient, String derivation) {
		if (hasStatus(storeClient, derivation)) {
			String statusQueryKey = "status";
			Variable status = SparqlBuilder.var(statusQueryKey);
			SelectQuery query = Queries.SELECT();
			
			GraphPattern queryPattern = iri(derivation).has(hasStatus, status);
			query.prefix(p_derived).where(queryPattern).select(status);
			
			JSONArray queryResult = storeClient.executeQuery(query.getQueryString());
			
			return queryResult.getJSONObject(0).getString(statusQueryKey);
		} else {
			throw new JPSRuntimeException("Unable to retrieve the status of derivation <" + derivation + ">.");
		}
	}
	
	/**
	 * This method retrieves the new derived IRI of a derivation after the job completed.
	 * @param storeClient
	 * @param derivation
	 * @return
	 */
	static List<String> getNewDerivedIRI(StoreClientInterface storeClient, String derivation) {
		if (isFinished(storeClient, derivation)) {
			String statusQueryKey = "status";
			String derivedQueryKey = "newDerivedIRI";
			
			SelectQuery query = Queries.SELECT();
			
			Variable status = SparqlBuilder.var(statusQueryKey);
			Variable newDerivedIRI = SparqlBuilder.var(derivedQueryKey);
			
			GraphPattern statusPattern = iri(derivation).has(hasStatus, status);
			GraphPattern derivedPattern = status.has(hasNewDerivedIRI, newDerivedIRI);
			
			query.prefix(p_derived).where(statusPattern, derivedPattern).select(newDerivedIRI);
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
	private static boolean checkInstanceExists(StoreClientInterface storeClient, String instance) {
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
	static void addTimeInstance(StoreClientInterface kbClient, String entity) {
		ModifyQuery modify = Queries.MODIFY();
		
		// add time stamp instance for the given entity
		String time_instant = derivednamespace + "time" + UUID.randomUUID().toString();
		String time_unix = derivednamespace + "time" + UUID.randomUUID().toString();

		long timestamp = 0;
		Iri time_instant_iri = iri(time_instant);
		Iri time_unix_iri = iri(time_unix);
		
		// unix time following the w3c standard
	    modify.insert(iri(entity).has(hasTime,time_instant_iri));
	    modify.insert(time_instant_iri.isA(InstantClass).andHas(inTimePosition, time_unix_iri));
	    modify.insert(time_unix_iri.isA(TimePosition).andHas(numericPosition, timestamp).andHas(hasTRS, iri("http://dbpedia.org/resource/Unix_time")));
	    	    
	    kbClient.setQuery(modify.prefix(p_time).getQueryString());
	    kbClient.executeUpdate();
	}
	
	/**
	 * This method removes the time instance of the given entity. 
	 * @param kbClient
	 * @param entity
	 */
	static void removeTimeInstance(StoreClientInterface kbClient, String entity) {
		ModifyQuery modify = Queries.MODIFY();
		
		SelectQuery query = Queries.SELECT();
		Variable time_instant = query.var();
		Variable time_unix = query.var();
		Variable timestamp = query.var();
		Variable TRS = query.var();
		
		TriplePattern[] queryPattern = {iri(entity).has(hasTime,time_instant),
				time_instant.isA(InstantClass).andHas(inTimePosition, time_unix),
				time_unix.isA(TimePosition).andHas(numericPosition, timestamp).andHas(hasTRS, TRS)};
		
		modify.delete(queryPattern).where(queryPattern).prefix(p_time);
		kbClient.executeUpdate(modify.getQueryString());
	}
	
	/**
	 * returns the url of the agent used to calculate the given derived quantity
	 * @param kbClient
	 * @param derivedQuantity
	 * @return
	 */
	static String getAgentUrl(StoreClientInterface kbClient, String derivedQuantity) {
		SelectQuery query = Queries.SELECT();
		
		String queryKey = "url";
		Variable url = SparqlBuilder.var(queryKey);
		
		Iri derivedQuantityIRI = iri(derivedQuantity);
		
		Iri[] predicates = {isDerivedUsing,hasOperation,hasHttpUrl};
		GraphPattern queryPattern = getQueryGraphPattern(query, predicates, null, derivedQuantityIRI, url);
		
		query.select(url).where(queryPattern).prefix(p_agent,p_derived);
		
		kbClient.setQuery(query.getQueryString());
		
		String queryResult = kbClient.executeQuery().getJSONObject(0).getString(queryKey);
		
		return queryResult;
	}
	
	/** 
	 * query the list of inputs for the given derived quantity
	 * @param kbClient
	 * @param derivedQuantity
	 * @return
	 */
	static List<String> getInputs(StoreClientInterface kbClient, String derivedQuantity) {
		String queryKey = "input";
		Variable input = SparqlBuilder.var(queryKey);
		GraphPattern queryPattern = iri(derivedQuantity).has(isDerivedFrom,input);
		SelectQuery query = Queries.SELECT();
		
		query.prefix(p_derived).where(queryPattern).select(input);
		kbClient.setQuery(query.getQueryString());
		JSONArray queryResult = kbClient.executeQuery();
		
		List<String> inputs = new ArrayList<>();
		
		for (int i = 0; i < queryResult.length(); i++) {
			inputs.add(queryResult.getJSONObject(i).getString(queryKey));
		}
		
		return inputs;
	}
	
	/**
	 * This method retrieves a list of derivations that <isDerivedUsing> a given <agentIRI>. 
	 * @param kbClient
	 * @param agentIRI
	 * @return
	 */
	static List<String> getDerivations(StoreClientInterface kbClient, String agentIRI) {
		String queryKey = "derivation";
		Variable derivation = SparqlBuilder.var(queryKey);
		GraphPattern queryPattern = derivation.has(isDerivedUsing, iri(agentIRI));
		SelectQuery query = Queries.SELECT();
		
		query.prefix(p_derived, p_agent).select(derivation).where(queryPattern);
		kbClient.setQuery(query.getQueryString());
		JSONArray queryResult = kbClient.executeQuery();
		
		List<String> derivations = new ArrayList<>();
		for (int i = 0; i < queryResult.length(); i++) {
			derivations.add(queryResult.getJSONObject(i).getString(queryKey));
		}
		
		return derivations;
	}
	
	/**
	 * This is used at the stage to detect circular dependency
	 * if the input is part of a derived quantity, this will add the derived instance
	 * if the input is not a derived instance, it will add the input itself
	 * @param kbClient
	 * @param derivedQuantity
	 */
	static List<String> getInputsAndDerived(StoreClientInterface kbClient, String derived) {
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
		JSONArray queryResult = kbClient.executeQuery(query.getQueryString());
		
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
	 * returns the derived quantity instance, <instance> <derived:belongsTo> ?x
	 * @param kbClient
	 * @param instance
	 * @return
	 */
	static String getDerivedIRI(StoreClientInterface kbClient, String instance) {
		SelectQuery query = Queries.SELECT();
		String queryKey = "derived";
		Variable derived = SparqlBuilder.var(queryKey);
		GraphPattern queryPattern = iri(instance).has(belongsTo, derived);
		query.prefix(p_derived).select(derived).where(queryPattern);
		
		JSONArray queryResult = kbClient.executeQuery(query.getQueryString());
		
		if (queryResult.length() != 1) {
			throw new JPSRuntimeException(instance + " linked with " + String.valueOf(queryResult.length()) + " derived instances");
		}
		
		return queryResult.getJSONObject(0).getString(queryKey);
	}
	
	/**
	 * returns entities belonging to this derived instance, ?x <derived:belongsTo> <derivedIRI>
	 * @param kbClient
	 * @param derivedIRI
	 * @return
	 */
	static List<String> getDerivedEntities(StoreClientInterface kbClient, String derivedIRI) {
		SelectQuery query = Queries.SELECT();
		String queryKey = "entity";
		Variable entity = SparqlBuilder.var(queryKey);
		GraphPattern queryPattern = entity.has(belongsTo, iri(derivedIRI));
		query.prefix(p_derived).select(entity).where(queryPattern);
		
		JSONArray queryResult = kbClient.executeQuery(query.getQueryString());
		
		List<String> entities = new ArrayList<>();
		for (int i = 0; i < queryResult.length(); i++) {
			entities.add(queryResult.getJSONObject(i).getString(queryKey));
		}
		
		return entities;
	}
	
	/**
	 * returns the derived instance, where the given entity is an input to it
	 * <derived> <derived:isDerivedFrom> <entity>
	 * @param kbClient
	 * @param entities
	 */
	static List<List<String>> getIsDerivedFromEntities(StoreClientInterface kbClient, List<String> entities) {
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
			JSONArray queryResult = kbClient.executeQuery(query.getQueryString());
			
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
	static void deleteInstances(StoreClientInterface kbClient, List<String> entities) {
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
			
			kbClient.executeUpdate(modify.getQueryString());
		}
	}
	
	/**
	 * This methods deletes the status of a derivation instance. 
	 * @param kbClient
	 * @param instance
	 */
	static void deleteStatus(StoreClientInterface kbClient, String instance) {
		String status = getStatus(kbClient, instance);
		deleteInstances(kbClient, Arrays.asList(status));
	}
	
	/**
	 * This methods retrieves the timestamp of a derivation instance. 
	 * @param kbClient
	 * @param instance
	 */
	static long getTimestamp(StoreClientInterface kbClient, String instance) {
		String queryKey = "timestamp";
		SelectQuery query = Queries.SELECT();
		Variable time = SparqlBuilder.var(queryKey);
		
		Iri instanceIRI = iri(instance);
		
		// here we try to match two triple patterns
		// type 1: this is an input with a time stamp directly attached to it
		// type 2: this is an input that is part of a derived quantity
		// instances with timestamps directly attached
		Iri[] predicates = {hasTime,inTimePosition,numericPosition};
		GraphPattern queryPattern = getQueryGraphPattern(query, predicates, null, instanceIRI, time).optional();

		// instances that do not have time stamp directly attached, but belongs to a derived instance
		Iri[] predicates2 = {belongsTo, hasTime, inTimePosition, numericPosition};
		GraphPattern queryPattern2 = getQueryGraphPattern(query, predicates2, null, instanceIRI, time).optional();
		
		query.prefix(p_time,p_derived).where(queryPattern, queryPattern2).select(time);
		
		JSONArray queryResult = kbClient.executeQuery(query.getQueryString());
		
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
	static void updateTimeStamp(StoreClientInterface kbClient, String instance) {
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
		
		kbClient.setQuery(modify.getQueryString());
		kbClient.executeUpdate();
	}
	
	/** 
	 * returns rdf:type of the given instance, ignoring owl:NamedIndividual and owl:Thing
	 * when an instance is part of an input to another derived instance, the type is used to reconnect the appropriate instance
	 * @param kbClient
	 * @param instance
	 * @return
	 */
	static List<String> getInstanceClass(StoreClientInterface kbClient, List<String> instances) {
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
			kbClient.setQuery(query.getQueryString());
			
			JSONArray queryResult = kbClient.executeQuery();
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
	 * This method retrieves the rdf:type of a given instance, whereas ignoring certain perdefined rdf:type. 
	 * @param kbClient
	 * @param instance
	 * @return
	 */
	static String getInstanceClass(StoreClientInterface kbClient, String instance) {
		String queryKey = "class";
		
		SelectQuery query = Queries.SELECT();
		Variable type = SparqlBuilder.var(queryKey);
		
		// ignore certain rdf:type
		Expression<?>[] filters = new Expression<?>[classesToIgnore.size()];
		for (int j = 0; j < classesToIgnore.size(); j++) {
			filters[j] = Expressions.notEquals(type, classesToIgnore.get(j));
		}
		GraphPattern queryPattern = iri(instance).isA(type).filter(Expressions.and(filters));
		
		query.select(type).where(queryPattern);
		kbClient.setQuery(query.getQueryString());
		
		JSONArray queryResult = kbClient.executeQuery();
		// not having an rdf:type may be fine, but having more than 1 is an issue
		if (queryResult.length() > 1) {
			throw new JPSRuntimeException("DerivedQuantitySparql.getInstanceClass: more than 1 rdf:type for " + instance);
		} else if (queryResult.length() == 1) {
			return queryResult.getJSONObject(0).getString(queryKey);
		} else {
			return "";
		}
	}
	
	/**
	 * this is used to reconnect a newly created instance to an existing derived instance
	 * @param kbClient
	 * @param input
	 * @param derived
	 */
	static void reconnectInputToDerived(StoreClientInterface kbClient, String input, String derived) {
		ModifyQuery modify = Queries.MODIFY();
		
		TriplePattern insert_tp = iri(derived).has(isDerivedFrom, iri(input));
		
		modify.prefix(p_derived).insert(insert_tp);
		
		kbClient.executeUpdate(modify.getQueryString());
	}
	
	/**
	 * returns true if it is a derived quantity with time series
	 * @param kbClient
	 * @param derived_iri
	 */
	static boolean isDerivedWithTimeSeries(StoreClientInterface kbClient, String derived_iri) {
		SelectQuery query = Queries.SELECT();
		Variable type = query.var();
		TriplePattern tp = iri(derived_iri).isA(type);
		Expression<?> constraint = Expressions.equals(type, DerivationWithTimeSeries);
		
		// this query will return one result if the constraint matches
		GraphPattern queryPattern = tp.filter(constraint);
		
		query.prefix(p_derived).select(type).where(queryPattern);
		if (kbClient.executeQuery(query.getQueryString()).length() == 1) {
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
	static void addNewEntitiesToDerived(StoreClientInterface storeClient, String instance, List<String> newEntities) {
		ModifyQuery modify = Queries.MODIFY();
		
		for (String newEntity : newEntities) {
			modify.insert(iri(newEntity).has(belongsTo, iri(instance)));
		}
		
		storeClient.executeUpdate(modify.prefix(p_derived).getQueryString());
	}
	
	static void addNewDerivedIRIToFinishedStatus(StoreClientInterface storeClient, String finishedStatus, List<String> newDerivedIRI) {
		ModifyQuery modify = Queries.MODIFY();
		
		for (String newIRI : newDerivedIRI) {
			modify.insert(iri(finishedStatus).has(hasNewDerivedIRI, iri(newIRI)));
		}
		
		storeClient.executeUpdate(modify.prefix(p_derived).getQueryString());
	}
	
	private static GraphPattern getQueryGraphPattern(SelectQuery Query, Iri[] Predicates, Iri[] RdfType, Iri FirstNode, Variable LastNode) {
        GraphPattern CombinedGP = null;
    	
    	Variable[] Variables = new Variable[Predicates.length];
    	
    	// initialise intermediate nodes
    	for (int i=0; i < Variables.length-1; i++) {
    		Variables[i] = Query.var();
    	}
    	Variables[Variables.length-1] = LastNode;
    	
    	// first triple
    	GraphPattern firstTriple = FirstNode.has(Predicates[0],Variables[0]);
    	if (RdfType != null) {
    		if (RdfType[0] != null) {
    			CombinedGP = GraphPatterns.and(firstTriple,FirstNode.isA(RdfType[0]));
    		} else {
    			CombinedGP = GraphPatterns.and(firstTriple);
    		}
    	} else {
    		CombinedGP = GraphPatterns.and(firstTriple);
    	}
    	
    	// the remaining
    	for (int i=0; i < Variables.length-1; i++) {
    		GraphPattern triple = Variables[i].has(Predicates[i+1],Variables[i+1]);
    		if (RdfType != null) {
    			if (RdfType[i+1] != null) {
    				CombinedGP.and(triple,Variables[i].isA(RdfType[i+1]));
    			} else {
    				CombinedGP.and(triple);
    			}
    		} else {
    			CombinedGP.and(triple);
    		}
    	}
    	
    	// type for the final node, if given
    	if (RdfType != null) {
    		if (RdfType[RdfType.length-1] != null) {
    			CombinedGP.and(Variables[Variables.length-1].isA(RdfType[RdfType.length-1]));
    		}
    	}
    	
    	return CombinedGP;
    }

	/**
	 * This method chunks the given iri and returns its namespace. 
	 * @param iri
	 * @return
	 */
	private static String getNameSpace(String iri) {
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
    private static String trimIRI(String iri) {
    	if (iri.startsWith("<")) {
    		iri = iri.substring(1);
    	}
    	if (iri.endsWith(">")) {
    		iri = iri.substring(0, iri.length()-1);
    	}
    	return iri;
    }
}
