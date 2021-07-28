package uk.ac.cam.cares.jps.base.derivedquantity;

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

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.interfaces.StoreClientInterface;

/**
 * SPARQL queries/updates for instances related to derived quantities
 * These functions are called by the DerivedQuantityClient class
 * @author Kok Foong Lee
 *
 */
public class DerivedQuantitySparql{
	private static String derivednamespace = "http://www.theworldavatar.com/ontology/ontoderived/ontoderived.owl#";
	// prefix/namespace
	private static Prefix p_agent = SparqlBuilder.prefix("agent",iri("http://www.theworldavatar.com/ontology/ontoagent/MSM.owl#"));
	private static Prefix p_derived = SparqlBuilder.prefix("derived",iri(derivednamespace));
	private static Prefix p_time = SparqlBuilder.prefix("time", iri("http://www.w3.org/2006/time#"));
	
	// classes
	private static Iri Service = p_agent.iri("Service");
    private static Iri TimePosition = p_time.iri("TimePosition");
    private static Iri DerivedQuantity = p_derived.iri("DerivedQuantity");
    private static Iri DerivedQuantityWithTimeSeries = p_derived.iri("DerivedQuantityWithTimeSeries");
	
	// object properties
	private static Iri hasHttpUrl = p_agent.iri("hasHttpUrl");
	private static Iri isDerivedFrom = p_derived.iri("isDerivedFrom");
	private static Iri isDerivedUsing = p_derived.iri("isDerivedUsing");
	private static Iri belongsTo = p_derived.iri("belongsTo");
	private static Iri hasTime = p_time.iri("hasTime");
	private static Iri numericPosition = p_time.iri("numericPosition");
	
	// the derived quantity client relies on matching rdf:type to figure out which old instances to delete
	// if your instances have more than 1 rdf:type, you must add them to this list so that the client can figure out which to use
	private static List<Iri> classesToIgnore = Arrays.asList(iri(OWL.THING),iri(OWL.NAMEDINDIVIDUAL));
	
	/**
	 * creates a new instance of derived quantity, grouping the given entities under this instance
	 * whenever this derived quantity gets updated, the provided entities will get deleted by the client
	 * @param kbClient
	 * @param entities
	 * @param agentIRI
	 * @param agentURL
	 * @param inputs
	 */
	public static String createDerivedQuantity(StoreClientInterface kbClient, 
			List<String> entities, String agentIRI, String agentURL, List<String> inputs) {
	    ModifyQuery modify = Queries.MODIFY();
		
		// create a unique IRI for this new derived quantity
		String derivedQuantity = derivednamespace + "derived" + UUID.randomUUID().toString();
		while (checkInstanceExists(kbClient, derivedQuantity)) {
			derivedQuantity = derivednamespace + "derived" + UUID.randomUUID().toString();
		}
		
		Iri derived_iri = iri(derivedQuantity);
		
		modify.insert(derived_iri.isA(DerivedQuantity));
		
		for (String entity : entities) {
			// ensure that given entity is not part of another derived quantity
			if (!hasBelongsTo(kbClient, entity)) {
				modify.insert(iri(entity).has(belongsTo, derived_iri));
			} else {
				throw new JPSRuntimeException(entity + " is already part of another derived quantity");
			}
		}
		
		// link to agent
		modify.insert(derived_iri.has(isDerivedUsing,iri(agentIRI)));
		// add agent url
		modify.insert(iri(agentIRI).isA(Service).andHas(hasHttpUrl, iri(agentURL)));
		
		// add time stamp instance for the derived quantity
		long timestamp = 0;
		String derivedQuantityTime = derivednamespace + "time" + UUID.randomUUID().toString();
		
		while (checkInstanceExists(kbClient, derivedQuantityTime)) {
			derivedQuantityTime = derivednamespace + "time" + UUID.randomUUID().toString();
		}

		Iri derivedQuantityTime_iri = iri(derivedQuantityTime);
	    modify.insert(derived_iri.has(hasTime,derivedQuantityTime_iri));
	    modify.insert(derivedQuantityTime_iri.isA(TimePosition).andHas(numericPosition,timestamp));
	    
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
	public static String createDerivedQuantityWithTimeSeries(StoreClientInterface kbClient, 
			String entity, String agentIRI, String agentURL, List<String> inputs) {
	    ModifyQuery modify = Queries.MODIFY();
		
		// create a unique IRI for this new derived quantity
		String derivedQuantity = derivednamespace + "derived" + UUID.randomUUID().toString();
		while (checkInstanceExists(kbClient, derivedQuantity)) {
			derivedQuantity = derivednamespace + "derived" + UUID.randomUUID().toString();
		}
		
		Iri derived_iri = iri(derivedQuantity);
		
		modify.insert(derived_iri.isA(DerivedQuantityWithTimeSeries));
		
		if (!hasBelongsTo(kbClient,entity)) {
			modify.insert(iri(entity).has(belongsTo, derived_iri));
		} else {
			throw new JPSRuntimeException(entity + " is already part of another derived quantity");
		}
		
		
		// link to agent
		modify.insert(derived_iri.has(isDerivedUsing,iri(agentIRI)));
		// add agent url
		modify.insert(iri(agentIRI).isA(Service).andHas(hasHttpUrl,iri(agentURL)));
		
		// add time stamp instance for the derived quantity
		long timestamp = 0;
		String derivedQuantityTime = derivednamespace + "time" + UUID.randomUUID().toString();
		
		while (checkInstanceExists(kbClient, derivedQuantityTime)) {
			derivedQuantityTime = derivednamespace + "time" + UUID.randomUUID().toString();
		}

		Iri derivedQuantityTime_iri = iri(derivedQuantityTime);
	    modify.insert(derived_iri.has(hasTime,derivedQuantityTime_iri));
	    modify.insert(derivedQuantityTime_iri.isA(TimePosition).andHas(numericPosition,timestamp));
	    
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
	 * add a time stamp instance to your input if it does not exist
	 * this should be dealt with by a class specifically for input agents
	 * @param kbClient
	 * @param input
	 */
	public static void addTimeInstance(StoreClientInterface kbClient, String input) {
		ModifyQuery modify = Queries.MODIFY();
		
		// add time stamp instance for the derived quantity
		String inputTime = derivednamespace + "time" + UUID.randomUUID().toString();
		
		while (checkInstanceExists(kbClient, inputTime)) {
			inputTime = derivednamespace + "time" + UUID.randomUUID().toString();
		}

		long timestamp = Instant.now().getEpochSecond();
		Iri inputTime_iri = iri(inputTime);
	    modify.insert(iri(input).has(hasTime,inputTime_iri));
	    modify.insert(inputTime_iri.isA(TimePosition).andHas(numericPosition,timestamp));
	    
	    kbClient.setQuery(modify.prefix(p_time).getQueryString());
	    kbClient.executeUpdate();
	}
	
	/**
	 * returns the url of the agent used to calculate the given derived quantity
	 * @param kbClient
	 * @param derivedQuantity
	 * @return
	 */
	public static String getAgentUrl(StoreClientInterface kbClient, String derivedQuantity) {
		SelectQuery query = Queries.SELECT();
		
		String queryKey = "url";
		Variable url = SparqlBuilder.var(queryKey);
		
		Iri derivedQuantityIRI = iri(derivedQuantity);
		
		Iri[] predicates = {isDerivedUsing,hasHttpUrl};
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
	
	public static String[] getInputs(StoreClientInterface kbClient, String derivedQuantity) {
		String queryKey = "input";
		Variable input = SparqlBuilder.var(queryKey);
		GraphPattern queryPattern = iri(derivedQuantity).has(isDerivedFrom,input);
		SelectQuery query = Queries.SELECT();
		
		query.prefix(p_derived).where(queryPattern).select(input);
		kbClient.setQuery(query.getQueryString());
		JSONArray queryResult = kbClient.executeQuery();
		
		String[] inputs = new String[queryResult.length()];
		
		for (int i = 0; i < queryResult.length(); i++) {
			inputs[i] = queryResult.getJSONObject(i).getString(queryKey);
		}
		
		return inputs;
	}
	
	/**
	 * in addition to the inputs, this includes the derived quantity instances that the input is part of
	 * @param kbClient
	 * @param derivedQuantity
	 */
	public static List<String> getInputsAndDerived(StoreClientInterface kbClient, String derived) {
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
			inputsAndDerived.add(queryResult.getJSONObject(i).getString(inputQueryKey));
			
			// some inputs may be a derived quantity
			String derivedIRI = queryResult.getJSONObject(i).optString(derivedQueryKey);
			if (derivedIRI.length() > 0) {
				inputsAndDerived.add(derivedIRI);
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
	public static String getDerivedIRI(StoreClientInterface kbClient, String instance) {
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
	public static String[] getDerivedEntities(StoreClientInterface kbClient, String derivedIRI) {
		SelectQuery query = Queries.SELECT();
		String queryKey = "entity";
		Variable entity = SparqlBuilder.var(queryKey);
		GraphPattern queryPattern = entity.has(belongsTo, iri(derivedIRI));
		query.prefix(p_derived).select(entity).where(queryPattern);
		
		JSONArray queryResult = kbClient.executeQuery(query.getQueryString());
		
		String[] entities = new String[queryResult.length()];
		for (int i = 0; i < queryResult.length(); i++) {
			entities[i] = queryResult.getJSONObject(i).getString(queryKey);
		}
		
		return entities;
	}
	
	/**
	 * returns the derived instance, where the given entity is an input to it
	 * <derived> <derived:isDerivedFrom> <entity>
	 * @param kbClient
	 * @param entities
	 */
	public static List<List<String>> getIsDerivedFromEntities(StoreClientInterface kbClient, String... entities) {
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
		
		for (int i = 0; i < entities.length; i++) {
			SelectQuery query = Queries.SELECT();
			
			GraphPattern queryPattern = GraphPatterns.and(derived.has(isDerivedFrom,iri(entities[i])), iri(entities[i]).isA(entityType))
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
	public static void deleteInstances(StoreClientInterface kbClient, String... entities) {
		SubSelect sub = GraphPatterns.select();
		
		Variable subject = sub.var();
		Variable pred1 = sub.var();
		Variable pred2 = sub.var();
		Variable object = sub.var();
		
		TriplePattern[] delete_tp = new TriplePattern[entities.length * 2];
		GraphPattern[] queryPattern = new GraphPattern[entities.length * 2];
		int i = 0;
		for (String entity : entities) {
			// case 1: entity is the object
			delete_tp[i] = subject.has(pred1, iri(entity)); 
			queryPattern[i] = delete_tp[i].optional();
			i++;

			// case 2: entity is the subject
			delete_tp[i] = iri(entity).has(pred2, object); 
			queryPattern[i] = delete_tp[i].optional();
			i++;
		}
		
		sub.select(subject,pred1,pred2,object).where(queryPattern);
		
		ModifyQuery modify = Queries.MODIFY();
		modify.delete(delete_tp).where(sub);
		
		kbClient.executeUpdate(modify.getQueryString());
	}
	
	public static long getTimestamp(StoreClientInterface kbClient, String instance) {
		String queryKey = "timestamp";
		SelectQuery query = Queries.SELECT();
		Variable time = SparqlBuilder.var(queryKey);
		
		Iri instanceIRI = iri(instance);
		
		// here we try to match two triple patterns
		// type 1: this is an input with a time stamp directly attached to it
		// type 2: this is an input that is part of a derived quantity
		// instances with timestamps directly attached
		Iri[] predicates = {hasTime,numericPosition};
		GraphPattern queryPattern = getQueryGraphPattern(query, predicates, null, instanceIRI, time).optional();

		// instances that do not have time stamp directly attached, but belongs to a derived instance
		Iri[] predicates2 = {belongsTo, hasTime, numericPosition};
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
					+ "adding a timestamp using DerivedQuantitySparql.addTimeInstance or create a derived instance using this instance", e);
		}
	}
	
	/**
	 * update time stamp for a given IRI of an instance
	 * @param kbClient
	 * @param instance
	 */
	public static void updateTimeStamp(StoreClientInterface kbClient, String instance) {
		long timestamp = Instant.now().getEpochSecond();
		
		// obtain time IRI through sub query
		SubSelect sub = GraphPatterns.select();
		
		Variable timeIRI = SparqlBuilder.var("timeIRI");
		Variable oldvalue = SparqlBuilder.var("oldvalue");
		
		GraphPattern queryPattern = GraphPatterns.and(iri(instance).has(hasTime,timeIRI), timeIRI.has(numericPosition,oldvalue));
		
		// triples to add and delete
		TriplePattern delete_tp = timeIRI.has(numericPosition,oldvalue);
		TriplePattern insert_tp = timeIRI.has(numericPosition, timestamp);
		
		sub.select(timeIRI,oldvalue).where(queryPattern);
		
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
	public static String[] getInstanceClass(StoreClientInterface kbClient, String... instances) {
		String queryKey = "class";
		
		String[] classOfInstances = new String[instances.length]; 

		
		for (int i = 0; i < instances.length; i++) {
			SelectQuery query = Queries.SELECT();
			Variable type = SparqlBuilder.var(queryKey);
			
			// ignore certain rdf:type
			Expression<?>[] filters = new Expression<?>[classesToIgnore.size()];
			for (int j = 0; j < classesToIgnore.size(); j++) {
				filters[j] = Expressions.notEquals(type, classesToIgnore.get(j));
			}
			GraphPattern queryPattern = iri(instances[i]).isA(type).filter(Expressions.and(filters));
			
			query.select(type).where(queryPattern);
			kbClient.setQuery(query.getQueryString());
			
			JSONArray queryResult = kbClient.executeQuery();
			// not having an rdf:type may be fine, but having more than 1 is an issue
			if (queryResult.length() > 1) {
				throw new JPSRuntimeException("DerivedQuantitySparql.getInstanceClass: more than 1 rdf:type for " + instances[i]);
			} else if (queryResult.length() == 1) {
				classOfInstances[i] = queryResult.getJSONObject(0).getString(queryKey);
			} else {
				classOfInstances[i] = "";
			}
		}
		return classOfInstances;
	}
	
	/**
	 * this is used to reconnect a newly created instance to an existing derived instance
	 * @param kbClient
	 * @param input
	 * @param derived
	 */
	public static void reconnectInputToDerived(StoreClientInterface kbClient, String input, String derived) {
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
	public static boolean isDerivedWithTimeSeries(StoreClientInterface kbClient, String derived_iri) {
		SelectQuery query = Queries.SELECT();
		Variable type = query.var();
		TriplePattern tp = iri(derived_iri).isA(type);
		Expression<?> constraint = Expressions.equals(type, DerivedQuantityWithTimeSeries);
		
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
	public static void addNewEntitiesToDerived(StoreClientInterface storeClient, String instance, String[] newEntities) {
		ModifyQuery modify = Queries.MODIFY();
		
		for (String newEntity : newEntities) {
			modify.insert(iri(newEntity).has(belongsTo, iri(instance)));
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
}
