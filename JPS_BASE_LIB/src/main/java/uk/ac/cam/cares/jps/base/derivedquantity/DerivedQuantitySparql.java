package uk.ac.cam.cares.jps.base.derivedquantity;

import static org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf.iri;

import java.time.Instant;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.rdf4j.sparqlbuilder.constraint.Expression;
import org.eclipse.rdf4j.sparqlbuilder.constraint.Expressions;
import org.eclipse.rdf4j.sparqlbuilder.core.Assignment;
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
import org.json.JSONArray;

import uk.ac.cam.cares.jps.base.interfaces.KnowledgeBaseClientInterface;

/**
 * SPARQL queries/updates for instances related to derived quantities
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
	
	/**
	 * use this to instantiate a derived quantity with multiple inputs
	 * 
	 * @param kbClient
	 * @param derivedQuantity
	 * @param inputs
	 * @param agentIRI
	 * @param agentURL
	 */
	public static void initDerivedQuantity(KnowledgeBaseClientInterface kbClient, String derivedQuantity, String agentIRI, String agentURL, String... inputs) {
		ModifyQuery modify = Queries.MODIFY();
		
		Iri derivedQuantity_iri = iri(derivedQuantity);
		
		long timestamp = 0;

		// link to agent
		modify.insert(derivedQuantity_iri.has(isDerivedUsing,iri(agentIRI)));
		// add agent url
		modify.insert(iri(agentIRI).isA(Service).andHas(hasHttpUrl,agentURL));
		
		// add time stamp instance for the derived quantity
		int numTime = DerivedQuantitySparql.countTimeInstance(kbClient);
		String derivedQuantityTime = derivednamespace + "time" + numTime;
		
		while (checkTimeExists(kbClient, derivedQuantityTime)) {
			numTime += 1;
			derivedQuantityTime = derivednamespace + "time" + numTime;
		}

		Iri derivedQuantityTime_iri = iri(derivedQuantityTime);
	    modify.insert(derivedQuantity_iri.has(hasTime,derivedQuantityTime_iri));
	    modify.insert(derivedQuantityTime_iri.isA(TimePosition).andHas(numericPosition,timestamp));
	    
	    // link derived quantity to each input
	    for (int i = 0; i < inputs.length; i++) {
			modify.insert(derivedQuantity_iri.has(isDerivedFrom, iri(inputs[i])));
	    }
	    
	    kbClient.setQuery(modify.prefix(p_time,p_derived,p_agent).getQueryString());
	    kbClient.executeUpdate();
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
	public static void createDerivedQuantity(KnowledgeBaseClientInterface kbClient, 
			List<String> entities, String agentIRI, String agentURL, List<String> inputs) {
	    ModifyQuery modify = Queries.MODIFY();
		
		// create a unique IRI for this new derived quantity
		int numDerived = countDerived(kbClient);
		
		String derivedQuantity = derivednamespace + "derived" + numDerived;
		while (checkDerivedExists(kbClient, derivedQuantity)) {
			numDerived += 1;
			derivedQuantity = derivednamespace + "derived" + numDerived;
		}
		
		Iri derived_iri = iri(derivedQuantity);
		
		for (String entity : entities) {
			modify.insert(iri(entity).has(belongsTo, derived_iri));
		}
		
		// link to agent
		modify.insert(derived_iri.has(isDerivedUsing,iri(agentIRI)));
		// add agent url
		modify.insert(iri(agentIRI).isA(Service).andHas(hasHttpUrl,agentURL));
		
		// add time stamp instance for the derived quantity
		long timestamp = 0;
		int numTime = DerivedQuantitySparql.countTimeInstance(kbClient);
		String derivedQuantityTime = derivednamespace + "time" + numTime;
		
		while (checkTimeExists(kbClient, derivedQuantityTime)) {
			numTime += 1;
			derivedQuantityTime = derivednamespace + "time" + numTime;
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
	}
	
	/**
	 * counts the number of derived quantity instances
	 * @param kbClient
	 */
	private static int countDerived(KnowledgeBaseClientInterface kbClient) {
		SelectQuery query = Queries.SELECT();
		String queryKey = "numDerived";
		Variable numDerived = SparqlBuilder.var(queryKey);
		Variable derived = query.var();
		
		GraphPattern queryPattern = GraphPatterns.and(derived.isA(DerivedQuantity),
				derived.isA(DerivedQuantityWithTimeSeries));
		
		Assignment count = Expressions.count(derived).as(numDerived);
		
		query.prefix(p_derived).select(count).where(queryPattern);
		
		kbClient.setQuery(query.getQueryString());
		return kbClient.executeQuery().getJSONObject(0).getInt(queryKey);
	}
	
	/**
	 * checks if an instance already exists to avoid IRI clash
	 * @param kbClient
	 * @param derived
	 */
	private static boolean checkDerivedExists(KnowledgeBaseClientInterface kbClient, String derived) {
		// includes DerivedQuantity and DerivedQuantityWithTimeSeries 
		String query = String.format("ask {<%s> a ?x}",derived);
		kbClient.setQuery(query);
		boolean derivedExists = kbClient.executeQuery().getJSONObject(0).getBoolean("ASK");
		return derivedExists;
	}
	
	/**
	 * add a time stamp instance to your input if it does not exist
	 * this should be dealt with by a class specifically for input agents
	 * @param kbClient
	 * @param input
	 */
	public static void initInputTimeStamp(KnowledgeBaseClientInterface kbClient, String input) {
		ModifyQuery modify = Queries.MODIFY();
		
		// add time stamp instance for the derived quantity
		int numTime = countTimeInstance(kbClient);
		String inputTime = derivednamespace + "time" + numTime;
		
		while (checkTimeExists(kbClient, inputTime)) {
			numTime += 1;
			inputTime = derivednamespace + "time" + numTime;
		}

		long timestamp = Instant.now().getEpochSecond();
		Iri inputTime_iri = iri(inputTime);
	    modify.insert(iri(input).has(hasTime,inputTime_iri));
	    modify.insert(inputTime_iri.isA(TimePosition).andHas(numericPosition,timestamp));
	    
	    kbClient.setQuery(modify.prefix(p_time).getQueryString());
	    kbClient.executeUpdate();
	}
	
	/**
	 * ensure there are no time duplicates, maybe the time instance was already initialise before,, e.g. an input is also a derived quantity
	 * @param kbClient
	 * @param time_iri_string
	 * @return
	 */
	private static boolean checkTimeExists(KnowledgeBaseClientInterface kbClient, String time_iri) {
		// ask query is not supported by SparqlBuilder, hence hardcode
		String query = String.format("ask {<%s> a <http://www.w3.org/2006/time#TimePosition>}",time_iri);
		kbClient.setQuery(query);
		boolean timeExist = kbClient.executeQuery().getJSONObject(0).getBoolean("ASK");
		return timeExist;
	}

	public static int countTimeInstance(KnowledgeBaseClientInterface kbClient) {
		SelectQuery query = Queries.SELECT();
    	String queryKey = "numtime";
    	Variable time = query.var();
    	Variable numtime = SparqlBuilder.var(queryKey);
    	GraphPattern querypattern = time.isA(TimePosition);
    	Assignment count = Expressions.count(time).as(numtime);
    	
    	query.prefix(p_time).select(count).where(querypattern);
    	kbClient.setQuery(query.getQueryString());
    	return kbClient.executeQuery().getJSONObject(0).getInt(queryKey);
	}
	
	/**
	 * returns the url of the agent used to calculate the given derived quantity
	 * @param kbClient
	 * @param derivedQuantity
	 * @return
	 */
	public static String getAgentUrl(KnowledgeBaseClientInterface kbClient, String derivedQuantity) {
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
	
	public static String[] getInputs(KnowledgeBaseClientInterface kbClient, String derivedQuantity) {
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
	
	public static long getTimestamp(KnowledgeBaseClientInterface kbClient, String instance) {
		String queryKey = "timestamp";
		SelectQuery query = Queries.SELECT();
		Variable time = SparqlBuilder.var(queryKey);
		
		Iri instanceIRI = iri(instance);
		
		Iri[] predicates = {hasTime,numericPosition};
		GraphPattern queryPattern = getQueryGraphPattern(query, predicates, null, instanceIRI, time);
		
		query.prefix(p_time).where(queryPattern).select(time);
		
		kbClient.setQuery(query.getQueryString());
		
		long timestamp = kbClient.executeQuery().getJSONObject(0).getLong(queryKey);
		
	    return timestamp;
	}
	
	/**
	 * update time stamp for a given IRI of an instance
	 * @param kbClient
	 * @param instance
	 */
	public static void updateTimeStamp(KnowledgeBaseClientInterface kbClient, String instance) {
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
	
	public static String getInstanceClass(KnowledgeBaseClientInterface kbClient, String instance) {
		String queryKey = "class";
		SelectQuery query = Queries.SELECT();
		Variable type = SparqlBuilder.var(queryKey);
		GraphPattern queryPattern = iri(instance).isA(type);
		query.select(type).where(queryPattern);
		kbClient.setQuery(query.getQueryString());
		return kbClient.executeQuery().getJSONObject(0).getString(queryKey);
	}
	
	public static void replaceDerivedTriples(KnowledgeBaseClientInterface kbClient, String oldInstance, String newInstance) {
		ModifyQuery modify = Queries.MODIFY();
		Iri oldInstanceIRI = iri(oldInstance);
		Iri newInstanceIRI = iri(newInstance);
		
		Variable agentIRI = SparqlBuilder.var("agent");
		Variable input = SparqlBuilder.var("input");
		Variable quantity = SparqlBuilder.var("quantity");
		Variable time = SparqlBuilder.var("time");
		
		// triples to add and delete
		TriplePattern delete_tp1 = oldInstanceIRI.has(isDerivedUsing,agentIRI);
		TriplePattern delete_tp2 = oldInstanceIRI.has(isDerivedFrom,input);
		TriplePattern delete_tp3 = quantity.has(isDerivedFrom,oldInstanceIRI);
		TriplePattern delete_tp4 = oldInstanceIRI.has(hasTime,time);
		
		TriplePattern insert_tp1 = newInstanceIRI.has(isDerivedUsing,agentIRI);
		TriplePattern insert_tp2 = newInstanceIRI.has(isDerivedFrom,input);
		TriplePattern insert_tp3 = quantity.has(isDerivedFrom,newInstanceIRI);
		TriplePattern insert_tp4 = newInstanceIRI.has(hasTime,time);
		
		// sub query to find related IRIs
		SubSelect sub = GraphPatterns.select();
		sub.select(agentIRI,input,quantity,time).where(delete_tp1,delete_tp2,delete_tp3,delete_tp4);
		
		modify.prefix(p_derived,p_time).delete(delete_tp1,delete_tp2,delete_tp3,delete_tp4).insert(insert_tp1,insert_tp2,insert_tp3,insert_tp4).where(sub);
		
		kbClient.setQuery(modify.getQueryString());
		kbClient.executeUpdate();
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
