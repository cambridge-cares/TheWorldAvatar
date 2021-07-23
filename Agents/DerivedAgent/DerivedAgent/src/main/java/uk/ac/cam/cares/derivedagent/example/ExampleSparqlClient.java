package uk.ac.cam.cares.derivedagent.example;

import static org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf.iri;

import java.util.UUID;

import org.eclipse.rdf4j.model.vocabulary.OWL;
import org.eclipse.rdf4j.sparqlbuilder.core.Prefix;
import org.eclipse.rdf4j.sparqlbuilder.core.SparqlBuilder;
import org.eclipse.rdf4j.sparqlbuilder.core.Variable;
import org.eclipse.rdf4j.sparqlbuilder.core.query.ModifyQuery;
import org.eclipse.rdf4j.sparqlbuilder.core.query.Queries;
import org.eclipse.rdf4j.sparqlbuilder.core.query.SelectQuery;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.GraphPattern;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.GraphPatterns;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.TriplePattern;
import org.eclipse.rdf4j.sparqlbuilder.rdf.Iri;
import org.json.JSONArray;

import uk.ac.cam.cares.jps.base.interfaces.StoreClientInterface;

public class ExampleSparqlClient {
	StoreClientInterface storeClient;
	
	// namespace
	public static String namespace = "http://derived_example#";
	private static Prefix p_namespace = SparqlBuilder.prefix("derived_example", iri(namespace));
	
	// rdf:type
	private static Iri MaxTime = p_namespace.iri("MaxTime");
	private static Iri MinTime = p_namespace.iri("MinTime");
	private static Iri TimeDuration = p_namespace.iri("TimeDuration");
	private static Iri InputData = p_namespace.iri("InputData"); // has a time series instance
	
	// property
	private static Iri hasValue = p_namespace.iri("hasValue");
	
    public ExampleSparqlClient(StoreClientInterface storeClient) {
    	this.storeClient = storeClient;
    }
    
    /**
     * in this example the instances are simple, directly linked to a literal
     * <instance> <hasValue> ?x
     * @param instance
     * @return
     */
    public int getValue(String instance) {
    	SelectQuery query = Queries.SELECT();
    	
    	String key = "value";
    	Variable value = SparqlBuilder.var(key);
    	TriplePattern queryPattern = iri(instance).has(hasValue,value);
    	
    	query.prefix(p_namespace).select(value).where(queryPattern);
    	
    	JSONArray queryResult = storeClient.executeQuery(query.getQueryString());
    	
    	return queryResult.getJSONObject(0).getInt(key);
    }
    
    public boolean isMaxTime(String instance) {
    	String query = String.format("ask {<%s> a <%s>}", instance, (namespace + "MaxTime"));
    	storeClient.setQuery(query);
    	boolean result = storeClient.executeQuery().getJSONObject(0).getBoolean("ASK");
    	return result;
    }
    
    public boolean isMinTime(String instance) {
    	String query = String.format("ask {<%s> a <%s>}", instance, (namespace + "MinTime"));
    	storeClient.setQuery(query);
    	boolean result = storeClient.executeQuery().getJSONObject(0).getBoolean("ASK");
    	return result;
    }
    
    public String createInputData() {
    	String inputIRI = namespace + UUID.randomUUID().toString();
    	
    	while (checkInstanceExists(inputIRI)) {
    		inputIRI = namespace + UUID.randomUUID().toString();
    	}
    	
    	ModifyQuery modify = Queries.MODIFY();
    	modify.insert(iri(inputIRI).isA(InputData)).prefix(p_namespace);
    	
    	// create the instance on kg
    	storeClient.executeUpdate(modify.getQueryString());

    	return inputIRI;
    }
    
    /**
     * create a new max time instance, return the IRI of the new instance
     * @param maxtime
     * @return
     */
    public String createMaxTime(int maxtime) {
    	String max_time_iri = namespace + UUID.randomUUID().toString();
    	
    	ModifyQuery modify = Queries.MODIFY();
    	
    	// instantiate max time
    	while (checkInstanceExists(max_time_iri)) {
    		max_time_iri = namespace + UUID.randomUUID().toString();
    	}
    	
    	modify.insert(iri(max_time_iri).isA(MaxTime).andIsA(iri(OWL.NAMEDINDIVIDUAL)).andHas(hasValue, maxtime));
    	
    	storeClient.executeUpdate(modify.prefix(p_namespace).getQueryString());
    	
    	return max_time_iri;
    }
    
    /**
     * creates a new min time instance
     * @param mintime
     * @return
     */
    public String createMinTime(int mintime) {
    	String min_time_iri = namespace + UUID.randomUUID().toString();
    	
    	ModifyQuery modify = Queries.MODIFY();
    	
    	while (checkInstanceExists(min_time_iri)) {
    		min_time_iri = namespace + UUID.randomUUID().toString();
    	}
    	modify.insert(iri(min_time_iri).isA(MinTime).andIsA(iri(OWL.NAMEDINDIVIDUAL)).andHas(hasValue, mintime));
    	
    	storeClient.executeUpdate(modify.prefix(p_namespace).getQueryString());
    	
    	return min_time_iri;
    }
    
    /**
     * creates a time duration instance
     * @param timeduration
     * @return
     */
    public String createTimeDuration(int timeduration) {
    	String timeduration_iri = namespace + UUID.randomUUID().toString();
    	
    	ModifyQuery modify = Queries.MODIFY();
    	
    	while (checkInstanceExists(timeduration_iri)) {
    		timeduration_iri = namespace + UUID.randomUUID().toString();
    	}
    	modify.insert(iri(timeduration_iri).isA(TimeDuration).andIsA(iri(OWL.NAMEDINDIVIDUAL)).andHas(hasValue, timeduration));
    	
    	storeClient.executeUpdate(modify.prefix(p_namespace).getQueryString());
    	
    	return timeduration_iri;
    }
    
    /**
     * returns true if instance exists in the triple store
     * @param instance
     * @return
     */
    private boolean checkInstanceExists(String instance) {
    	SelectQuery query = Queries.SELECT();
    	
    	// includes both cases where the instance is a subject and object
    	GraphPattern queryPattern = GraphPatterns.and(iri(instance).has(query.var(),query.var()).optional(),
    			query.var().has(query.var(),iri(instance)).optional());
    	
    	query.prefix(p_namespace).where(queryPattern);
    
    	JSONArray queryresult = storeClient.executeQuery(query.getQueryString());
    	
    	if (queryresult.getJSONObject(0).isEmpty()) {
    		return false;
    	} else {
    		return true;
    	}
    }
}
