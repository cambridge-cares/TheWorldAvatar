package uk.ac.cam.cares.derivedagent.example;

import static org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf.iri;

import java.util.UUID;

import org.eclipse.rdf4j.sparqlbuilder.core.Prefix;
import org.eclipse.rdf4j.sparqlbuilder.core.SparqlBuilder;
import org.eclipse.rdf4j.sparqlbuilder.core.query.ModifyQuery;
import org.eclipse.rdf4j.sparqlbuilder.core.query.Queries;
import org.eclipse.rdf4j.sparqlbuilder.core.query.SelectQuery;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.GraphPattern;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.GraphPatterns;
import org.eclipse.rdf4j.sparqlbuilder.rdf.Iri;
import org.json.JSONArray;

import uk.ac.cam.cares.jps.base.interfaces.StoreClientInterface;

public class ExampleSparqlClient {
	StoreClientInterface storeClient;
	
	// namespace
	private static String namespace = "http://derived_example#";
	private static Prefix p_namespace = SparqlBuilder.prefix("derived_example", iri(namespace));
	
	// rdf:type
	private static Iri MaxTime = p_namespace.iri("MaxTime");
	private static Iri MinTime = p_namespace.iri("MinTime");
	private static Iri TimeDuration = p_namespace.iri("TimeDuration");
	
	// property
	private static Iri hasValue = p_namespace.iri("hasValue");
	
    public ExampleSparqlClient(StoreClientInterface storeClient) {
    	this.storeClient = storeClient;
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
    		modify.insert(iri(max_time_iri).isA(MaxTime).andHas(hasValue, maxtime));
    	}
    	
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
    		modify.insert(iri(min_time_iri).isA(MinTime).andHas(hasValue, mintime));
    	}
    	
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
    		modify.insert(iri(timeduration_iri).isA(TimeDuration).andHas(hasValue, timeduration));
    	}
    	
    	storeClient.executeUpdate(modify.prefix(p_namespace).getQueryString());
    	
    	return timeduration_iri;
    }
    
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
