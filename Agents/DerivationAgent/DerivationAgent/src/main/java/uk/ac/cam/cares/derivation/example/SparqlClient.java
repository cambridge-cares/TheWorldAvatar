package uk.ac.cam.cares.derivation.example;

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
import org.eclipse.rdf4j.sparqlbuilder.rdf.Iri;
import org.json.JSONArray;

import uk.ac.cam.cares.jps.base.interfaces.StoreClientInterface;

public class SparqlClient {
	StoreClientInterface storeClient;
	
	// namespace
	public static String namespace = "http://derived_example#";
	private static Prefix p_namespace = SparqlBuilder.prefix("derived_example", iri(namespace));
	
	// rdf:type
	private static Iri MaxValue = p_namespace.iri("MaxValue");
	private static Iri MinValue = p_namespace.iri("MinValue");
	private static Iri CalculatedDifference = p_namespace.iri("CalculatedDifference");
	private static Iri InputData = p_namespace.iri("InputData"); // has a time series instance
	private static Iri ScalarValue = p_namespace.iri("ScalarValue");
	
	// property
	private static Iri hasValue = p_namespace.iri("hasValue");
	private static Iri numericalValue = p_namespace.iri("numericalValue");
	
    public SparqlClient(StoreClientInterface storeClient) {
    	this.storeClient = storeClient;
    }
    
    /**
     * clears kg before initialising anything
     */
    public void clearKG() {
    	Variable x = SparqlBuilder.var("x");
    	Variable y = SparqlBuilder.var("y");
    	Variable z = SparqlBuilder.var("z");
    	
    	ModifyQuery modify = Queries.MODIFY();
    	modify.delete(x.has(y,z)).where(x.has(y,z));
    	
    	storeClient.executeUpdate(modify.getQueryString());
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
    	Variable value_iri = query.var();
    	Variable value = SparqlBuilder.var(key);
    	GraphPattern queryPattern = GraphPatterns.and(iri(instance).has(hasValue,value_iri), value_iri.has(numericalValue,value));
    	
    	query.prefix(p_namespace).select(value).where(queryPattern);
    	
    	JSONArray queryResult = storeClient.executeQuery(query.getQueryString());
    	
    	return queryResult.getJSONObject(0).getInt(key);
    }
    
    public boolean isMaxValue(String instance) {
    	String query = String.format("ask {<%s> a <%s>}", instance, (namespace + "MaxValue"));
    	storeClient.setQuery(query);
    	boolean result = storeClient.executeQuery().getJSONObject(0).getBoolean("ASK");
    	return result;
    }
    
    public boolean isMinValue(String instance) {
    	String query = String.format("ask {<%s> a <%s>}", instance, (namespace + "MinValue"));
    	storeClient.setQuery(query);
    	boolean result = storeClient.executeQuery().getJSONObject(0).getBoolean("ASK");
    	return result;
    }
    
    public boolean isInputData(String instance) {
    	String query = String.format("ask {<%s> a <%s>}", instance, (namespace + "InputData"));
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
     * create a new max value instance, return the IRI of the new instance
     * @param maxtime
     * @return
     */
    public String[] createMaxValue(int maxvalue) {
    	String max_value_iri = namespace + UUID.randomUUID().toString();
    	String max_value_value_iri = namespace + UUID.randomUUID().toString();
    	
    	ModifyQuery modify = Queries.MODIFY();
    	
    	// instantiate max time
    	while (checkInstanceExists(max_value_iri) || checkInstanceExists(max_value_value_iri)) {
    		max_value_iri = namespace + UUID.randomUUID().toString();
    		max_value_value_iri = namespace + UUID.randomUUID().toString();
    	}
    	
    	modify.insert(iri(max_value_iri).isA(MaxValue).andIsA(iri(OWL.NAMEDINDIVIDUAL)).andHas(hasValue, iri(max_value_value_iri)));
    	modify.insert(iri(max_value_value_iri).has(numericalValue, maxvalue).andIsA(ScalarValue));
    	
    	storeClient.executeUpdate(modify.prefix(p_namespace).getQueryString());
    	
    	String[] createdEntities = {max_value_iri,max_value_value_iri};
    	
    	return createdEntities;
    }
    
    /**
     * creates a new min time instance
     * @param mintime
     * @return
     */
    public String[] createMinValue(int minvalue) {
    	String min_value_iri = namespace + UUID.randomUUID().toString();
    	String min_value_value_iri = namespace + UUID.randomUUID().toString();
    	
    	ModifyQuery modify = Queries.MODIFY();
    	
    	while (checkInstanceExists(min_value_iri) || checkInstanceExists(min_value_value_iri)) {
    		min_value_iri = namespace + UUID.randomUUID().toString();
    		min_value_value_iri = namespace + UUID.randomUUID().toString();
    	}
    	
    	modify.insert(iri(min_value_iri).isA(MinValue).andIsA(iri(OWL.NAMEDINDIVIDUAL)).andHas(hasValue, iri(min_value_value_iri)));
    	modify.insert(iri(min_value_value_iri).has(numericalValue, minvalue).andIsA(ScalarValue));
    	
    	storeClient.executeUpdate(modify.prefix(p_namespace).getQueryString());
    	String[] createdEntities = {min_value_iri,min_value_value_iri};
    	
    	return createdEntities;
    }
    
    /**
     * creates a difference instance
     * @param difference
     * @return
     */
    public String[] createCalculatedDifference(int difference) {
    	String difference_iri = namespace + UUID.randomUUID().toString();
    	String difference_value_iri = namespace + UUID.randomUUID().toString();
    	
    	ModifyQuery modify = Queries.MODIFY();
    	
    	while (checkInstanceExists(difference_iri) || checkInstanceExists(difference_value_iri)) {
    		difference_iri = namespace + UUID.randomUUID().toString();
    		difference_value_iri = namespace + UUID.randomUUID().toString();
    	}
    	
    	modify.insert(iri(difference_iri).isA(CalculatedDifference).andIsA(iri(OWL.NAMEDINDIVIDUAL)).andHas(hasValue, iri(difference_value_iri)));
    	modify.insert(iri(difference_value_iri).has(numericalValue, difference).andIsA(ScalarValue));
    	
    	storeClient.executeUpdate(modify.prefix(p_namespace).getQueryString());
    	String[] createdEntities = {difference_iri,difference_value_iri};
    	
    	return createdEntities;
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
