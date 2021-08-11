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

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
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
	private static Iri Average = p_namespace.iri("Average");
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
     * query <instance> <hasValue> ?x, ?x <numericalValue> ?value
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
    
    boolean isAverage(String instance) {
    	String query = String.format("ask {<%s> a <%s>}", instance, (namespace + "Average"));
    	storeClient.setQuery(query);
    	boolean result = storeClient.executeQuery().getJSONObject(0).getBoolean("ASK");
    	return result;
    }
    
    public String createInputData() {
    	String inputIRI = namespace + UUID.randomUUID().toString();
    	
    	ModifyQuery modify = Queries.MODIFY();
    	modify.insert(iri(inputIRI).isA(InputData)).prefix(p_namespace);
    	
    	// create the instance on kg
    	storeClient.executeUpdate(modify.getQueryString());

    	return inputIRI;
    }
    
    /**
     * create a new max value instance, return the IRI of the new instance
     * <iri> a <MasValue>
     * <iri> a owl:NamedIndividual
     * @param maxtime
     * @return
     */
    public String createMaxValue() {
    	String max_value_iri = namespace + UUID.randomUUID().toString();
    	ModifyQuery modify = Queries.MODIFY();
    	modify.insert(iri(max_value_iri).isA(MaxValue).andIsA(iri(OWL.NAMEDINDIVIDUAL)));
    	storeClient.executeUpdate(modify.prefix(p_namespace).getQueryString());
    	return max_value_iri;
    }
    
    /**
     * creates a new min value instance
     * <iri> a <MinValue>
     * <iri> a owl:NamedIndividual
     * @param mintime
     * @return
     */
    public String createMinValue() {
    	String min_value_iri = namespace + UUID.randomUUID().toString();
    	ModifyQuery modify = Queries.MODIFY();
    	modify.insert(iri(min_value_iri).isA(MinValue).andIsA(iri(OWL.NAMEDINDIVIDUAL)));
    	storeClient.executeUpdate(modify.prefix(p_namespace).getQueryString());
    	return min_value_iri;
    }
    
    /**
     * creates a difference instance. 
     * <iri> a <CalculatedDifference>
     * <iri> a owl:NamedIndividual
     * @param difference
     * @return
     */
    public String createCalculatedDifference() {
    	String difference_iri = namespace + UUID.randomUUID().toString();    	
    	ModifyQuery modify = Queries.MODIFY();
    	modify.insert(iri(difference_iri).isA(CalculatedDifference).andIsA(iri(OWL.NAMEDINDIVIDUAL)));
    	storeClient.executeUpdate(modify.prefix(p_namespace).getQueryString());
    	return difference_iri;
    }
    
    /**
     * creates an average instance. 
     * <iri> a <Average>
     * <iri> a owl:NamedIndividual
     * @param difference
     * @return
     */
    String createAverage() {
    	String average_iri = namespace + UUID.randomUUID().toString();  
    	ModifyQuery modify = Queries.MODIFY();
    	modify.insert(iri(average_iri).isA(Average).andIsA(iri(OWL.NAMEDINDIVIDUAL)));
    	storeClient.executeUpdate(modify.prefix(p_namespace).getQueryString());
    	return average_iri;
    }
    
    /**
     * adds a value instance to the given property
     * <property> <hasValue> <valueIRI>, <valueIRI> a <ScalarValue>, <valueIRI> <numericalValue> value
     * @param property
     * @param value
     * @return
     */
    public String addValueInstance(String property, int value) {
    	String value_iri = namespace + UUID.randomUUID().toString();
    	ModifyQuery modify = Queries.MODIFY();
    	modify.insert(iri(property).has(hasValue,iri(value_iri)));
    	modify.insert(iri(value_iri).isA(ScalarValue).andHas(numericalValue,value));
    	storeClient.executeUpdate(modify.prefix(p_namespace).getQueryString());
    	return value_iri;
    }
    
    /**
     * returns the input instance. There can only be one input instance at a time based on the way it is initialised
     * @return
     */
    public String getInputIRI() {
    	SelectQuery query = Queries.SELECT();
    	String queryKey = "input";
    	Variable input = SparqlBuilder.var(queryKey);
    	
    	GraphPattern queryPattern = input.isA(InputData);
    	
    	query.prefix(p_namespace).select(input).where(queryPattern);
    	
    	JSONArray queryResult = storeClient.executeQuery(query.getQueryString());
    	
    	if (queryResult.length() != 1) {
    		throw new JPSRuntimeException("There should only be one input instance, consider a reset by running InitialiseInstances");
    	}
    	
    	try {
    		return queryResult.getJSONObject(0).getString(queryKey);
    	} catch (Exception e) {
    		System.out.println(e.getMessage());
    		throw new JPSRuntimeException("Input is probably not initialised yet/properly, please run InitialiseInstances");
    	}
    }
    
    public String getAverageIRI() {
    	SelectQuery query = Queries.SELECT();
    	String queryKey = "average";
    	Variable average = SparqlBuilder.var(queryKey);
    	
    	GraphPattern queryPattern = average.isA(Average);
    	
    	query.prefix(p_namespace).select(average).where(queryPattern);
    	JSONArray queryResult = storeClient.executeQuery(query.getQueryString());
    	
    	if (queryResult.length() != 1) {
    		throw new JPSRuntimeException("There should only be one average instance, consider a reset by running InitialiseInstances");
    	}
    	
    	try {
    		return queryResult.getJSONObject(0).getString(queryKey);
    	} catch (Exception e) {
    		System.out.println(e.getMessage());
    		throw new JPSRuntimeException("Average is probably not initialised yet/properly, please run InitialiseInstances");
    	}
    }
    
    /**
     * returns the CalculatedDifference instance.
     * @return
     */
    public String getCalculatedDifference() {
    	SelectQuery query = Queries.SELECT();
    	String queryKey = "derivation";
    	Variable diff = SparqlBuilder.var(queryKey);
    	
    	GraphPattern queryPattern = diff.isA(CalculatedDifference);
    	
    	query.prefix(p_namespace).select(diff).where(queryPattern);
    	
    	JSONArray queryResult = storeClient.executeQuery(query.getQueryString());
    	
    	if (queryResult.length() != 1) {
    		throw new JPSRuntimeException("There should only be one CalculatedDifference instance, consider a reset by running InitialiseInstances");
    	}
    	
    	try {
    		return queryResult.getJSONObject(0).getString(queryKey);
    	} catch (Exception e) {
    		System.out.println(e.getMessage());
    		throw new JPSRuntimeException("CalculatedDifference is probably not initialised yet/properly, please run InitialiseInstances");
    	}
    }
}
