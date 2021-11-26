package uk.ac.cam.cares.derivation.asynexample;

import static org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf.iri;

import java.util.List;
import java.util.UUID;

import org.eclipse.rdf4j.model.vocabulary.OWL;
import org.eclipse.rdf4j.sparqlbuilder.core.Prefix;
import org.eclipse.rdf4j.sparqlbuilder.core.PropertyPaths;
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
	public static String namespace = "http://derivation_asyn_example#";
	public static String prefix = "derivation_asyn_example";
	public static Prefix p_namespace = SparqlBuilder.prefix(prefix, iri(namespace));
	
	// rdf:type
	public static Iri MaxValue = p_namespace.iri("MaxValue");
	public static Iri MinValue = p_namespace.iri("MinValue");
	public static Iri Difference = p_namespace.iri("Difference");
	public static Iri ListOfRandomPoints = p_namespace.iri("ListOfRandomPoints");
	public static Iri Point = p_namespace.iri("Point");
	public static Iri UpperLimit = p_namespace.iri("UpperLimit");
	public static Iri LowerLimit = p_namespace.iri("LowerLimit");
	public static Iri NumberOfPoints = p_namespace.iri("NumberOfPoints");
	public static Iri ScalarValue = p_namespace.iri("ScalarValue");
	
	// property
	public static Iri hasPoint = p_namespace.iri("hasValue");
	public static Iri hasValue = p_namespace.iri("hasValue");
	public static Iri numericalValue = p_namespace.iri("numericalValue");
	
	public static void main(String[] args) {
		System.out.println(SparqlClient.MaxValue.getQueryString().replaceAll(prefix+":", namespace));
	}
	
	public SparqlClient(StoreClientInterface storeClient) {
		this.storeClient = storeClient;
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
    
    boolean isDifference(String instance) {
    	String query = String.format("ask {<%s> a <%s>}", instance, (namespace + "Difference"));
    	storeClient.setQuery(query);
    	boolean result = storeClient.executeQuery().getJSONObject(0).getBoolean("ASK");
    	return result;
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
    public String createDifference() {
    	String difference_iri = namespace + UUID.randomUUID().toString();    	
    	ModifyQuery modify = Queries.MODIFY();
    	modify.insert(iri(difference_iri).isA(Difference).andIsA(iri(OWL.NAMEDINDIVIDUAL)));
    	storeClient.executeUpdate(modify.prefix(p_namespace).getQueryString());
    	return difference_iri;
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
    
    public String createListOfRandomPoints(List<Integer> listOfRandomPoints) {
    	String listOfRandomPoints_iri = namespace + UUID.randomUUID().toString();    	
    	ModifyQuery modify = Queries.MODIFY();
    	modify.insert(iri(listOfRandomPoints_iri).isA(ListOfRandomPoints).andIsA(iri(OWL.NAMEDINDIVIDUAL)));
    	storeClient.executeUpdate(modify.prefix(p_namespace).getQueryString());
    	
    	for (Integer pt : listOfRandomPoints) {
    		String pt_iri = createPoint();
    		addValueInstance(pt_iri, pt);
    		addPointInstance(listOfRandomPoints_iri, pt_iri);
    	}
    	
    	return listOfRandomPoints_iri;
    }
    
    public String createPoint() {
    	String point_iri = namespace + UUID.randomUUID().toString();    	
    	ModifyQuery modify = Queries.MODIFY();
    	modify.insert(iri(point_iri).isA(Point).andIsA(iri(OWL.NAMEDINDIVIDUAL)));
    	storeClient.executeUpdate(modify.prefix(p_namespace).getQueryString());
    	return point_iri;
    }
    
    public void addPointInstance(String listOfRandomPoints_iri, String point_iri) {
    	ModifyQuery modify = Queries.MODIFY();
    	modify.insert(iri(listOfRandomPoints_iri).has(hasPoint,iri(point_iri)));
    	storeClient.executeUpdate(modify.prefix(p_namespace).getQueryString());
    }
    
    /**
     * Get the extreme (max or min) value from a list of randomly generated points.
     * query and order <listOfRandomPoints> <hasPoint>/<hasValue>/<numericalValue> ?value
     * @param listOfRandomPoints_iri
     * @param max
     * @return
     */
    public Integer getExtremeValueInList(String listOfRandomPoints_iri, boolean max) {
    	SelectQuery query = Queries.SELECT();
    	
    	String key = "value";
    	Variable value = SparqlBuilder.var(key);
    	GraphPattern queryPattern = iri(listOfRandomPoints_iri).has(PropertyPaths.path(hasPoint,hasValue,numericalValue),value);
    	
    	// construct query string with different orderBy to get either max or min value
    	if (max) {
    		query.prefix(p_namespace).select(value).where(queryPattern).orderBy(SparqlBuilder.desc(value)).limit(1);
    	} else {
        	query.prefix(p_namespace).select(value).where(queryPattern).orderBy(value).limit(1);
    	}
    	
    	JSONArray queryResult = storeClient.executeQuery(query.getQueryString());
    	
    	return queryResult.getJSONObject(0).getInt(key);
    }
}
