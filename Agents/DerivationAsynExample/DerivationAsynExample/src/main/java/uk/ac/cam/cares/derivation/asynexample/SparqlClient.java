package uk.ac.cam.cares.derivation.asynexample;

import static org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf.iri;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
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
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.TriplePattern;
import org.eclipse.rdf4j.sparqlbuilder.rdf.Iri;
import org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf;
import org.json.JSONArray;

import uk.ac.cam.cares.jps.base.derivation.DerivationSparql;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
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
	public static Iri DifferenceReverse = p_namespace.iri("DifferenceReverse");
	public static Iri ListOfRandomPoints = p_namespace.iri("ListOfRandomPoints");
	public static Iri Point = p_namespace.iri("Point");
	public static Iri UpperLimit = p_namespace.iri("UpperLimit");
	public static Iri LowerLimit = p_namespace.iri("LowerLimit");
	public static Iri NumberOfPoints = p_namespace.iri("NumberOfPoints");
	public static Iri ScalarValue = p_namespace.iri("ScalarValue");
	public static Iri InputPlaceholderExceptionThrow = p_namespace.iri("InputPlaceholderExceptionThrow");
	public static Iri OutputPlaceholderExceptionThrow = p_namespace.iri("OutputPlaceholderExceptionThrow");

	// property
	public static Iri hasPoint = p_namespace.iri("hasPoint");
	public static Iri hasValue = p_namespace.iri("hasValue");
	public static Iri numericalValue = p_namespace.iri("numericalValue");
	
	// OntoAgent related
	public static Prefix p_agent = SparqlBuilder.prefix("agent",iri("http://www.theworldavatar.com/ontology/ontoagent/MSM.owl#"));
	public static Iri Service = p_agent.iri("Service");
	public static Iri Operation = p_agent.iri("Operation");
	public static Iri MessageContent = p_agent.iri("MessageContent");
	public static Iri MessagePart = p_agent.iri("MessagePart");
	
	public static Iri hasHttpUrl = p_agent.iri("hasHttpUrl");
	public static Iri hasOperation = p_agent.iri("hasOperation");
	public static Iri hasInput = p_agent.iri("hasInput");
	public static Iri hasOutput = p_agent.iri("hasOutput");
	public static Iri hasMandatoryPart = p_agent.iri("hasMandatoryPart");
	public static Iri hasType = p_agent.iri("hasType");
	public static Iri hasName = p_agent.iri("hasName");
	
	// derivation realted
	public static Prefix p_derivation = SparqlBuilder.prefix("derivation",
			iri(DerivationSparql.derivednamespace));
	public static Iri belongsTo = p_derivation.iri("belongsTo");

	public SparqlClient(StoreClientInterface storeClient) {
		this.storeClient = storeClient;
	}
	
	/**
	 * This method returns the rdf:type in the string format of the given class.
	 * 
	 * @param clz
	 * @return
	 */
	public static String getRdfTypeString(Iri clz) {
		return clz.getQueryString().replaceAll(prefix + ":", namespace);
	}

	/**
	 * This method returns the rdf:type in the string format of the given
	 * object/date property.
	 */
	public static String getPropertyString(Iri property) {
		return property.getQueryString().replaceAll(prefix + ":", namespace);
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
    
    /**
     * This method updates the numerical value of a value instance to the given numerical value.
     * update value in <property> <hasValue> <valueIRI>, <valueIRI> a <ScalarValue>, <valueIRI> <numericalValue> value
     * @param property
     * @param value
     */
    public void updateValue(String property, int value) {
    	SelectQuery query = Queries.SELECT();
    	
    	String valueKey = "value";
    	String numvalKey = "numValue";
    	Variable value_iri = SparqlBuilder.var(valueKey);
    	Variable numVal_iri = SparqlBuilder.var(numvalKey);
    	GraphPattern queryPattern = GraphPatterns.and(iri(property).has(hasValue,value_iri), value_iri.has(numericalValue,numVal_iri));
    	
    	query.prefix(p_namespace).select(value_iri,numVal_iri).where(queryPattern);
    	
    	JSONArray queryResult = storeClient.executeQuery(query.getQueryString());
    	
    	ModifyQuery modify = Queries.MODIFY();
    	modify.delete(iri(queryResult.getJSONObject(0).getString(valueKey)).has(numericalValue,queryResult.getJSONObject(0).getInt(numvalKey)));
    	modify.insert(iri(queryResult.getJSONObject(0).getString(valueKey)).has(numericalValue,value));
    	
    	storeClient.executeUpdate(modify.prefix(p_namespace).getQueryString());
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
     * <iri> a <Difference>
     * <iri> a owl:NamedIndividual
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
     * This method creates a UpperLimit instance. 
     * <iri> a <UpperLimit>
     * <iri> a owl:NamedIndividual
     * @return
     */
    public String createUpperLimit() {
    	String upperlimit_iri = namespace + UUID.randomUUID().toString();    	
    	ModifyQuery modify = Queries.MODIFY();
    	modify.insert(iri(upperlimit_iri).isA(UpperLimit).andIsA(iri(OWL.NAMEDINDIVIDUAL)));
    	storeClient.executeUpdate(modify.prefix(p_namespace).getQueryString());
    	return upperlimit_iri;
    }
    
    /**
     * This method creates a LowerLimit instance. 
     * <iri> a <LowerLimit>
     * <iri> a owl:NamedIndividual
     * @return
     */
    public String createLowerLimit() {
    	String lowerlimit_iri = namespace + UUID.randomUUID().toString();    	
    	ModifyQuery modify = Queries.MODIFY();
    	modify.insert(iri(lowerlimit_iri).isA(LowerLimit).andIsA(iri(OWL.NAMEDINDIVIDUAL)));
    	storeClient.executeUpdate(modify.prefix(p_namespace).getQueryString());
    	return lowerlimit_iri;
    }
    
    /**
     * This method creates a NumberOfPoints instance. 
     * <iri> a <NumberOfPoints>
     * <iri> a owl:NamedIndividual
     * @return
     */
    public String createNumberOfPoints() {
    	String numberOfPoints_iri = namespace + UUID.randomUUID().toString();    	
    	ModifyQuery modify = Queries.MODIFY();
    	modify.insert(iri(numberOfPoints_iri).isA(NumberOfPoints).andIsA(iri(OWL.NAMEDINDIVIDUAL)));
    	storeClient.executeUpdate(modify.prefix(p_namespace).getQueryString());
    	return numberOfPoints_iri;
    }

	/**
	 * This method creates a InputPlaceholderExceptionThrow instance.
	 * <iri> a <InputPlaceholderExceptionThrow>
	 * <iri> a owl:NamedIndividual
	 * @return
	 */
	public String createInputPlaceholderExceptionThrow() {
		String inputPlaceholderExceptionThrowIRI = namespace + UUID.randomUUID().toString();
		ModifyQuery modify = Queries.MODIFY();
		modify.insert(iri(inputPlaceholderExceptionThrowIRI).isA(InputPlaceholderExceptionThrow).andIsA(iri(OWL.NAMEDINDIVIDUAL)));
		storeClient.executeUpdate(modify.prefix(p_namespace).getQueryString());
		return inputPlaceholderExceptionThrowIRI;
	}

    /**
     * This method queries ?x a <UpperLimit>.
     * @return
     */
    public String getUpperLimitIRI() {
    	SelectQuery query = Queries.SELECT();
    	
    	String key = "upperlimit";
    	Variable ul_iri = SparqlBuilder.var(key);
    	GraphPattern queryPattern = ul_iri.isA(UpperLimit);
    	
    	query.prefix(p_namespace).select(ul_iri).where(queryPattern);
    	
    	JSONArray queryResult = storeClient.executeQuery(query.getQueryString());
    	
    	if (queryResult.length() != 1) {
    		throw new JPSRuntimeException("There should only be one UpperLimit instance, consider a reset by running InitialiseInstances");
    	}
    	
    	try {
    		return queryResult.getJSONObject(0).getString(key);
    	} catch (Exception e) {
    		System.out.println(e.getMessage());
    		throw new JPSRuntimeException("UpperLimit is probably not initialised yet/properly, please run InitialiseInstances");
    	}
    }
    
    /**
     * This method queries ?x a <LowerLimit>.
     * @return
     */
    public String getLowerLimitIRI() {
    	SelectQuery query = Queries.SELECT();
    	
    	String key = "lowerlimit";
    	Variable ul_iri = SparqlBuilder.var(key);
    	GraphPattern queryPattern = ul_iri.isA(LowerLimit);
    	
    	query.prefix(p_namespace).select(ul_iri).where(queryPattern);
    	
    	JSONArray queryResult = storeClient.executeQuery(query.getQueryString());
    	
    	if (queryResult.length() != 1) {
    		throw new JPSRuntimeException("There should only be one LowerLimit instance, consider a reset by running InitialiseInstances");
    	}
    	
    	try {
    		return queryResult.getJSONObject(0).getString(key);
    	} catch (Exception e) {
    		System.out.println(e.getMessage());
    		throw new JPSRuntimeException("LowerLimit is probably not initialised yet/properly, please run InitialiseInstances");
    	}
    }
    
    /**
     * This method queries ?x a <NumberOfPoints>.
     * @return
     */
    public String getNumberOfPointsIRI() {
    	SelectQuery query = Queries.SELECT();
    	
    	String key = "numberofpoints";
    	Variable ul_iri = SparqlBuilder.var(key);
    	GraphPattern queryPattern = ul_iri.isA(NumberOfPoints);
    	
    	query.prefix(p_namespace).select(ul_iri).where(queryPattern);
    	
    	JSONArray queryResult = storeClient.executeQuery(query.getQueryString());
    	
    	if (queryResult.length() != 1) {
    		throw new JPSRuntimeException("There should only be one NumberOfPoints instance, consider a reset by running InitialiseInstances");
    	}
    	
    	try {
    		return queryResult.getJSONObject(0).getString(key);
    	} catch (Exception e) {
    		System.out.println(e.getMessage());
    		throw new JPSRuntimeException("NumberOfPoints is probably not initialised yet/properly, please run InitialiseInstances");
    	}
    }
    
    /**
     * This method queries ?x a <Difference>.
     * @return
     */
    public String getDifferenceIRI() {
    	SelectQuery query = Queries.SELECT();
    	
    	String key = "difference";
    	Variable ul_iri = SparqlBuilder.var(key);
    	GraphPattern queryPattern = ul_iri.isA(Difference);
    	
    	query.prefix(p_namespace).select(ul_iri).where(queryPattern);
    	
    	JSONArray queryResult = storeClient.executeQuery(query.getQueryString());
    	
		if (queryResult.length() > 1) {
    		throw new JPSRuntimeException("There should be at MOST ONE Difference instance, consider a reset by running InitialiseInstances");
    	} else if (queryResult.length() == 1) {
			return queryResult.getJSONObject(0).getString(key);
		} else {
			return new String();
		}
    }

	/**
	 * This method queries ?x a <DifferenceReverse>, ?x <hasValue> ?v, ?v <numericalValue> ?value.
	 * @return
	 */
	 public Map<String, Integer> getDiffReverseValues() {
		SelectQuery query = Queries.SELECT();

		String diffReverseKey = "diffReverse";
		String valueKey = "value";
		Variable ul_iri = SparqlBuilder.var(diffReverseKey);
		Variable value_iri = query.var();
		Variable value = SparqlBuilder.var(valueKey);
		GraphPattern queryPattern = GraphPatterns.and(
			ul_iri.isA(DifferenceReverse).andHas(hasValue,value_iri),
			value_iri.has(numericalValue,value));

		query.prefix(p_namespace).select(ul_iri, value).where(queryPattern);

		JSONArray queryResult = storeClient.executeQuery(query.getQueryString());

		Map<String, Integer> diffReverseValues = new HashMap<>();
		for (int i = 0; i < queryResult.length(); i++) {
			diffReverseValues.put(queryResult.getJSONObject(i).getString(diffReverseKey), queryResult.getJSONObject(i).getInt(valueKey));
		}

		return diffReverseValues;
	}

	/**
     * This method queries ?x a <ListOfRandomPoints>.
     * @return
     */
    public String getListOfRandomPointsIRI() {
    	SelectQuery query = Queries.SELECT();
    	
    	String key = "listofrandompoints";
    	Variable ul_iri = SparqlBuilder.var(key);
		Variable derivation = SparqlBuilder.var("derivation");
		GraphPattern queryPattern = ul_iri.isA(ListOfRandomPoints).andHas(belongsTo, derivation);
    	
		query.prefix(p_namespace, p_derivation).select(ul_iri).where(queryPattern);
    	
    	JSONArray queryResult = storeClient.executeQuery(query.getQueryString());
    	
    	if (queryResult.length() > 1) {
    		throw new JPSRuntimeException("There should be at MOST ONE ListOfRandomPoints instance, consider a reset by running InitialiseInstances");
    	} else if (queryResult.length() == 1) {
			return queryResult.getJSONObject(0).getString(key);
		} else {
			return new String();
		}
    }
	
	/**
     * This method queries ?x a <MaxValue>.
     * @return
     */
    public String getMaxValueIRI() {
    	SelectQuery query = Queries.SELECT();
    	
    	String key = "maxvalue";
    	Variable ul_iri = SparqlBuilder.var(key);
    	GraphPattern queryPattern = ul_iri.isA(MaxValue);
    	
    	query.prefix(p_namespace).select(ul_iri).where(queryPattern);
    	
    	JSONArray queryResult = storeClient.executeQuery(query.getQueryString());
    	
    	if (queryResult.length() > 1) {
    		throw new JPSRuntimeException("There should be at MOST ONE MaxValue instance, consider a reset by running InitialiseInstances");
    	} else if (queryResult.length() == 1) {
			return queryResult.getJSONObject(0).getString(key);
		} else {
			return new String();
		}
    }

	/**
     * This method queries ?x a <MinValue>.
     * @return
     */
    public String getMinValueIRI() {
    	SelectQuery query = Queries.SELECT();
    	
    	String key = "minvalue";
    	Variable ul_iri = SparqlBuilder.var(key);
    	GraphPattern queryPattern = ul_iri.isA(MinValue);
    	
    	query.prefix(p_namespace).select(ul_iri).where(queryPattern);
    	
    	JSONArray queryResult = storeClient.executeQuery(query.getQueryString());
    	
		if (queryResult.length() > 1) {
    		throw new JPSRuntimeException("There should be at MOST ONE MinValue instance, consider a reset by running InitialiseInstances");
    	} else if (queryResult.length() == 1) {
			return queryResult.getJSONObject(0).getString(key);
		} else {
			return new String();
		}
    }

	/**
	 * This method counts the number of ?pt in below triples
	 * ?list a <ListOfRandomPoints>.
	 * ?list <hasPoint> ?pt.
	 * ?list <belongsTo> ?derivation.
	 * 
	 * @return
	 */
	public int getAmountOfPointsInList() {
		SelectQuery query = Queries.SELECT();

		String listKey = "list";
		String pointKey = "pt";
		
		Variable lst = SparqlBuilder.var(listKey);
		Variable pt = SparqlBuilder.var(pointKey);
		Variable derivation = SparqlBuilder.var("derivation");
		GraphPattern queryPattern = lst.isA(ListOfRandomPoints).andHas(hasPoint, pt).andHas(belongsTo, derivation);

		query.prefix(p_namespace, p_derivation).select(pt).where(queryPattern);

		JSONArray queryResult = storeClient.executeQuery(query.getQueryString());

		return queryResult.length();
	}

	/**
	 * This method counts the number of ?pt in the whole knowledge graph
	 * ?pt a <Point>
	 * 
	 */
	public int getAmountOfPointsInKG() {
		SelectQuery query = Queries.SELECT();

		String pointKey = "pt";

		Variable pt = SparqlBuilder.var(pointKey);
		GraphPattern queryPattern = pt.isA(Point);

		query.prefix(p_namespace).select(pt).where(queryPattern);

		JSONArray queryResult = storeClient.executeQuery(query.getQueryString());

		return queryResult.length();
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
	 * This method generates below triples given <propertyIRI> and <valueIRI>:
	 * <propertyIRI> <hasValue> <valueIRI>.
	 * <valueIRI> <numericalValue> value.
	 * 
	 * @param quantityInstance
	 * @param valueInstance
	 * @param value
	 * @return
	 */
	public List<TriplePattern> addValueInstance(String quantityInstance, String valueInstance, int value) {
		List<TriplePattern> triples = new ArrayList<>();
		triples.add(iri(quantityInstance).has(iri(getPropertyString(hasValue)), iri(valueInstance)));
		triples.add(iri(valueInstance).has(iri(getPropertyString(numericalValue)), value));
		return triples;
	}

    /**
     * This method creates a ListOfRandomPoints instance given a list of value.
     * <iri> a <ListOfRandomPoints>,
     * <iri> a owl:NamedIndividual,
     * <iri> <hasPoint> <pointIRI>,
     * <pointIRI> a <Point>,
     * <pointIRI> <hasValue> <valueIRI>,
     * <valueIRI> a <ScalarValue>, 
     * <valueIRI> <numericalValue> value
     * @param listOfRandomPoints
     * @return
     */
	public List<String> createListOfRandomPoints(String listOfRandomPoints_iri, List<Integer> listOfRandomPoints) {
		List<String> iris = new ArrayList<>();
		iris.add(listOfRandomPoints_iri);
    	ModifyQuery modify = Queries.MODIFY();
    	modify.insert(iri(listOfRandomPoints_iri).isA(ListOfRandomPoints).andIsA(iri(OWL.NAMEDINDIVIDUAL)));
    	storeClient.executeUpdate(modify.prefix(p_namespace).getQueryString());
    	
    	if (listOfRandomPoints != null) {
        	for (Integer pt : listOfRandomPoints) {
        		String pt_iri = createPoint();
				iris.add(pt_iri);
				String value_iri = addValueInstance(pt_iri, pt);
				iris.add(value_iri);
        		addPointInstance(listOfRandomPoints_iri, pt_iri);
        	}
    	}
    	
		return iris;
	}

	/**
	 * This method generates below triples:
	 * <lstRandPtsIRI> <hasPoint> <pt_n>.
	 * <pt_n> <hasValue> <valueIRI_n>.
	 * <valueIRI_n> <numericalValue> value_n.
	 * 
	 * Note that all these triples will be repeated n times depend on the size of
	 * map ptIRIs.
	 * 
	 * @param lstRandPtsIRI
	 * @param ptIRIs
	 * @param valuesMap
	 * @return
	 */
	public List<TriplePattern> createListOfRandomPoints(String lstRandPtsIRI, Map<String, String> ptIRIs,
			Map<String, Integer> valuesMap) {
		List<TriplePattern> triples = new ArrayList<>();
		if (ptIRIs.values().stream().allMatch(valIri -> valuesMap.containsKey(valIri))) {
			ptIRIs.forEach((ptIri, valIri) -> {
				triples.add(Rdf.iri(lstRandPtsIRI).has(Rdf.iri(getPropertyString(hasPoint)), Rdf.iri(ptIri)));
				triples.addAll(addValueInstance(ptIri, valIri, valuesMap.get(valIri)));
			});
		} else {
			throw new JPSRuntimeException(
					"The provided valuesMap is incomplete that it doesn't contain all IRIs appeared in ptIRIs. valuesMap: "
							+ valuesMap + "; ptIRIs: " + ptIRIs);
		}
		return triples;
	}
    
    /**
     * This method creates a Point instance. 
     * <iri> a <Point>
     * <iri> a owl:NamedIndividual
     * @return
     */
    public String createPoint() {
    	String point_iri = namespace + UUID.randomUUID().toString();    	
    	ModifyQuery modify = Queries.MODIFY();
    	modify.insert(iri(point_iri).isA(Point).andIsA(iri(OWL.NAMEDINDIVIDUAL)));
    	storeClient.executeUpdate(modify.prefix(p_namespace).getQueryString());
    	return point_iri;
    }
    
    /**
     * This method links point_iri to the listOfRandomPoints_iri it belongs to.
     * @param listOfRandomPoints_iri
     * @param point_iri
     */
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
    public int getExtremeValueInList(String listOfRandomPoints_iri, boolean max) {
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
