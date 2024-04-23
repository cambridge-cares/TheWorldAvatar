package uk.ac.cam.cares.goal.example;


import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

import org.eclipse.rdf4j.model.vocabulary.OWL;
import org.eclipse.rdf4j.query.algebra.In;
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

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.interfaces.StoreClientInterface;

import static org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf.iri;


public class SparqlClient {

    StoreClientInterface storeClient;

    // namespace
    public static String namespace = "http://goalframework_example#";

    public static String prefix = "goalframework";
    private static Prefix p_namespace = SparqlBuilder.prefix(prefix, iri(namespace));

    // rdf:type
    public static Iri GoalRange = p_namespace.iri("GoalRange");

    public static Iri Input = p_namespace.iri("Input"); // has a time series instance
    public static Iri ScalarValue = p_namespace.iri("ScalarValue");

    public static Iri Truck = p_namespace.iri("Truck");

    // property
    public static Iri hasValue = p_namespace.iri("hasValue");
    public static Iri numericalValue = p_namespace.iri("numericalValue");
    public static Iri stringValue = p_namespace.iri("stringValue");

    public static Iri hasMaximum = p_namespace.iri("hasMaximum");

    public static Iri hasMinimum = p_namespace.iri("hasMinimum");


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


    public String createInputData() {

        String inputIRI = namespace + UUID.randomUUID().toString();

        ModifyQuery modify = Queries.MODIFY();
        modify.insert(iri(inputIRI).isA(Input)).prefix(p_namespace);

        // create the instance on kg
        storeClient.executeUpdate(modify.getQueryString());

        return inputIRI;
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
     * creates a new GoalRange instance
     * <iri> a <GoalRange>
     * <iri> a owl:NamedIndividual
     * @param
     * @return
     */
    public String createGoalRangeIRI() {
        String goalRange_iri = namespace + UUID.randomUUID().toString();
        ModifyQuery modify = Queries.MODIFY();
        modify.insert(iri(goalRange_iri).isA(GoalRange).andIsA(iri(OWL.NAMEDINDIVIDUAL)).andIsA(GoalRange));
        modify.insert();
        storeClient.executeUpdate(modify.prefix(p_namespace).getQueryString());
        return goalRange_iri;
    }

    /**
     * creates a new GoalRange instance
     * <iri> a <GoalRange>
     * <iri> a owl:NamedIndividual
     * @param
     * @return
     */
    public void createRangeCondition(String goalRange_iri, String Maximum, String Minimum) {
        ModifyQuery modify = Queries.MODIFY();
        modify.insert(iri(goalRange_iri).has(hasMaximum,Maximum).andHas(hasMinimum,Minimum));
        storeClient.executeUpdate(modify.prefix(p_namespace).getQueryString());
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
        modify.insert(iri(property).has(hasMaximum,iri(value_iri)));
        modify.insert(iri(value_iri).isA(ScalarValue).andHas(numericalValue,value));
        storeClient.executeUpdate(modify.prefix(p_namespace).getQueryString());
        return value_iri;
    }

    /**
     * adds a value instance to the given property
     * <property> <hasValue> <valueIRI>, <valueIRI> a <ScalarValue>, <valueIRI> <numericalValue> value
     * @param property
     * @param value
     * @return
     */
    public String addStringInstance(String property, String value) {
        String value_iri = namespace + UUID.randomUUID().toString();
        ModifyQuery modify = Queries.MODIFY();
        modify.insert(iri(property).has(hasMaximum,iri(value_iri)));
        modify.insert(iri(value_iri).isA(ScalarValue).andHas(stringValue,value));
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

        GraphPattern queryPattern = input.isA(Input);

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

    /**
     * This method generates below triples given <propertyIRI> and <valueIRI>:
     * <propertyIRI> <hasValue> <valueIRI>.
     * <valueIRI> <stringValue> value.
     *
     * @param quantityInstance
     * @param valueInstance
     * @param value
     * @return
     */
    public List<TriplePattern> addStringInstance(String quantityInstance, String valueInstance, String value) {
        List<TriplePattern> triples = new ArrayList<>();
        triples.add(iri(quantityInstance).has(iri(getPropertyString(hasMaximum)), iri(valueInstance)));
        triples.add(iri(valueInstance).has(iri(getPropertyString(stringValue)), value));
        return triples;
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
        triples.add(iri(quantityInstance).has(iri(getPropertyString(hasMaximum)), iri(valueInstance)));
        triples.add(iri(valueInstance).has(iri(getPropertyString(numericalValue)), value));
        return triples;
    }

    /**
     * This method returns the rdf:type in the string format of the given
     * object/date property.
     *
     * @param property
     * @return
     */
    public static String getPropertyString(Iri property) {
        return property.getQueryString().replaceAll(prefix + ":", namespace);
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





}
