package uk.ac.cam.cares.jps.base.derivation;

import java.util.List;
import java.util.UUID;
import java.util.stream.Collectors;

import org.eclipse.rdf4j.sparqlbuilder.core.Variable;
import org.eclipse.rdf4j.sparqlbuilder.core.query.ModifyQuery;
import org.eclipse.rdf4j.sparqlbuilder.core.query.Queries;
import org.eclipse.rdf4j.sparqlbuilder.core.query.SelectQuery;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.TriplePattern;
import org.json.JSONArray;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.junit.jupiter.Testcontainers;

import uk.ac.cam.cares.jps.base.BlazegraphContainer;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

@Testcontainers
public class DerivationOutputsIntegrationTest {
    static RemoteStoreClient storeClient;
    static String kgUrl;
    ModifyQuery modify;

    @Container
    private static final BlazegraphContainer blazegraph = new BlazegraphContainer();

    @BeforeAll
    public static void initialise() {
        // initialise all variables to be used
        storeClient = blazegraph.getRemoteStoreClient();
    }

    @Test
    void testSparqlUpdateNumberLiteral() {
        ////////////////////////
        // test SPARQL INSERT //
        ////////////////////////
        // create a list of triples to insert
        List<TriplePattern> outputTriples = generateLiteralTriples();

        // sparql update all new generated outputTriples
        modify = Queries.MODIFY();
        outputTriples.stream().forEach(t -> modify.insert(t));
        System.out.println(modify.getQueryString());
        storeClient.executeUpdate(modify.getQueryString());

        // check that the triples were inserted
        Assertions.assertTrue(allOutputTriplesExistInKG(outputTriples));

        //////////////////////////////
        // test SPARQL INSERT-WHERE //
        //////////////////////////////
        // create a list of triples to insert
        outputTriples = generateLiteralTriples();

        // sparql update all new generated outputTriples
        SelectQuery query = Queries.SELECT();
        Variable s = query.var();
        Variable p = query.var();
        Variable o = query.var();
        modify = Queries.MODIFY();
        outputTriples.stream().forEach(t -> modify.insert(t));
        modify.where(s.has(p, o));
        System.out.println(modify.getQueryString());
        storeClient.executeUpdate(modify.getQueryString());

        // check that the triples were inserted
        Assertions.assertTrue(allOutputTriplesExistInKG(outputTriples));

        /////////////////////////////////////
        // test SPARQL DELETE-INSERT-WHERE //
        /////////////////////////////////////
        // create a list of triples to insert
        outputTriples = generateLiteralTriples();

        // sparql update all new generated outputTriples
        query = Queries.SELECT();
        s = query.var();
        p = query.var();
        o = query.var();
        modify = Queries.MODIFY();
        outputTriples.stream().forEach(t -> modify.insert(t));
        modify.delete(s.has(p, o)).where(s.has(p, o));
        System.out.println(modify.getQueryString());
        storeClient.executeUpdate(modify.getQueryString());

        // check that the triples were inserted
        Assertions.assertTrue(allOutputTriplesExistInKG(outputTriples));
    }

    private List<TriplePattern> generateLiteralTriples() {
        DerivationOutputs derivationOutputs = new DerivationOutputs();

        String s1 = "http://" + UUID.randomUUID().toString();
        String p1 = "http://" + UUID.randomUUID().toString();
        Double o1 = Double.NaN; // add NaN

        String s2 = "http://" + UUID.randomUUID().toString();
        String p2 = "http://" + UUID.randomUUID().toString();
        Double o2 = Double.POSITIVE_INFINITY; // add positive infinity INF

        String s3 = "http://" + UUID.randomUUID().toString();
        String p3 = "http://" + UUID.randomUUID().toString();
        Double o3 = Double.NEGATIVE_INFINITY; // add negative infinity -INF

        String s4 = "http://" + UUID.randomUUID().toString();
        String p4 = "http://" + UUID.randomUUID().toString();
        Double o4 = 2.3; // add a normal number

        derivationOutputs.addLiteral(s1, p1, o1);
        derivationOutputs.addLiteral(s2, p2, o2);
        derivationOutputs.addLiteral(s3, p3, o3);
        derivationOutputs.addLiteral(s4, p4, o4);

        return derivationOutputs.getOutputTriples();
    }

    private boolean allOutputTriplesExistInKG(List<TriplePattern> outputTriples) {
        // check if all outputTriples exist in the triple store
        String queryString = "ASK { "
                + outputTriples.stream().map(t -> t.getQueryString()).collect(Collectors.joining("")) + " }";
        JSONArray queryResult = storeClient.executeQuery(queryString);
        System.out.println(queryResult);
        return queryResult.getJSONObject(0).getBoolean("ASK");
    }
}
