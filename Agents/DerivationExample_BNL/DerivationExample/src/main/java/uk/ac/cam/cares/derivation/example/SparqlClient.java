package uk.ac.cam.cares.derivation.example;


import static org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf.iri;

import java.util.ArrayList;
import java.util.List;
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

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.interfaces.StoreClientInterface;


public class SparqlClient {

    StoreClientInterface storeClient;

    // namespace
    public static String namespace = "http://derivation_example#";
    public static String prefix = "derivation_example";
    private static Prefix p_namespace = SparqlBuilder.prefix(prefix, iri(namespace));

    public static Iri InputData = p_namespace.iri("InputData"); // has a time series instance

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
        modify.insert(iri(inputIRI).isA(InputData)).prefix(p_namespace);

        // create the instance on kg
        storeClient.executeUpdate(modify.getQueryString());

        return inputIRI;
    }


}
