package uk.ac.cam.cares.jps.base.query.fedq;

import org.apache.jena.query.Query;
import org.apache.jena.query.QueryExecution;
import org.apache.jena.query.QueryExecutionFactory;
import org.apache.jena.query.QueryFactory;
import org.apache.jena.sparql.algebra.Op;
import org.apache.jena.sparql.algebra.OpWalker;
import org.apache.jena.sparql.algebra.OpVisitorBase;
import org.apache.jena.sparql.algebra.op.OpBGP;
import org.apache.jena.sparql.core.BasicPattern;
import org.eclipse.rdf4j.federated.FedXFactory;
import org.eclipse.rdf4j.federated.endpoint.Endpoint;
import org.eclipse.rdf4j.federated.endpoint.EndpointFactory;
import org.eclipse.rdf4j.federated.repository.FedXRepository;
import org.eclipse.rdf4j.query.BindingSet;
import org.eclipse.rdf4j.query.TupleQuery;
import org.eclipse.rdf4j.query.TupleQueryResult;
import org.eclipse.rdf4j.repository.RepositoryConnection;

import com.google.gson.Gson;
import com.google.gson.reflect.TypeToken;
import java.lang.reflect.Type;

import org.apache.jena.graph.Node;
import org.apache.jena.graph.Triple;

import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ModelFactory;

import java.io.*;
import java.util.*;

/**
 * The class wraps some functionality to-
 * 1. analyse query to extract relevant classes and properties
 * 2. load inverted index from files data into memory maps
 * 3. find out endpoints from the classes and properties
 * 4. process SPARQL query against the endpoints using FedX API
 */

public class ProcessQuery {

    private String classIndexFilePath = "";
    private String propertyIndexFilePath = "";
    private String cpIndexFilePath = "";
    private Set<Node> classes = new HashSet<>();
    private Set<Node> properties = new HashSet<>();
    private Map<String, Set<String>> classIndex = new HashMap<>();
    private Map<String, Set<String>> propertyIndex = new HashMap<>();
    private Map<String, Set<String>> cpIndex = new HashMap<>();

    /**
     * It extracts classes and properties from the user query
     * 
     * @return
     */
    public void extractClassesAndProperties(String sparqlQuery) {
        Query query = QueryFactory.create(sparqlQuery);
        Op queryObject = org.apache.jena.sparql.algebra.Algebra.compile(query);

        OpWalker.walk(queryObject, new OpVisitorBase() {
            @Override
            public void visit(OpBGP opBGP) {
                BasicPattern triples = opBGP.getPattern();
                for (Triple triple : triples) {
                    Node subject = triple.getSubject();
                    Node predicate = triple.getPredicate();
                    Node object = triple.getObject();
                    if (subject.isURI())
                        classes.add(subject);
                    if (predicate.isURI())
                        properties.add(predicate);
                    if (object.isURI() && object.getURI().startsWith("http"))
                        classes.add(object);
                }
            }
        });
    }

    /**
     * it loads indices from initialised directory
     * 
     * @return
     */
    public void loadIndices(String indexDir) throws IOException {
        if (indexDir.trim().endsWith("/")) {
            this.classIndexFilePath = indexDir.trim() + "cinv.indx";
            this.propertyIndexFilePath = indexDir.trim() + "pinv.indx";
            this.cpIndexFilePath = indexDir.trim() + "cpinv.indx";
        } else {
            this.classIndexFilePath = indexDir.trim() + "/cinv.indx";
            this.propertyIndexFilePath = indexDir.trim() + "/pinv.indx";
            this.cpIndexFilePath = indexDir.trim() + "/cpinv.indx";
        }

        this.classIndex = loadIndexFromFile(this.classIndexFilePath);
        this.propertyIndex = loadIndexFromFile(this.propertyIndexFilePath);
        // this.cpIndex = loadIndexFromFile(this.cpIndexFilePath);
        // System.out.printf("%d\n%d\n",this.classIndex.size(),this.propertyIndex.size());
    }

    /**
     * it loads key-to-endpoint index from initialised file
     * 
     * @return
     */
    private Map<String, Set<String>> loadIndexFromFile(String filePath) throws IOException {
        Map<String, Set<String>> index = new HashMap<>();
        Gson gson = new Gson();
        Type mapType = new TypeToken<HashMap<String, Set<String>>>() {
        }.getType();
        try (FileReader reader = new FileReader(filePath)) {
            index = gson.fromJson(reader, mapType);
        }
        return index;
    }

    /**
     * it it finds endpoints from the extracted classes and properties
     * 
     * @return
     */
    public Set<String> getEndpoints() {
        Set<String> endpoints = new HashSet<>();
        for (Node classUriRef : classes) {
            String classUri = classUriRef.getURI();
            if (classIndex.containsKey(classUri)) {
                // System.out.println("Found class alignment: " + classUri);
                endpoints.addAll(classIndex.get(classUri));
            }
            // else {
            // System.out.println("Un-aligned class: " + classUri);
            // }
        }
        for (Node propertyUriRef : properties) {
            String propertyUri = propertyUriRef.getURI();
            if (propertyIndex.containsKey(propertyUri)) {
                // System.out.println("Found property alignment: " + propertyUri);
                endpoints.addAll(propertyIndex.get(propertyUri));
            }
            // else {
            // System.out.println("Un-aligned property: " + propertyUri);
            // }
        }
        // System.out.println("The Final Endpoints: ");
        // System.out.println(endpoints);

        return endpoints;
    }

    /**
     * it processes SPARQL against endpoint-set to retrieve the final result usinf
     * FedX API
     * 
     * @param endpoint_set
     * @param query
     * @return
     */
    public Set<BindingSet> executeQuery(Set<String> endpoint_set, String sparqlQuery) {
        int counter = 0;
        List<Endpoint> endpoints = new ArrayList<>();
        Set<BindingSet> result = new HashSet<>();

        for (String element : endpoint_set) {
            endpoints.add(EndpointFactory.loadSPARQLEndpoint("namesspace_" + counter++, element));
        }
        FedXRepository repository = FedXFactory.createFederation(endpoints);

        try (RepositoryConnection conn = repository.getConnection()) {

            TupleQuery tq = conn.prepareTupleQuery(sparqlQuery);
            try (TupleQueryResult tqRes = tq.evaluate()) {

                while (tqRes.hasNext()) {
                    BindingSet b = tqRes.next();
                    result.add(b);
                }
            }
        }

        repository.shutDown();
        return result;
    }

    public static void main(String[] args) throws IOException {
        // String sparqlQuery = """
        // PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
        // PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
        // PREFIX ontokin: <http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl#>

        // SELECT DISTINCT ?MechanismName
        // WHERE {
        // ?MechanismIRI rdf:type ontokin:ReactionMechanism .
        // ?MechanismIRI rdfs:label ?MechanismName .
        // }
        // """;

        String sparqlQuery = """
                PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
                PREFIX owl: <http://www.w3.org/2002/07/owl#>
                PREFIX pt: <http://www.daml.org/2003/01/periodictable/PeriodicTable.owl#>
                PREFIX OntoKin: <http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl#>
                PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

                SELECT DISTINCT ?identifier ?atomicMass ?atomicMassUnits
                WHERE {
                ?element1 rdf:type pt:Element .
                BIND(STRAFTER(STR(?element1), \"#\") AS ?identifier)
                ?element2 rdf:type OntoKin:Element .
                ?element2 rdfs:label ?identifier1 .
                ?element2 OntoKin:hasAtomicMass ?atomicMass .
                ?element2 OntoKin:hasAtomicMassUnits ?atomicMassUnits .
                FILTER(?identifier = ?identifier1)
                }
                """;

        // String sparqlQuery = """
        // PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
        // PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
        // PREFIX pt: <http://www.daml.org/2003/01/periodictable/PeriodicTable.owl#>
        // PREFIX OntoKin: <http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl#>

        // SELECT DISTINCT ?identifier ?atomicMass ?atomicMassUnits
        // WHERE {
        // SERVICE
        // <http://localhost:8080/blazegraph/namespace/namespace_compchem/sparql> {
        // ?element1 rdf:type pt:Element .
        // BIND(STRAFTER(STR(?element1), "#") AS ?identifier)
        // }
        // SERVICE <http://localhost:8080/blazegraph/namespace/namespace_kin/sparql> {
        // ?element2 rdf:type OntoKin:Element .
        // ?element2 rdfs:label ?identifier1 .
        // ?element2 OntoKin:hasAtomicMass ?atomicMass .
        // ?element2 OntoKin:hasAtomicMassUnits ?atomicMassUnits .
        // FILTER(?identifier = ?identifier1)
        // }
        // }
        // """;

        ProcessQuery ana = new ProcessQuery();
        String indexLocation = "C:/Users/printer_admin/Downloads/KGs/tests/";
        ana.loadIndices(indexLocation);

        int iteration = 600;
        int lower_index = 32;
        int upper_index = 40;

        String endpoints[] = { "http://localhost:8080/blazegraph/namespace/namespace_kin/sparql",
                "http://localhost:8080/blazegraph/namespace/namespace_compchem/sparql",
                "http://localhost:8080/blazegraph/namespace/namespace_automotive/sparql",
                "http://localhost:8080/blazegraph/namespace/namespace_species/sparql",
                "http://localhost:8080/blazegraph/namespace/namespace_uken/sparql",
                "http://localhost:8080/blazegraph/namespace/uken_1m/sparql",
                "http://localhost:8080/blazegraph/namespace/uken_10m/sparql",
                "http://localhost:8080/blazegraph/namespace/uken_200m/sparql"
        };
        String experiments[] = { "0", "01", "012", "0123", "01234", "012345", "0123456", "01234567",
                "1", "12", "123", "1234", "12345", "123456", "1234567", "12345670",
                "2", "23", "234", "2345", "23456", "234567", "2345670", "23456701",
                "3", "34", "345", "3456", "34567", "345670", "3456701", "34567012",
                "4", "45", "456", "4567", "45670", "456701", "4567012", "45670123",
                "5", "56", "567", "5670", "56701", "567012", "5670123", "56701234",
                "6", "67", "670", "6701", "67012", "670123", "6701234", "67012345",
                "7", "70", "701", "7012", "70123", "701234", "7012345", "70123456" };

        double[][] results = new double[64][3];
        for (int expt = lower_index; expt < upper_index; expt++) {
            // init result
            for (int k = 0; k < 3; k++) {
                results[expt][k] = 0;
            }

            long totatl_duration = 0;
            long first_duration = 0;

            for (int i = 0; i < iteration; i++) {
                long startTime = System.nanoTime();

                Set<String> eps = new HashSet<>();

                // run against extracted endpoints
                // ana.extractClassesAndProperties(sparqlQuery);
                // eps = ana.getEndpoints();

                // run against manual endpoints
                // eps.add("http://localhost:8080/blazegraph/namespace/namespace_all/sparql");
                // eps.add("http://localhost:8080/blazegraph/namespace/namespace_automotive/sparql");
                // eps.add("http://localhost:8080/blazegraph/namespace/namespace_compchem/sparql");
                // eps.add("http://localhost:8080/blazegraph/namespace/namespace_kin/sparql");
                // eps.add("http://localhost:8080/blazegraph/namespace/namespace_species/sparql");
                // eps.add("http://localhost:8080/blazegraph/namespace/namespace_uken/sparql");
                // eps.add("http://localhost:8080/blazegraph/namespace/uken_10m/sparql");
                // eps.add("http://localhost:8080/blazegraph/namespace/uken_1m/sparql");
                // eps.add("http://localhost:8080/blazegraph/namespace/uken_200m/sparql");
                // select endpoints
                int nos = experiments[expt].length();
                for (int index = 0; index < nos; index++) {
                    char charDigit = experiments[expt].charAt(index);
                    int ep_index = Character.getNumericValue(charDigit);
                    eps.add(endpoints[ep_index]);
                }

                Set<BindingSet> result = ana.executeQuery(eps, sparqlQuery);
                long endTime = System.nanoTime();
                long duration = (endTime - startTime) / 1000000;
                if (i == 0) {
                    first_duration = duration;
                    for (BindingSet element : result) {
                        System.out.println(element);
                    }
                    System.out.println("Against " + eps.size() + " endpoints");
                }
                totatl_duration += duration;
                // System.out.print(i + "\t");
            }

            results[expt][0] = (double) first_duration;
            results[expt][1] = totatl_duration / (double) iteration;
            results[expt][2] = (totatl_duration - first_duration) / ((double) iteration - 1.0);

            System.out.println("Experiment " + (expt + 1) + " finished: [" + results[expt][0] + ", " + results[expt][1]
                    + ", " + results[expt][2] + "]");
            // System.out.println("First execution time: " + first_duration + "
            // millisecond.");
            // System.out
            // .println("Processing Time including first iteration: " + totatl_duration /
            // iteration + " millisecond");
            // System.out.println("Processing Time without first iteration: "
            // + (totatl_duration - first_duration) / (iteration - 1) + " millisecond");
        }

        for (int expt = lower_index; expt < upper_index; expt++) {
            for (int k = 0; k < 3; k++) {
                System.out.print(results[expt][k] + "\t");
            }
            System.out.println();
        }
    }

}
