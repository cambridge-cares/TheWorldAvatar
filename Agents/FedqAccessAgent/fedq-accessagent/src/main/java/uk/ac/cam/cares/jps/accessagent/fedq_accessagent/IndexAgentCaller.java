package uk.ac.cam.cares.jps.accessagent.fedq_accessagent;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import org.apache.jena.sparql.algebra.Op;
import org.apache.jena.sparql.algebra.OpWalker;
import org.apache.jena.sparql.algebra.OpVisitorBase;
import org.apache.jena.sparql.algebra.op.OpBGP;
import org.apache.jena.sparql.core.BasicPattern;

import org.apache.jena.graph.Node;
import org.apache.jena.graph.Triple;

import org.apache.jena.query.Query;
import org.apache.jena.query.QueryFactory;
import org.apache.jena.query.QueryParseException;
import org.apache.jena.update.UpdateAction;
import org.apache.jena.update.UpdateFactory;
import org.apache.jena.update.UpdateRequest;

import org.eclipse.rdf4j.federated.FedXFactory;
import org.eclipse.rdf4j.federated.endpoint.Endpoint;
import org.eclipse.rdf4j.federated.endpoint.EndpointFactory;
import org.eclipse.rdf4j.federated.repository.FedXRepository;
import org.eclipse.rdf4j.query.BindingSet;
import org.eclipse.rdf4j.query.TupleQuery;
import org.eclipse.rdf4j.query.TupleQueryResult;
import org.eclipse.rdf4j.repository.RepositoryConnection;

import java.util.Set;
import java.util.HashSet;
import java.util.List;
import java.util.ArrayList;

import java.io.IOException;

@RestController
@RequestMapping("/api/index")
public class IndexAgentCaller {

    @Autowired
    private IndexAgent indexAgent;
    private List<String> stop_cps;

    public IndexAgentCaller(){
        stop_cps = List.of(
            "http://www.w3.org/2002/07/owl#Ontology",
            "http://www.w3.org/2002/07/owl#Class",
            "http://www.w3.org/2002/07/owl#NamedIndividual",
            "http://www.w3.org/1999/02/22-rdf-syntax-ns#type",
            "http://www.w3.org/2000/01/rdf-schema#label",
            "http://www.w3.org/2000/01/rdf-schema#comment"
        );
    }

    // /api/index/init reads triples from all endpoints of local blazegraph to create index
    @PostMapping("/init")
    public ResponseEntity<String> initIndex() {
        indexAgent.initializeIndex();
        return ResponseEntity.ok("Index initialised successfully from local blazegraph server. ");
    }

    // /api/index/create reads triples from a specific endpoint to create index
    @PostMapping("/create")
    public ResponseEntity<String> createIndexFrom(@RequestParam String endpointUrl) {
        indexAgent.createIndexFrom(endpointUrl);
        return ResponseEntity.ok("Index created successfully from endpoint: " + endpointUrl);
    }


    @PostMapping("/reindex")
    public ResponseEntity<String> reindexing() {
        try{
            indexAgent.serialiseIndexData();
        } catch (IOException e) {
            e.printStackTrace();
        }
        return ResponseEntity.ok("Index updated successfully through reindexing.");
    }

    // /api/index/backup serialise index data in a file and remove reindex-file, if there is any
    @PostMapping("/backup")
    public ResponseEntity<String> backup() {
        try{
            indexAgent.serialiseIndexData();
        } catch (IOException e) {
            e.printStackTrace();
        }
        return ResponseEntity.ok("Memory Index Backup Successfully.");
    }

    // /api/index/add adds an index data to dragonfly and broadcast to other nodes
    @PostMapping("/add")
    public ResponseEntity<String> addIndexEntity(@RequestParam String key, @RequestParam String value) {
        indexAgent.addIndexEntity(key, value);
        return ResponseEntity.ok("Value added successfully: " + value);
    }

    // /api/index/get gets endpoints from the index
    @GetMapping("/get")
    public ResponseEntity<Set<String>> getEndpoints(@RequestParam String key) {
        Set<String> values = indexAgent.getEndpoints(key);
        return ResponseEntity.ok(values);
    }

    // /api/index/analyse retrieves all endpoints associated to a specific SPARQL
    @PostMapping("/analyse")
    public ResponseEntity<String> analyseSPARQL(@RequestBody String sparql) {
        Set<String> cp_keys=new HashSet<String>();
        Set<String> endpoints=new HashSet<String>();
        if(isQuery(sparql)==1){
            cp_keys=extractClassesAndProperties(sparql);
            for (String key : cp_keys) {
                if(stop_cps.contains(key)) continue;
                endpoints.addAll(indexAgent.getEndpoints(key));
            }
        }
        
        return ResponseEntity.ok(endpoints.toString());
    }

    // /api/index/query derived results out of a specific SPARQL using FedX
    @PostMapping("/query")
    public ResponseEntity<String> executeQuery(@RequestBody String sparql) {
        Set<String> cp_keys=new HashSet<String>();
        Set<String> endpoints=new HashSet<String>();
        Set<BindingSet> result = new HashSet<BindingSet>();
        if(isQuery(sparql)==1){
            cp_keys=extractClassesAndProperties(sparql);
            for (String key : cp_keys) {
                if(stop_cps.contains(key)) continue;
                System.out.println(key);
                endpoints.addAll(indexAgent.getEndpoints(key));
            }

            if(endpoints.size()>0){
                result=executeQuery(endpoints, sparql);
            }else{
                return ResponseEntity.ok("No Endpoint Found!\n");        
            }
        }
        
        return ResponseEntity.ok(result.toString()+"\n");
    }

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

    // case 1 -> "SPARQL Query (Read Operation)";
    // case 0 -> "SPARQL Update (Write Operation)";
    // default -> "Invalid SPARQL Syntax";
    public int isQuery(String sparql) {
        try {
            // Try parsing as a SPARQL Query (SELECT, ASK, DESCRIBE, CONSTRUCT)
            Query query = QueryFactory.create(sparql);
            return 1;
        } catch (QueryParseException e) {
            try {
                // Try parsing as a SPARQL Update (INSERT, DELETE, LOAD, CLEAR, etc.)
                UpdateRequest updateRequest = UpdateFactory.create(sparql);
                return 0;
            } catch (Exception ex) {
                return -1;
            }
        }
    }

    public Set<String> extractClassesAndProperties(String sparqlQuery) {
        Set<String> cp_keys=new HashSet<String>();

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
                    if (subject.isURI()){
                        cp_keys.add(subject.getURI());
                    }
                    if (predicate.isURI()){
                        cp_keys.add(predicate.getURI());
                    }
                    if (object.isURI() && object.getURI().startsWith("http")){
                        cp_keys.add(object.getURI());
                    }
                }
            }
        });

        return cp_keys;
    }
}
