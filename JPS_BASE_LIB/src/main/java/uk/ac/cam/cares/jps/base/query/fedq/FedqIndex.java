package uk.ac.cam.cares.jps.base.query.fedq;

import com.google.gson.*;
import com.google.gson.reflect.TypeToken;

import org.apache.jena.graph.Node;
import org.apache.jena.graph.Triple;
import org.apache.jena.query.Query;
import org.apache.jena.query.QueryExecution;
import org.apache.jena.query.QueryFactory;
import org.apache.jena.query.QuerySolution;
import org.apache.jena.rdf.model.*;
import org.apache.jena.riot.system.StreamRDF;
import org.apache.jena.riot.system.StreamRDFBase;
import org.apache.jena.riot.Lang;
import org.apache.jena.riot.RDFParser;

import org.apache.jena.sparql.algebra.Op;
import org.apache.jena.sparql.algebra.OpVisitorBase;
import org.apache.jena.sparql.algebra.OpWalker;
import org.apache.jena.sparql.algebra.op.OpBGP;
import org.apache.jena.sparql.core.BasicPattern;
import org.apache.jena.sparql.exec.http.QueryExecutionHTTP;
import org.apache.jena.vocabulary.RDF;
import org.apache.jena.query.ResultSet;

import java.io.*;
import java.lang.reflect.Type;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * The class wraps some functionality to-
 * 1. create inverted index on a number of endpoints
 * a) class to endpoint inverted index
 * b) property to endpoint inverted index
 * c) class-to property to endpoint inverted inedx
 * 2. update inverted index on insertion of new set of triple into an endpoint
 * 3. load inverted index from a file data into a memory map
 * 4. save inverted index from memory map to storage file
 */

public class FedqIndex {
    private static final String CLASS_PROPERTY_EXTRACTION_QUERY = """
            PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
            PREFIX owl: <http://www.w3.org/2002/07/owl#>
            PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
            SELECT ?class ?property
            WHERE {
                {
                    ?subject ?property ?object .
                    ?subject a ?class .
                    FILTER (
                        isIRI(?class) &&
                        ?class != owl:Ontology &&
                        ?class != owl:Class &&
                        ?class != owl:NamedIndividual &&
                        !isBlank(?property) &&
                        isIRI(?property) &&
                        ?property != rdf:type &&
                        ?property != rdfs:label &&
                        ?property != rdfs:comment
                    )
                }
                UNION
                {
                    ?subject ?property ?object .
                    ?object a ?class .
                    FILTER (
                        isIRI(?class) &&
                        ?class != owl:Ontology &&
                        ?class != owl:Class &&
                        ?class != owl:NamedIndividual &&
                        !isBlank(?property) &&
                        isIRI(?property) &&
                        ?property != rdf:type &&
                        ?property != rdfs:label &&
                        ?property != rdfs:comment
                    )
                }
            }
            """;

    private String classIndexFilePath = "";
    private String propertyIndexFilePath = "";
    private String cpIndexFilePath = "";

    // private int tripleCount = 0;
    private HashSet<String> stop_classes = new HashSet<>();
    private HashSet<String> stop_properties = new HashSet<>();
    private Map<String, Set<String>> classIndex = new HashMap<>();
    private Map<String, Set<String>> propertyIndex = new HashMap<>();
    private Map<String, Map<String, Set<String>>> cpIndex = new HashMap<>();
    private Model model = ModelFactory.createDefaultModel();

    // loads
    private Set<Node> classes = new HashSet<>();
    private Set<Node> properties = new HashSet<>();

    public FedqIndex() {
        this.loadStopCPs("src/main/java/uk/ac/cam/cares/jps/base/query/fedq/stopcps.json");
        setIndexLocation("");
    }

    public FedqIndex(String indexDir) {
        this.loadStopCPs("src/main/java/uk/ac/cam/cares/jps/base/query/fedq/stopcps.json");
        this.setIndexLocation(indexDir);
    }

    /**
     * indexDir specify the root directory of the indices
     * 
     * @param indexDir
     * @return
     */
    public void setIndexLocation(String indexDir) {
        if (indexDir.length() > 0 && !indexDir.endsWith("/")) {
            indexDir = indexDir.trim() + "/";
        }
        this.classIndexFilePath = indexDir + "cinv.indx";
        this.propertyIndexFilePath = indexDir + "pinv.indx";
        this.cpIndexFilePath = indexDir + "cpinv.indx";
    }

    /**
     * createInvertedIndex works on an endpoint to retrieve class and properties
     * through
     * sparqlQuey to create an index
     * 
     * @param endpointUrl,
     * @param sparqlQuery
     * @return
     */
    public void createInvertedIndex(String endpointUrl) {
        int counter = 0;
        // Create a Query object
        Query query = QueryFactory.create(CLASS_PROPERTY_EXTRACTION_QUERY);

        // Create a QueryExecution using the builder
        try (QueryExecution qexec = QueryExecutionHTTP.create()
                .endpoint(endpointUrl)
                .query(query)
                .build()) {
            ResultSet results = qexec.execSelect();
            while (results.hasNext()) {
                QuerySolution soln = results.nextSolution();
                String classUri = soln.get("class").toString();
                String propertyUri = soln.get("property").toString();

                if (this.stop_classes.contains(classUri))
                    continue;
                if (!classUri.isEmpty()) {
                    classIndex.computeIfAbsent(classUri, k -> new HashSet<>()).add(endpointUrl);
                }

                if (this.stop_properties.contains(propertyUri))
                    continue;
                if (!propertyUri.isEmpty()) {
                    propertyIndex.computeIfAbsent(propertyUri, k -> new HashSet<>()).add(endpointUrl);
                }

                if (!classUri.isEmpty() && !propertyUri.isEmpty()) {
                    cpIndex.computeIfAbsent(classUri, k -> new HashMap<>())
                            .computeIfAbsent(propertyUri, k -> new HashSet<>())
                            .add(endpointUrl);
                }

                if (++counter % 1000000 == 0)
                    System.out.print(counter + " ");
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public Map<String, Set<String>> getClassIndex() {
        return classIndex;
    }

    public Map<String, Set<String>> getPropertyIndex() {
        return propertyIndex;
    }

    public Map<String, Map<String, Set<String>>> getCpIndex() {
        return cpIndex;
    }

    /**
     * it loads stop-CPs: list of common classes & properties
     * 
     * @param stop_cps_file
     * @return
     */
    public void loadStopCPs(String stop_cps_file) {
        Map<String, HashSet<String>> stop_cps = new HashMap<>();
        try (Reader reader = new FileReader(stop_cps_file)) {
            stop_cps = new Gson().fromJson(reader, new TypeToken<Map<String, Set<String>>>() {
            }.getType());
            this.stop_classes = stop_cps.get("classes");
            this.stop_properties = stop_cps.get("properties");
            System.out.println("Stop-classes & Stop-properties loaded successfully.");
        } catch (FileNotFoundException e) {
            System.err.println("File '" + stop_cps_file + "' not found.");
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    /**
     * it specifies update of indices on new set of triple to be inserted into an
     * endpoint
     * 
     * @param newTriples
     * @param endpoints
     * @return
     */
    public void addTriplesAndUpdateIndex(List<Triple> newTriples, String endpointUrl) {
        for (Triple triple : newTriples) {
            // model.add(model.asStatement(triple));

            if (triple.getPredicate().equals(RDF.type.asNode())) {
                String classUri = triple.getObject().getURI();
                if (this.stop_classes.contains(classUri))
                    continue;
                classIndex.computeIfAbsent(classUri, k -> new HashSet<>()).add(endpointUrl);
            } else {
                String propertyUri = triple.getPredicate().getURI();
                if (this.stop_properties.contains(propertyUri))
                    continue;
                propertyIndex.computeIfAbsent(propertyUri, k -> new HashSet<>()).add(endpointUrl);
            }
        }

        for (Triple triple : newTriples) {
            if (propertyIndex.containsKey(triple.getPredicate().getURI())) {
                String propertyUri = triple.getPredicate().getURI();
                if (classIndex.containsKey(triple.getSubject().getURI())) {
                    String classUri = triple.getSubject().getURI();
                    if (this.stop_classes.contains(classUri))
                        continue;

                    cpIndex.computeIfAbsent(classUri, k -> new HashMap<>())
                            .computeIfAbsent(propertyUri, k -> new HashSet<>())
                            .add(endpointUrl);
                }
                if (classIndex.containsKey(triple.getObject().getURI())) {
                    String classUri = triple.getObject().getURI();
                    if (this.stop_classes.contains(classUri))
                        continue;
                    cpIndex.computeIfAbsent(classUri, k -> new HashMap<>())
                            .computeIfAbsent(propertyUri, k -> new HashSet<>())
                            .add(endpointUrl);
                }
            }
        }

        // tripleCount += newTriples.size();
        System.out.println("Added " + newTriples.size() + " triples and updated the index.");
    }

    public ArrayList<Triple> extractTriples(String sparqlInsert) {
        ArrayList<Triple> tripleList = new ArrayList<>();

        // Step 1: Extract the data portion between `{` and `}` braces
        Pattern pattern = Pattern.compile("\\{(.*?)}", Pattern.DOTALL);
        Matcher matcher = pattern.matcher(sparqlInsert);
        String dataContent = "";
        if (matcher.find()) {
            dataContent = matcher.group(1).trim();
        }

        // Step 2: Parse the extracted data content as N-Triples format
        ByteArrayInputStream inputStream = new ByteArrayInputStream(dataContent.getBytes());

        // Step 3: Use a custom StreamRDF to collect triples
        StreamRDF tripleCollector = new StreamRDFBase() {
            @Override
            public void triple(Triple triple) {
                tripleList.add(triple);
            }
        };

        // Step 4: Parse the triples using RDFParser
        RDFParser.create()
                .source(inputStream)
                .lang(Lang.TTL)
                .parse(tripleCollector);

        return tripleList;
    }

    /**
     * it loads indices from initialised directory
     * 
     * @return
     */
    public void loadIndices() {
        loadClassIndex();
        loadPropertyIndex();
        loadCpIndex();
    }

    /**
     * it loads class-to-endpoint index from initialised file
     * 
     * @return
     */
    private void loadClassIndex() {
        try (Reader reader = new FileReader(classIndexFilePath)) {
            classIndex = new Gson().fromJson(reader, new TypeToken<Map<String, List<String>>>() {
            }.getType());
            System.out.println("Class Index loaded from " + classIndexFilePath);
        } catch (FileNotFoundException e) {
            System.err.println("File '" + classIndexFilePath + "' not found.");
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    /**
     * it loads property-to-endpoint index from initialised file
     * 
     * @return
     */
    private void loadPropertyIndex() {
        try (Reader reader = new FileReader(propertyIndexFilePath)) {
            propertyIndex = new Gson().fromJson(reader, new TypeToken<Map<String, List<String>>>() {
            }.getType());
            System.out.println("Property Index loaded from " + propertyIndexFilePath);
        } catch (FileNotFoundException e) {
            System.err.println("File '" + propertyIndexFilePath + "' not found.");
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    /**
     * it loads class-to-property-to-endpoint index from initialised file
     * 
     * @return
     */
    private void loadCpIndex() {
        try (Reader reader = new FileReader(cpIndexFilePath)) {
            cpIndex = new Gson().fromJson(reader, new TypeToken<Map<String, Map<String, List<String>>>>() {
            }.getType());
            System.out.println("Class-Property multilevel inverted index loaded successfully.");
        } catch (FileNotFoundException e) {
            System.err.println("File '" + cpIndexFilePath + "' not found.");
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    /**
     * it saves indices into initialised directory
     * 
     * @return
     */
    public void saveIndices() {
        saveClassIndex();
        savePropertyIndex();
        saveCpIndex();
    }

    /**
     * it saves class-to-endpoint index into initialised file
     * 
     * @return
     */
    private void saveClassIndex() {
        try (Writer writer = new FileWriter(classIndexFilePath)) {
            new GsonBuilder().setPrettyPrinting().create().toJson(classIndex, writer);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    /**
     * it saves property-to-endpoint index into initialised file
     * 
     * @return
     */
    private void savePropertyIndex() {
        try (Writer writer = new FileWriter(propertyIndexFilePath)) {
            new GsonBuilder().setPrettyPrinting().create().toJson(propertyIndex, writer);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    /**
     * it saves class-to-property-to-endpoint index into initialised file
     * 
     * @return
     */
    private void saveCpIndex() {
        try (Writer writer = new FileWriter(cpIndexFilePath)) {
            new GsonBuilder().setPrettyPrinting().create().toJson(cpIndex, writer);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

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
                endpoints.addAll(classIndex.get(classUri));
            }
        }
        for (Node propertyUriRef : properties) {
            String propertyUri = propertyUriRef.getURI();
            if (propertyIndex.containsKey(propertyUri)) {

                endpoints.addAll(propertyIndex.get(propertyUri));
            }

        }

        return endpoints;
    }

    public static void main(String[] args) {

        // String blazegraphBaseUrl = "http://localhost:8080/blazegraph";
        // String[] namespaces = { "namespace_uken", "namespace_kin",
        // "namespace_compchem" };

        FedqIndex fqix = new FedqIndex("C:/Users/printer_admin/Downloads/KGs/tests");

        String sparqlInsert = """
                    INSERT DATA {
                        <http://example.org/subject1> <http://example.org/predicate1> <http://example.org/object1> .
                        <http://example.org/subject2> <http://example.org/predicate2> <http://example.org/object2> .
                    }
                """;

        ArrayList<Triple> triples = fqix.extractTriples(sparqlInsert);
        // addTriplesAndUpdateIndex(List<Triple> newTriples, String endpointUrl)
        for (Triple triple : triples) {
            System.out.println(triple.toString());
        }

        // for (String namespace : namespaces) {
        // String endpointUrl = blazegraphBaseUrl + "/namespace/" + namespace +
        // "/sparql";
        // System.out.println("Start processing endpoint: " + endpointUrl);
        // fqix.createInvertedIndex(endpointUrl);
        // }

        // fqix.saveIndices();
        // System.out.println("Completed and index-files are saved.");
    }
}
