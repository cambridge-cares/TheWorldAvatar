package uk.ac.cam.cares.jps.base.query.fedq;

import com.google.gson.*;
import com.google.gson.reflect.TypeToken;
import org.apache.jena.graph.Triple;
import org.apache.jena.query.Query;
import org.apache.jena.query.QueryExecution;
import org.apache.jena.query.QueryFactory;
import org.apache.jena.query.QuerySolution;
import org.apache.jena.rdf.model.*;
import org.apache.jena.sparql.exec.http.QueryExecutionHTTP;
import org.apache.jena.vocabulary.RDF;
import org.apache.jena.query.ResultSet;

import java.io.*;
import java.util.*;

public class BuildInvertedIndex {
    private String classIndexFilePath = "";
    private String propertyIndexFilePath = "";
    private String cpIndexFilePath = "";

    private int tripleCount = 0;
    private Map<String, Set<String>> classIndex = new HashMap<>();
    private Map<String, Set<String>> propertyIndex = new HashMap<>();
    private Map<String, Map<String, Set<String>>> cpIndex = new HashMap<>();
    private Model model = ModelFactory.createDefaultModel();

    public void setIndexLocation(String indexDir) {
        if (!indexDir.endsWith("/")) {
            indexDir = indexDir.trim() + "/";
        }
        this.classIndexFilePath = indexDir + "cinv.indx";
        this.propertyIndexFilePath = indexDir + "pinv.indx";
        this.cpIndexFilePath = indexDir + "cpinv.indx";
    }

    public void startProcessing(String endpointUrl, String sparqlQuery) {
        int counter = 0;
        // Create a Query object
        Query query = QueryFactory.create(sparqlQuery);

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

                if (!classUri.isEmpty()) {
                    classIndex.computeIfAbsent(classUri, k -> new HashSet<>()).add(endpointUrl);
                }
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

    public void addTriplesAndUpdateIndex(List<Triple> newTriples, String endpointUrl) {
        for (Triple triple : newTriples) {
            model.add(model.asStatement(triple));

            if (triple.getPredicate().equals(RDF.type.asNode())) {
                String classUri = triple.getObject().getURI();
                String lastToken = getSubstringFromLast(classUri);
                if (lastToken.equals("Ontology") || lastToken.equals("Class") || lastToken.equals("NamedIndividual")) {
                    continue;
                }
                classIndex.computeIfAbsent(classUri, k -> new HashSet<>()).add(endpointUrl);
            } else {
                String propertyUri = triple.getPredicate().getURI();
                String lastToken = getSubstringFromLast(propertyUri);
                if (lastToken.equals("label")) {
                    continue;
                }
                propertyIndex.computeIfAbsent(propertyUri, k -> new HashSet<>()).add(endpointUrl);
            }
        }

        for (Triple triple : newTriples) {
            if (propertyIndex.containsKey(triple.getPredicate().getURI())) {
                String propertyUri = triple.getPredicate().getURI();
                if (classIndex.containsKey(triple.getSubject().getURI())) {
                    String classUri = triple.getSubject().getURI();
                    cpIndex.computeIfAbsent(classUri, k -> new HashMap<>())
                            .computeIfAbsent(propertyUri, k -> new HashSet<>())
                            .add(endpointUrl);
                }
                if (classIndex.containsKey(triple.getObject().getURI())) {
                    String classUri = triple.getObject().getURI();
                    cpIndex.computeIfAbsent(classUri, k -> new HashMap<>())
                            .computeIfAbsent(propertyUri, k -> new HashSet<>())
                            .add(endpointUrl);
                }
            }
        }

        tripleCount += newTriples.size();
        System.out.println("Added " + newTriples.size() + " triples and updated the index.");
    }

    public void loadIndices() {
        loadClassIndex();
        loadPropertyIndex();
        loadCpIndex();
    }

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

    public void saveIndices() {
        saveClassIndex();
        savePropertyIndex();
        saveCpIndex();
    }

    private void saveClassIndex() {
        try (Writer writer = new FileWriter(classIndexFilePath)) {
            new GsonBuilder().setPrettyPrinting().create().toJson(classIndex, writer);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    private void savePropertyIndex() {
        try (Writer writer = new FileWriter(propertyIndexFilePath)) {
            new GsonBuilder().setPrettyPrinting().create().toJson(propertyIndex, writer);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    private void saveCpIndex() {
        try (Writer writer = new FileWriter(cpIndexFilePath)) {
            new GsonBuilder().setPrettyPrinting().create().toJson(cpIndex, writer);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    private String getSubstringFromLast(String string) {
        int lastSlash = string.lastIndexOf('/');
        int lastHash = string.lastIndexOf('#');
        return string.substring(Math.max(lastSlash, lastHash) + 1);
    }

    public static void main(String[] args) {

        String sparqlQuery = """
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
        String blazegraphBaseUrl = "http://localhost:8080/blazegraph";
        String[] namespaces = { "namespace_uken", "namespace_kin", "namespace_compchem" };
        String indexLocation = "C:/Users/printer_admin/Downloads/KGs/tests";

        BuildInvertedIndex kgs = new BuildInvertedIndex();
        kgs.setIndexLocation(indexLocation);

        for (String namespace : namespaces) {
            String endpointUrl = blazegraphBaseUrl + "/namespace/" + namespace + "/sparql";
            System.out.println("Start processing endpoint: " + endpointUrl);
            kgs.startProcessing(endpointUrl, sparqlQuery);
        }

        kgs.saveIndices();
        System.out.println("Completed and index-files are saved in " + indexLocation + ".");
    }
}
