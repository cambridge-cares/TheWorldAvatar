package uk.ac.cam.cares.jps.accessagent.fedq_accessagent;

import java.io.File;
import java.io.IOException;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.*;

import java.io.ByteArrayInputStream;
import java.nio.charset.StandardCharsets;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.stereotype.Service;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.web.client.RestTemplate;
import org.springframework.http.ResponseEntity;

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

import org.apache.jena.rdf.model.*;
import org.apache.jena.util.FileManager;
import org.apache.jena.query.*;
import org.apache.jena.graph.Node;


@Service
public class IndexAgent {
    private static final String BACKUP_DIRECTORY = "/data";
    private static final String BACKUP_FILE = BACKUP_DIRECTORY + "/backup.json"; 
    private static final String BLAZEGRAPH_URL = "http://172.26.15.166:48889/blazegraph/";
    private final RestTemplate restTemplate = new RestTemplate();
    private final Gson gson = new Gson();
    
    private final ObjectMapper objectMapper = new ObjectMapper(); // JSON serializer

    @Autowired
    private StringRedisTemplate stringRedisTemplate; // Used for Pub/Sub

    @Autowired
    private RedisTemplate<String, String> redisTemplate;

    private final String STACK_ID="S001";

    // Read stack ID from environment variable or default to "defaultStack"
    // public IndexAgent(@Value("${stack.id:defaultStack}") String stackId) {
    //     this.STACK_ID = stackId;
    //     System.out.println("Initialized IndexAgent for Stack: " + STACK_ID);
    // }

    private static final String CHANNEL_NAME = "stackSyncChannel"; // Pub/Sub channel

    // Add a value to a Set with a key(ensuring uniqueness)
    public void addValue(String key, String value) {
        if (key != null && value != null) {
            String stackKey = STACK_ID + ":" + key; // Namespaced key
            redisTemplate.opsForSet().add(stackKey, value);

            // 🔹 Broadcast the update
            String message = STACK_ID + ":ADD:" + key + ":" + value;
            stringRedisTemplate.convertAndSend(CHANNEL_NAME, message);
        }
    }

    // Load/not-a-new-add a value 
    public void loadValue(String key, String value) {
            String stackKey = STACK_ID + ":" + key; // Namespaced key
            redisTemplate.opsForSet().add(stackKey, value);
        }
    }

    // Remove value & broadcast
    public void removeValue(String key, String value) {
        if (key != null && value != null) {
            String stackKey = STACK_ID + ":" + key;
            redisTemplate.opsForSet().remove(stackKey, value);

            // 🔹 Broadcast the removal
            String message = STACK_ID + ":REMOVE:" + key + ":" + value;
            stringRedisTemplate.convertAndSend(CHANNEL_NAME, message);
        }
    }

    // Retrieve all values from the Set using key
    public Set<String> getValues(String key) {
        Set<String> values = redisTemplate.opsForSet().members(key);
        if (values == null) {
            System.out.println("No values found for key: " + key);
        }
        return values;
    }

    // Retrieve values from ALL stacks for a key
    public Set<String> getAllStackValues(String key) {
        Set<String> resultSet = new HashSet<>();

        // 🔹 Get all stack-prefixed keys matching the given key
        Set<String> keys = redisTemplate.keys("*:" + key);

        if (keys != null) { // Prevent NullPointerException
            for (String stackKey : keys) {
                Set<String> values = redisTemplate.opsForSet().members(stackKey);
                if (values != null) { // Prevent NullPointerException
                    resultSet.addAll(values);
                }
            }
        }

        return resultSet;
    }

    public List<String> extractNamespaces(String rdfData) {
        List<String> namespaces = new ArrayList<>();

        // Parse the RDF data using Jena
        Model model = ModelFactory.createDefaultModel();
        ByteArrayInputStream inputStream = new ByteArrayInputStream(rdfData.getBytes(StandardCharsets.UTF_8));

        model.read(inputStream, null, "N-TRIPLES");

        // Define the query to extract namespaces
        String queryString = "SELECT ?namespace WHERE { " +
                             "?s <http://www.bigdata.com/rdf#/features/KB/Namespace> ?namespace . " +
                             "}";

        // Execute the query using ARQ (Apache Jena Query Engine)
        Query query = QueryFactory.create(queryString);
        QueryExecution qexec = QueryExecutionFactory.create(query, model);

        // Process the results
        try {
            ResultSet results = qexec.execSelect();
            while (results.hasNext()) {
                QuerySolution solution = results.nextSolution();
                String namespace = solution.getLiteral("namespace").getString();
                namespaces.add(namespace);
                System.out.println("Namespace found: " + namespace);
            }
        } finally {
            qexec.close();
        }
        return namespaces;
    }

    public List<String> getNamespaces() {
        List<String> namespaces = new ArrayList<>();

        try {
            ResponseEntity<String> response = restTemplate.getForEntity(BLAZEGRAPH_URL+"namespace", String.class);
            String responseBody = response.getBody();
            namespaces=extractNamespaces(responseBody);
            // if (responseBody != null) {
            //     System.out.println("Response Body: " + responseBody); 

            //     //JsonArray namespacesArray = gson.fromJson(responseBody, JsonArray.class);
            //     JsonElement element = gson.fromJson(responseBody, JsonElement.class);

            //     // for (int i = 0; i < namespacesArray.size(); i++) {
            //     //     JsonObject namespaceObject = namespacesArray.get(i).getAsJsonObject();
            //     //     String namespace = namespaceObject.get("namespace").getAsString();
            //     //     namespaces.add(namespace);
            //     // }
            //     // Check if the response is an array
            //     if (element.isJsonArray()) {
            //         JsonArray namespacesArray = element.getAsJsonArray();
            //         for (int i = 0; i < namespacesArray.size(); i++) {
            //             JsonObject namespaceObject = namespacesArray.get(i).getAsJsonObject();
            //             String namespace = namespaceObject.get("namespace").getAsString();
            //             System.out.println("Found Namespace: " + namespace);
            //             // Add logic to process each namespace as needed
            //         }
            //     } else if (element.isJsonObject()) {
            //         JsonObject responseObject = element.getAsJsonObject();
            //         // If it's an object, print the object or handle accordingly
            //         System.out.println("Received Object: " + responseObject);
            //     } else if (element.isJsonPrimitive()) {
            //         // If the response is a primitive (string/number), handle it accordingly
            //         System.out.println("Received Primitive: " + element.getAsString());
            //     } else {
            //         System.out.println("Unexpected response format");
            //     }
            // }
        } catch (Exception e) {
            System.err.println("Error fetching namespaces from Blazegraph: " + e.getMessage());
        }

        return namespaces;
    }

    public void initializeIndex() {
        int counter = 0;
        String sparqlQuery = """
                PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
                PREFIX owl: <http://www.w3.org/2002/07/owl#>
                PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
                SELECT DISTINCT ?class ?property
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
        // Create a Query object
        Query query = QueryFactory.create(sparqlQuery);

        List<String> namespaces=this.getNamespaces();

        for(String namespace : namespaces){
            String endpointUrl = BLAZEGRAPH_URL + "namespace/" + namespace + "/sparql";
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
    
                    // if (this.stop_classes.contains(classUri))
                    //     continue;
                    if (!classUri.isEmpty()) {
                        loadValue(classUri, endpointUrl);
                        //System.out.println("C: "+classUri+ "=>"+ endpointUrl);
                    }
    
                    // if (this.stop_properties.contains(propertyUri))
                    //     continue;
                    if (!propertyUri.isEmpty()) {
                        loadValue(propertyUri, endpointUrl);
                        //System.out.println("P: "+propertyUri+ "=>"+ endpointUrl);
                    }
    
                    // if (!classUri.isEmpty() && !propertyUri.isEmpty()) {
                    //     cpIndex.computeIfAbsent(classUri, k -> new HashMap<>())
                    //             .computeIfAbsent(propertyUri, k -> new HashSet<>())
                    //             .add(endpointUrl);
                    // }
                }
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
    }

    // Deserialize Dragonfly index from JSON file (restore data)**
    public void restoreFromBackup() throws IOException {
        try {
            File file = new File(BACKUP_FILE);
            if (file.exists()) {
                // Use correct generic types for HashMap
                ObjectMapper objectMapper = new ObjectMapper();
                TypeReference<Map<String, Set<String>>> typeRef = new TypeReference<>() {
                };

                Map<String, Set<String>> backupData = objectMapper.readValue(file, typeRef);

                // Now safely iterate over the restored backup data
                for (Map.Entry<String, Set<String>> entry : backupData.entrySet()) {
                    String key = entry.getKey();
                    Set<String> values = entry.getValue();

                    for (String value : values) {
                        loadValue(key, value); // Load into Redis
                    }
                }
                System.out.println("Successfully restored Dragonfly index data from backup.");
            } else {
                System.out.println("No backup file found. Skipping restore and Initializing index from store-endpoints.");
                this.initializeIndex();
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    // Serialize Dragonfly index to JSON file (backup data)**
    public void backupDataToJson() throws IOException {
        try {
            File backupDir = new File(BACKUP_DIRECTORY);
            if (!backupDir.exists()) {
                System.out.println("Creating backup directory: " + BACKUP_DIRECTORY);
                boolean created = backupDir.mkdirs();
                if (!created) {
                    throw new IOException("Failed to create backup directory.");
                }
            }

            Set<String> keys = redisTemplate.keys("*");
            if (keys == null || keys.isEmpty()) {
                System.out.println("No data found in Redis to serialize.");
                return;
            }
            
            Map<String, Set<String>> backupData = new HashMap<>();
            int count=0;
            for (String key : keys) {
                if(key == null || !key.contains(":")) continue;
                Set<String> values = getValues(key);
                if (values != null && !values.isEmpty()) {
                    backupData.put(key, values);
                    //System.out.println((count++)+". Backup data for key: " + key + " -> " + values);
                }
            }

            if (backupData.isEmpty()) {
                System.out.println("No values to backup.");
                return;
            }

            File file = new File(BACKUP_FILE);
            objectMapper.writeValue(file, backupData);
            System.out.println("Backup saved to: " + BACKUP_FILE);
            
            //System.out.println(backupData.toString());
            
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
