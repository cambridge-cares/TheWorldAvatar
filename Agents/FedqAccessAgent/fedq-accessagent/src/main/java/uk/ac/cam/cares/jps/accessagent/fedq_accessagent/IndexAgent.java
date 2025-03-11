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
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
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
    // public void addValue(String key, String value) {
    //     if (key != null && value != null) {
    //         String stackKey = STACK_ID + ":" + key; // Namespaced key
    //         redisTemplate.opsForSet().add(stackKey, value);

    //         // 🔹 Broadcast the update
    //         String message = STACK_ID + ":ADD:" + key + ":" + value;
    //         stringRedisTemplate.convertAndSend(CHANNEL_NAME, message);
    //     }
    // }

    public void addValue(String key, String endpoint) {
        if (key != null && endpoint != null) {
            try {
                ObjectMapper objectMapper = new ObjectMapper();
                Map<String, String> message = new HashMap<>();
                message.put("stack", STACK_ID);
                message.put("endpoint", endpoint);
                
                //insert into the dragonfly memory
                redisTemplate.opsForSet().add(key, objectMapper.writeValueAsString(message)); // Store as JSON
    
                // Publish/broadcast JSON-based message
                message.put("key", key);
                message.put("action", "ADD");
                String jsonMessage = objectMapper.writeValueAsString(message);
                stringRedisTemplate.convertAndSend(CHANNEL_NAME, jsonMessage);
                System.out.println("Published: " + jsonMessage);
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
    }

    public void addValue(String key, String endpoint, String source_stack) {
        if (key != null && endpoint != null) {
            try {
                ObjectMapper objectMapper = new ObjectMapper();
                Map<String, String> message = new HashMap<>();
                message.put("stack", source_stack);
                message.put("endpoint", endpoint);
                
                //insert into the dragonfly memory
                redisTemplate.opsForSet().add(key, objectMapper.writeValueAsString(message)); // Store as JSON
    
                // Publish/broadcast JSON-based message
                message.put("key", key);
                message.put("action", "ADD");
                String jsonMessage = objectMapper.writeValueAsString(message);
                stringRedisTemplate.convertAndSend(CHANNEL_NAME, jsonMessage);
                System.out.println("Published: " + jsonMessage);
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
    }

    // Load/not-a-new-add a endpoint 
    public void loadValue(String key, String endpoint) {
        if (key != null && endpoint != null) {
            // String stackKey = STACK_ID + ":" + key; // Namespaced key
            // redisTemplate.opsForSet().add(stackKey, endpoint);
            try {
                ObjectMapper objectMapper = new ObjectMapper();
                Map<String, String> message = new HashMap<>();
                message.put("stack", STACK_ID);
                message.put("endpoint", endpoint);
                
                //insert into the dragonfly memory
                redisTemplate.opsForSet().add(key, objectMapper.writeValueAsString(message)); // Store as JSON
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
    }

    public void loadValue(String key, String endpoint, String source_stack) {
        if (key != null && endpoint != null) {
            // String stackKey = source_stack + ":" + key; // Namespaced key
            // redisTemplate.opsForSet().add(stackKey, endpoint);
            try{
                ObjectMapper objectMapper = new ObjectMapper();
                Map<String, String> message = new HashMap<>();
                message.put("stack", source_stack);
                message.put("endpoint", endpoint);
                
                //insert into the dragonfly memory
                redisTemplate.opsForSet().add(key, objectMapper.writeValueAsString(message)); // Store as JSON
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
    }

    // Remove endpoint & broadcast
    public void removeValue(String key, String endpoint, String source_stack) {
        try{
            if (key != null && endpoint != null) {
                // Retrieve the current Set of JSON strings from Redis for the given key
                Set<String> jsonSet = redisTemplate.opsForSet().members(key);
    
                if (jsonSet != null && !jsonSet.isEmpty()) {
                    // Initialize a new set to store updated JSON endpoints
                    Set<String> updatedJsonSet = new HashSet<>();
                    ObjectMapper objectMapper = new ObjectMapper();
                    boolean removed = false; // Flag to track if the pair was found and removed
    
                    // Iterate through the Set of JSONs
                    for (String jsonString : jsonSet) {
                        // Convert JSON string to a Map or JSON object
                        ObjectNode jsonObject = objectMapper.readValue(jsonString, ObjectNode.class);
    
                        // Check if the endpoint matches and remove it
                        if (jsonObject.has("endpoint") && jsonObject.get("endpoint").asText().equals(endpoint) &&
                            jsonObject.has("stack") && jsonObject.get("stack").asText().equals(source_stack)) {
                            // If endpoint matches, skip this one (i.e., do not add it to the updated set)
                            removed = true;
                            continue; // Skip this JSON entry
                        }
    
                        // Otherwise, keep the current JSON object in the updated set
                        updatedJsonSet.add(jsonObject.toString());
                    }
    
                    // If we removed the endpoint, update the Redis Set with the modified Set
                    if (removed) {
                        // Replace the old Set with the new Set in Redis
                        redisTemplate.opsForSet().add(key, updatedJsonSet.toArray(new String[0]));
    
                        // Broadcast the REMOVE JSON-based message
                        Map<String, String> message = new HashMap<>();
                        message.put("stack", STACK_ID);
                        message.put("endpoint", endpoint);
                        message.put("key", key);
                        message.put("action", "REMOVE");
                        String jsonMessage = objectMapper.writeValueAsString(message);
                        stringRedisTemplate.convertAndSend(CHANNEL_NAME, jsonMessage);
                        System.out.println("Synced REMOVE: " + jsonMessage);
                    } else {
                        System.out.println("No matching endpoint found for key: " + key);
                    }
                } else {
                    System.out.println("No values found for key: " + key);
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public void removeValueWithoutBroadcast(String key, String endpoint, String source_stack) {
        try{

            if (key != null && endpoint != null) {
                // Retrieve the current Set of JSON strings from Redis for the given key
                Set<String> jsonSet = redisTemplate.opsForSet().members(key);
    
                if (jsonSet != null && !jsonSet.isEmpty()) {
                    // Initialize a new set to store updated JSON endpoints
                    Set<String> updatedJsonSet = new HashSet<>();
                    ObjectMapper objectMapper = new ObjectMapper();
                    boolean removed = false; // Flag to track if the pair was found and removed
    
                    // Iterate through the Set of JSONs
                    for (String jsonString : jsonSet) {
                        // Convert JSON string to a Map or JSON object
                        ObjectNode jsonObject = objectMapper.readValue(jsonString, ObjectNode.class);
    
                        // Check if the endpoint matches and remove it
                        if (jsonObject.has("endpoint") && jsonObject.get("endpoint").asText().equals(endpoint) &&
                            jsonObject.has("stack") && jsonObject.get("stack").asText().equals(source_stack)) {
                            // If endpoint matches, skip this one (i.e., do not add it to the updated set)
                            removed = true;
                            continue; // Skip this JSON entry
                        }
    
                        // Otherwise, keep the current JSON object in the updated set
                        updatedJsonSet.add(jsonObject.toString());
                    }
    
                    // If we removed the endpoint, update the Redis Set with the modified Set
                    if (removed) {
                        // Replace the old Set with the new Set in Redis
                        redisTemplate.opsForSet().add(key, updatedJsonSet.toArray(new String[0]));
                    } else {
                        System.out.println("No matching endpoint found for key: " + key);
                    }
                } else {
                    System.out.println("No values found for key: " + key);
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    // Retrieve all values from the Set using key
    public Set<String> getEndpoints(String key) {
        Set<String> endpoints = new HashSet<>();
        try {
            // Retrieve the Set of JSON strings from Redis
            Set<String> jsonSet = redisTemplate.opsForSet().members(key);
    
            if (jsonSet != null && !jsonSet.isEmpty()) {
                ObjectMapper objectMapper = new ObjectMapper();
                
                // Iterate through each JSON string in the set
                for (String jsonString : jsonSet) {
                    // Convert JSON string to a JsonNode (generic tree structure for JSON)
                    JsonNode jsonNode = objectMapper.readTree(jsonString);
                    
                    // Check if the JSON contains "endpoint" field and add it to the set
                    if (jsonNode.has("endpoint")) {
                        String endpoint = jsonNode.get("endpoint").asText();
                        endpoints.add(endpoint);
                    }
                }
            } else {
                System.out.println("No values found for key: " + key);
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
    
        // Return the Set of endpoints
        return endpoints;
    }

    // Retrieve values from ALL stacks for a key
    // public Set<String> getAllStackValues(String key) {
    //     Set<String> resultSet = new HashSet<>();

    //     // 🔹 Get all stack-prefixed keys matching the given key
    //     Set<String> keys = redisTemplate.keys("*:" + key);

    //     if (keys != null) { // Prevent NullPointerException
    //         for (String stackKey : keys) {
    //             Set<String> values = redisTemplate.opsForSet().members(stackKey);
    //             if (values != null) { // Prevent NullPointerException
    //                 resultSet.addAll(values);
    //             }
    //         }
    //     }

    //     return resultSet;
    // }

    
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
                if(key == null) continue;
                Set<String> values = redisTemplate.opsForSet().members(key);
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
                    redisTemplate.opsForSet().add(entry.getKey(), entry.getValue().toArray(new String[0]));
                    // String key = entry.getKey();
                    // Set<String> values = entry.getValue();

                    // for (String value : values) {
                    //     loadValue(key, value); // Load into Redis
                    // }
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

    public List<String> getNamespaces() {
        List<String> namespaces = new ArrayList<>();

        try {
            ResponseEntity<String> response = restTemplate.getForEntity(BLAZEGRAPH_URL+"namespace", String.class);
            String responseBody = response.getBody();
            namespaces=extractNamespaces(responseBody);
        } catch (Exception e) {
            System.err.println("Error fetching namespaces from Blazegraph: " + e.getMessage());
        }

        return namespaces;
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

}
