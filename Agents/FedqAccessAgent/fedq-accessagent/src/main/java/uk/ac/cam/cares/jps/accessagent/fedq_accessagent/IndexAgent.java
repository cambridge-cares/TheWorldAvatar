package uk.ac.cam.cares.jps.accessagent.fedq_accessagent;

import java.io.File;
import java.io.IOException;
import java.util.HashSet;
//import java.util.Map;
//import java.util.Set;
import java.util.*;
import java.util.stream.Collectors;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;

import java.io.ByteArrayInputStream;
import java.nio.charset.StandardCharsets;
import jakarta.annotation.PostConstruct;

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
import redis.clients.jedis.Jedis;
import redis.clients.jedis.JedisPubSub;

import com.google.gson.*;
import com.google.gson.JsonObject;
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
    private static final String BASE_BACKUP_FILE = BACKUP_DIRECTORY + "/index-data.json";
    private static final String DUMP_FILE_PREFIX = BACKUP_DIRECTORY + "/dump";
    private static final String REINDEX_FILE = BACKUP_DIRECTORY + "/reindex.dat"; 
    private static final String BLAZEGRAPH_URL = "http://172.26.15.166:3838/blazegraph/ui/";
    private static final String RDF4J_BASE = "http://localhost:8080/rdf4j-server/repositories/";
    private final RestTemplate restTemplate = new RestTemplate();
    private final Gson gson = new Gson();
    private List<String> stop_cps;

    private final String extractClassPropertyQuery = """
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
    
    private final ObjectMapper objectMapper = new ObjectMapper(); // JSON serializer
    
    @Autowired
    private StringRedisTemplate stringRedisTemplate; // Used for Pub/Sub

    @Autowired
    private RedisTemplate<String, String> redisTemplate;

    //@Value("${dragonfly.nodes}") 
    String nodeList="172.26.15.166:6379,139.59.245.38:6379";

    //@Value("${dragonfly.selfnodeindex}")
    private int SELF_INDEX=0;
    Jedis self_jedis;

    private List<Jedis> publishers = new ArrayList<>();
    private static final String CHANNEL_NAME = "twa_index_channel"; // Pub/Sub channel

    public IndexAgent(){
        stop_cps = List.of(
            "http://www.w3.org/2002/07/owl#Ontology",
            "http://www.w3.org/2002/07/owl#Class",
            "http://www.w3.org/2002/07/owl#NamedIndividual",
            "http://www.w3.org/1999/02/22-rdf-syntax-ns#type",
            "http://www.w3.org/2000/01/rdf-schema#label",
            "http://www.w3.org/2000/01/rdf-schema#comment",
            "http://www.w3.org/2002/07/owl#sameAs",
            "http://www.w3.org/2000/01/rdf-schema#sameAs",
            "http://purl.org/dc/elements/1.1/identifier",
            "http://www.w3.org/2002/07/owl#Thing"
        );
        
    }

    @PostConstruct
    public void init() {
        initPublishers(nodeList);
        new Thread(this::subscribeMessages).start();
    }

    private void initPublishers(String nodeList) {
        String node;
        List<String> nodes = Arrays.stream(nodeList.split(","))
                      .map(String::trim)
                      .collect(Collectors.toList());

        for (int i=0;i<nodes.size();i++) {
            node=nodes.get(i);
            String[] parts = node.split(":");
            Jedis jedis = new Jedis(parts[0], Integer.parseInt(parts[1]));
            publishers.add(jedis);
            if(i==SELF_INDEX){
                self_jedis=jedis;
            }
        }
    }

    public void subscribeMessages(){
        JedisPubSub subscriber = new JedisPubSub() {
            @Override
            public void onMessage(String channel, String message) {
                JsonObject jsonObject = JsonParser.parseString(message).getAsJsonObject();
                if(jsonObject.get("action").getAsString()=="ADD"){
                    redisTemplate.opsForSet().add(jsonObject.get("key").getAsString(), jsonObject.get("endpoint").getAsString());
                }else if(jsonObject.get("action").getAsString()=="REMOVE"){
                    redisTemplate.opsForSet().remove(jsonObject.get("key").getAsString(), jsonObject.get("endpoint").getAsString());
                }
                System.out.println("Received & Locally Operated: " + message + " on channel: " + channel);
            }
        };
        self_jedis.subscribe(subscriber, CHANNEL_NAME);
    }

    private void publishMessage(String message){
        for (int i = 0; i < publishers.size(); i++) {
            if (i != SELF_INDEX) {
                publishers.get(i).publish(CHANNEL_NAME, message);
            }
        }
    }

    public void addIndexEntity(String key, String endpoint) {
        if (key != null && endpoint != null) {
            try {
                ObjectMapper objectMapper = new ObjectMapper();
                Map<String, String> message = new HashMap<>();
                
                //insert into the dragonfly memory
                redisTemplate.opsForSet().add(key, endpoint); 
                createdReindexFile();

                // Publish/broadcast JSON-based message
                //message.put("[from=" + SELF_INDEX + "]");
                message.put("endpoint", endpoint);
                message.put("key", key);
                message.put("action", "ADD");
                String jsonMessage = objectMapper.writeValueAsString(message);
                publishMessage(jsonMessage);
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
    }

    // Load/not-a-new-add a endpoint 
    public void addIndexEntityWithoutBroadcasting(String key, String endpoint) {
        if (key != null && endpoint != null) {
            //insert into the dragonfly memory
            redisTemplate.opsForSet().add(key, endpoint);
        }
    }

    // Remove endpoint & broadcast
    public void removeIndexEntity(String key, String endpoint) {
        try{
            if (key != null && endpoint != null) {
                redisTemplate.opsForSet().remove(key, endpoint);
                createdReindexFile();

                // Broadcast the REMOVE JSON-based message
                Map<String, String> message = new HashMap<>();
                message.put("endpoint", endpoint);
                message.put("key", key);
                message.put("action", "REMOVE");

                String jsonMessage = objectMapper.writeValueAsString(message);
                publishMessage(jsonMessage);
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public void removeIndexEntityWithoutBroadcasting(String key, String endpoint, String source_stack) {
        try{

            if (key != null && endpoint != null) {
                redisTemplate.opsForSet().remove(key, endpoint);
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    private int createdReindexFile(){
        try {
            File reindexing_file = new File(REINDEX_FILE); 
            if (reindexing_file.exists()) {
                return 2;
            }else{
                if (reindexing_file.createNewFile()) {
                    return 1;
                } else {
                    return 0;
                }
            }
        } catch (IOException e) {
            e.printStackTrace();
        }

        return 0;
    }

    // Retrieve all values from the Set using key
    public Set<String> getEndpoints(String key) {
        // Retrieve the Set of endpoints from Redis
        return redisTemplate.opsForSet().members(key);
    }
    
    // Serialize Dragonfly index to JSON file (backup data)**
    public void serialiseIndexData() throws IOException {
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

            //BASE_BACKUP_FILE 
            File base_backup_file = new File(BASE_BACKUP_FILE);
            if (base_backup_file.exists()) {
                DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd_HH-mm-ss");
                String timestamp = LocalDateTime.now().format(formatter);

                // 1. log the old file with timestamp in name
                File new_dumpFile = new File(DUMP_FILE_PREFIX+ "-" + timestamp + ".json");
                if (base_backup_file.renameTo(new_dumpFile)) {
                    System.out.println("Old index-file is dumped as: " + new_dumpFile.getName());
                }

                // 2. create the base file with current data
                objectMapper.writeValue(base_backup_file, backupData);
            }else{
                objectMapper.writeValue(base_backup_file, backupData);
            }
            System.out.println(" New index-data is saved as: " + base_backup_file.getName());

            // 3. remove reindex file
            deleteReindexFile();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    // Deserialize Dragonfly index from JSON file (restore data)**
    public void deserialiseIndexData() throws IOException {
        try {
            File base_backup_file = new File(BASE_BACKUP_FILE);
            
            if (base_backup_file.exists()) {
                // Use correct generic types for HashMap
                ObjectMapper objectMapper = new ObjectMapper();
                TypeReference<Map<String, Set<String>>> typeRef = new TypeReference<>() {
                };

                Map<String, Set<String>> backupData = objectMapper.readValue(base_backup_file, typeRef);

                // Now safely iterate over the restored backup data
                for (Map.Entry<String, Set<String>> entry : backupData.entrySet()) {
                    redisTemplate.opsForSet().add(entry.getKey(), entry.getValue().toArray(new String[0]));
                }
                System.out.println("Successfully restored Dragonfly index data from backup.");
                 
            } else {
                System.out.println("No backup file found. Skipping restore and Initializing index from store-endpoints.");
                this.initializeIndex();
                // request index data from other stacks

                //broadcast the retrived data from initialization
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    public int deleteReindexFile(){
        File reindexing_file = new File(REINDEX_FILE);
        if (reindexing_file.exists()) {
            if (reindexing_file.delete()) {
                System.out.println("Reindexing completed.");
                return 1;
            } else {
                System.out.println("Error in initialising reindexing file.");
                return -1;
            }
        }
        return -1;
    }

    public boolean existsReindexFile(){
        File reindexing_file = new File(REINDEX_FILE);
        return reindexing_file.exists()?true:false;
    }

    // Deserialize Dragonfly index from JSON file (restore data)**
    public int reindexing() throws IOException {
        try {
            File reindexing_file = new File(REINDEX_FILE);
            
            if (reindexing_file.exists()) {
                // Use correct generic types for HashMap
                ObjectMapper objectMapper = new ObjectMapper();
                TypeReference<Map<String, Set<String>>> typeRef = new TypeReference<>() {
                };

                Map<String, Set<String>> reindexingData = objectMapper.readValue(reindexing_file, typeRef);

                // Now safely iterate over the restored backup data
                for (Map.Entry<String, Set<String>> entry : reindexingData.entrySet()) {
                    redisTemplate.opsForSet().add(entry.getKey(), entry.getValue().toArray(new String[0]));
                }
                
                //remove status file if it exists
                if (reindexing_file.delete()) {
                    System.out.println("Successfully reindexing data.");
                    return 1;
                } else {
                    System.out.println("Error in initialising reindexing file.");
                    return -1;
                }   
                
            } else {
                System.out.println("Data is up-to-date.");
                return 0;
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
        return -1;
    } 

    /**
     * initializeIndex reads triples from all endpoints of a local blazegraph to create an index.
     * It does not broadcast initialised index data. 
     */
    public void initializeIndex() {
        int counter = 0;
        
        // read all available namespaces in the local blazegraph 
        Query queryForClassProperty = QueryFactory.create(extractClassPropertyQuery);
        List<String> namespaces=this.getNamespaces();
        
        //And create index for each of them
        for(String namespace : namespaces){
            String endpointUrl = BLAZEGRAPH_URL + "namespace/" + namespace + "/sparql";
            createIndex(queryForClassProperty, endpointUrl);
        }
        System.out.println("Loading completed.");
    }

    private void createIndex(Query query, String endpointUrl){
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

                if (this.stop_cps.contains(classUri)){
                    System.out.println("Ignore C: "+classUri+ "=>"+ endpointUrl);
                    continue;
                }
                if (!classUri.isEmpty()) {
                    addIndexEntityWithoutBroadcasting(classUri, endpointUrl);
                    System.out.println("C: "+classUri+ "=>"+ endpointUrl);
                }

                if (this.stop_cps.contains(propertyUri)){
                    System.out.println("Ignore P: "+propertyUri+ "=>"+ endpointUrl);
                    continue;
                }
                if (!propertyUri.isEmpty()) {
                    addIndexEntityWithoutBroadcasting(propertyUri, endpointUrl);
                    System.out.println("P: "+propertyUri+ "=>"+ endpointUrl);
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    /**
     * createIndexFrom reads triples from a specific endpoint to create index
     * It does not broadcast initialised index data.
     */
    public void createIndexFrom(String endpointUrl){
        Query queryForClassProperty = QueryFactory.create(extractClassPropertyQuery);
        createIndex(queryForClassProperty, endpointUrl);
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
