package uk.ac.cam.cares.jps.accessagent.fedq_accessagent;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.event.ContextClosedEvent;
import org.springframework.context.event.EventListener;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.redis.connection.Message;
import org.springframework.data.redis.connection.MessageListener;
import org.springframework.boot.context.event.ApplicationReadyEvent;
import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Component;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.stereotype.Service;
import com.fasterxml.jackson.databind.ObjectMapper;

import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.Set;
import java.util.Map;
import java.util.HashMap;

@Service
public class IndexListener implements MessageListener {
    private static final String DATA_DIRECTORY = "/data";
    private static final String REINDEX_FILE = DATA_DIRECTORY + "/reindex.json";
    private final ObjectMapper objectMapper = new ObjectMapper();

    @Autowired
    private RedisTemplate<String, String> redisTemplate;

    @Autowired
    private IndexAgent indexAgent; // Access the main index agent

    // Read stack ID from environment variable or default to "defaultStack"
    private final String STACK_ID;
    public IndexListener(@Value("${stack.id:defaultStack}") String stackId) {
        this.STACK_ID = stackId;
        System.out.println("Initialized IndexAgent for Stack: " + STACK_ID);
    }

    @Override
    public void onMessage(Message message, byte[] pattern) {
        try{

            String receivedMessage = new String(message.getBody(), StandardCharsets.UTF_8);
            System.out.println("Received Broadcast: " + receivedMessage);
    
            // Parse JSON
            ObjectMapper objectMapper = new ObjectMapper();
            Map<String, String> data = objectMapper.readValue(receivedMessage, Map.class);
    
            String sourceStack = data.get("stack");
            String action = data.get("action");
            String key = data.get("key");
            String endpoint = data.get("endpoint");
    
            // Ignore messages from this stack to prevent duplicates
            if (STACK_ID.equals(sourceStack)) {
                return;
            }
    
            // Perform the corresponding action
            if ("ADD".equalsIgnoreCase(action)) {
                indexAgent.addIndexEntity(key, endpoint, sourceStack);
                updateReindexFile(key, true);
                System.out.println("Synced ADD from " + sourceStack + " → " + key + " = " + endpoint);
            } else if ("REMOVE".equalsIgnoreCase(action)) {
                indexAgent.removeIndexEntity(key, endpoint, sourceStack);
                updateReindexFile(key, false);
                System.out.println("Synced REMOVE from " + sourceStack + " → " + key + " = " + endpoint);
            } else {
                System.err.println("Unknown action: " + action);
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    private synchronized void updateReindexFile(String key, boolean isAdd) {
        try {
            File file = new File(REINDEX_FILE);
            Map<String, Set<String>> reindexData = new HashMap<>();

            // Load existing data if the file exists
            if (file.exists()) {
                reindexData = objectMapper.readValue(file, HashMap.class);
            }

            if (isAdd) {
                // Add key values from Redis
                Set<String> jsonSet = redisTemplate.opsForSet().members(key);
                reindexData.put(key, jsonSet);
            } else {
                // Remove key
                reindexData.remove(key);
            }

            // Write back to file
            objectMapper.writeValue(file, reindexData);
            System.out.println("Updated backup.json successfully.");

        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    @EventListener(ContextClosedEvent.class)
    public void onShutdown() {
        try {
            indexAgent.serialiseIndexData();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    @EventListener(ApplicationReadyEvent.class)
    public void onStartup() {
        try {
            indexAgent.deserialiseIndexData();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
