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
    private static final String BACKUP_DIRECTORY = "/data";
    private static final String BACKUP_FILE = BACKUP_DIRECTORY + "/backup.json";
    private final ObjectMapper objectMapper = new ObjectMapper();

    @Autowired
    private RedisTemplate<String, String> redisTemplate;

    @Autowired
    private IndexAgent indexAgent; // Access the main index agent

    private final String STACK_ID;

    // Read stack ID from environment variable or default to "defaultStack"
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
                indexAgent.addValue(key, endpoint, sourceStack);
                updateBackupFile(key, true);
                System.out.println("Synced ADD from " + sourceStack + " → " + key + " = " + endpoint);
            } else if ("REMOVE".equalsIgnoreCase(action)) {
                indexAgent.removeValue(key, endpoint, sourceStack);
                updateBackupFile(key, false);
                System.out.println("Synced REMOVE from " + sourceStack + " → " + key + " = " + endpoint);
            } else {
                System.err.println("Unknown action: " + action);
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    private synchronized void updateBackupFile(String key, boolean isAdd) {
        try {
            File file = new File(BACKUP_FILE);
            Map<String, Set<String>> backupData = new HashMap<>();

            // Load existing data if the file exists
            if (file.exists()) {
                backupData = objectMapper.readValue(file, HashMap.class);
            }

            if (isAdd) {
                // Add key values from Redis
                Set<String> jsonSet = redisTemplate.opsForSet().members(key);
                backupData.put(key, jsonSet);
            } else {
                // Remove key
                backupData.remove(key);
            }

            // Write back to file
            objectMapper.writeValue(file, backupData);
            System.out.println("Updated backup.json successfully.");

        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    @EventListener(ContextClosedEvent.class)
    public void onShutdown() {
        try {
            indexAgent.backupDataToJson();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    @EventListener(ApplicationReadyEvent.class)
    public void onStartup() {
        try {
            indexAgent.restoreFromBackup();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
