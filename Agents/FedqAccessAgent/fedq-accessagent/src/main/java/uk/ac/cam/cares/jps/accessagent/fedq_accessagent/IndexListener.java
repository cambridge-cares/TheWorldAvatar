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

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.Set;
import java.util.Map;

@Service
public class IndexListener implements MessageListener {

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
                indexAgent.loadValue(key, endpoint, sourceStack);
                System.out.println("Synced ADD from " + sourceStack + " → " + key + " = " + endpoint);
            } else if ("REMOVE".equalsIgnoreCase(action)) {
                indexAgent.removeValueWithoutBroadcast(key, endpoint, sourceStack);
                System.out.println("Synced REMOVE from " + sourceStack + " → " + key + " = " + endpoint);
            } else {
                System.err.println("Unknown action: " + action);
            }
        } catch (Exception e) {
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
