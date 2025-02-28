package uk.ac.cam.cares.jps.accessagent.fedq_accessagent;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.redis.connection.Message;
import org.springframework.data.redis.connection.MessageListener;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.stereotype.Service;
import java.nio.charset.StandardCharsets;
import java.util.Set;

@Service
public class IndexBroadcastListener implements MessageListener {

    @Autowired
    private RedisTemplate<String, String> redisTemplate;

    @Autowired
    private IndexAgent indexAgent; // Access the main index agent

    private final String STACK_ID;

    // Read stack ID from environment variable or default to "defaultStack"
    public IndexBroadcastListener(@Value("${stack.id:defaultStack}") String stackId) {
        this.STACK_ID = stackId;
        System.out.println("Initialized IndexAgent for Stack: " + STACK_ID);
    }

    @Override
    public void onMessage(Message message, byte[] pattern) {
        String receivedMessage = new String(message.getBody(), StandardCharsets.UTF_8);
        System.out.println("Received Broadcast: " + receivedMessage);

        String[] parts = receivedMessage.split(":", 4);
        if (parts.length < 4) {
            System.err.println("Invalid message format: " + receivedMessage);
            return;
        }

        String sourceStack = parts[0];
        String action = parts[1]; // "ADD" or "REMOVE"
        String key = parts[2];
        String value = parts[3];

        // Ignore messages from this stack to prevent duplicates
        if (STACK_ID.equals(sourceStack)) {
            return;
        }

        // Perform the corresponding action
        if ("ADD".equalsIgnoreCase(action)) {
            indexAgent.addValue(key, value);
            System.out.println("Synced ADD from " + sourceStack + " → " + key + " = " + value);
        } else if ("REMOVE".equalsIgnoreCase(action)) {
            indexAgent.removeValue(key, value);
            System.out.println("Synced REMOVE from " + sourceStack + " → " + key + " = " + value);
        } else {
            System.err.println("Unknown action: " + action);
        }
    }
}
