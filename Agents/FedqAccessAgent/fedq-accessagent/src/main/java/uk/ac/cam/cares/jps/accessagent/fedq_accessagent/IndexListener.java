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
        } catch (Exception e) {
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
