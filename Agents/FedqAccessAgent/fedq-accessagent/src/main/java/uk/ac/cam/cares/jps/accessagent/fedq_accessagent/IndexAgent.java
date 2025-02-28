package uk.ac.cam.cares.jps.accessagent.fedq_accessagent;

import java.io.File;
import java.io.IOException;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.stereotype.Service;
import org.springframework.beans.factory.annotation.Value;

@Service
public class IndexAgent {
    private static final String BACKUP_FILE = "/data/backup.json";
    private final ObjectMapper objectMapper = new ObjectMapper(); // JSON serializer

    @Autowired
    private StringRedisTemplate stringRedisTemplate; // Used for Pub/Sub

    @Autowired
    private RedisTemplate<String, String> redisTemplate;

    private final String STACK_ID;

    // Read stack ID from environment variable or default to "defaultStack"
    public IndexAgent(@Value("${stack.id:defaultStack}") String stackId) {
        this.STACK_ID = stackId;
        System.out.println("Initialized IndexAgent for Stack: " + STACK_ID);
    }

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
        Set<String> values = new HashSet<>();
        if (key != null) {
            String stackKey = STACK_ID + ":" + key;
            values = redisTemplate.opsForSet().members(stackKey);
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

    // Deserialize Dragonfly index from JSON file (restore data)**
    public void restoreFromBackup() {
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
                        addValue(key, value); // Reinsert into Redis
                    }
                }
                System.out.println("Successfully restored Dragonfly index data from backup.");
            } else {
                System.out.println("No backup file found. Skipping restore.");
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    // Serialize Dragonfly index to JSON file (backup data)**
    public void backupDataToJson() {
        try {
            Set<String> keys = redisTemplate.keys("*");
            if (keys != null) {
                var backupData = new java.util.HashMap<String, Set<String>>();
                for (String key : keys) {
                    backupData.put(key, getValues(key));
                }
                objectMapper.writeValue(new File(BACKUP_FILE), backupData);
                System.out.println("Backup saved to " + BACKUP_FILE);
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
