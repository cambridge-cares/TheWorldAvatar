package uk.ac.cam.cares.jps.accessagent.fedq_accessagent;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.Set;

@RestController
@RequestMapping("/api/index")
public class IndexAgentCaller {

    @Autowired
    private IndexAgent indexAgent;

    @PostMapping("/add")
    public ResponseEntity<String> addValue(@RequestParam String key, @RequestParam String value) {
        indexAgent.addValue(key, value);
        return ResponseEntity.ok("Value added successfully: " + value);
    }

    @GetMapping("/get")
    public ResponseEntity<Set<String>> getValues(@RequestParam String key) {
        Set<String> values = indexAgent.getValues(key);
        return ResponseEntity.ok(values);
    }

    public Set<String> getValuesCL(String key) {
        Set<String> values = indexAgent.getValues(key);
        return values;
    }
}
