package uk.ac.cam.cares.jps.accessagent.fedq_accessagent;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import org.apache.jena.query.Query;
import org.apache.jena.query.QueryFactory;
import org.apache.jena.query.QueryParseException;
import org.apache.jena.update.UpdateAction;
import org.apache.jena.update.UpdateFactory;
import org.apache.jena.update.UpdateRequest;

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
    public ResponseEntity<Set<String>> getEndpoints(@RequestParam String key) {
        Set<String> values = indexAgent.getEndpoints(key);
        return ResponseEntity.ok(values);
    }

    @PostMapping("/classify")
    public ResponseEntity<String> classifySPARQL(@RequestBody String sparql) {
        int result = isQuery(sparql);
        String responseMessage = switch (result) {
            case 1 -> "SPARQL Query (Read Operation)";
            case 0 -> "SPARQL Update (Write Operation)";
            default -> "Invalid SPARQL Syntax";
        };
        return ResponseEntity.ok(responseMessage);
    }

    public int isQuery(String sparql) {
        try {
            // Try parsing as a SPARQL Query (SELECT, ASK, DESCRIBE, CONSTRUCT)
            Query query = QueryFactory.create(sparql);
            return 1;
        } catch (QueryParseException e) {
            try {
                // Try parsing as a SPARQL Update (INSERT, DELETE, LOAD, CLEAR, etc.)
                UpdateRequest updateRequest = UpdateFactory.create(sparql);
                return 0;
            } catch (Exception ex) {
                return -1;
            }
        }
    }
}
