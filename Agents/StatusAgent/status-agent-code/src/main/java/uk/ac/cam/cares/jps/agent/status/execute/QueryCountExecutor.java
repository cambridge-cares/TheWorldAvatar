package uk.ac.cam.cares.jps.agent.status.execute;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.ThreadContext;
import org.apache.logging.log4j.core.LoggerContext;
import org.json.JSONArray;
import org.json.JSONObject;
import uk.ac.cam.cares.jps.agent.status.define.TestDefinition;
import uk.ac.cam.cares.jps.agent.status.record.TestRecord;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

/**
 * Simple test class that reads a SPARQL query from file and runs it against the current KG
 * endpoint.
 *
 * @author Michael Hillman
 */
public class QueryCountExecutor extends TestExecutor {

    /**
     * Initialise a new QueryExecutor instance with the input test definition.
     *
     * @param definition Definition of test to run.
     */
    public QueryCountExecutor(TestDefinition definition) {
        super(definition);
    }

    /**
     * Attempts to contact the KG endpoint and count the number of triples present via the JPS Base
     * Library.
     */
    @Override
    public void execute() {
        // Generate the test record
        record = new TestRecord();
        record.setDefinition(definition);

        // Mark the test time
        record.markTime();

        // Set logging context
        ThreadContext.put("groupName", definition.getType().toString());
        ThreadContext.put("testName", definition.getName());
        ThreadContext.put("testTime", record.getExecutionTime());
        ((LoggerContext) LogManager.getContext(false)).reconfigure();

        // Get the endpoint
        String endpoint = definition.getInput("endpoint");
        if (endpoint == null || endpoint.isEmpty()) {
            LOGGER.error("Could not find required 'endpoint' input.");
            record.setResult(false);
            return;
        }
        LOGGER.info("Read 'endpoint' as: " + endpoint);

        // Get the SPARQL query file
        String queryFile = definition.getInput("query-file");
        if (queryFile == null || queryFile.isEmpty()) {
            LOGGER.error("Could not find required 'query-file' input.");
            record.setResult(false);
            return;
        }
        LOGGER.info("Read 'query-file' as: " + queryFile);

        // Get the SPARQL query
        String query = QueryStore.readQuery(queryFile);
        if (query == null || query.isEmpty()) {
            LOGGER.error("Could not find/read the query file!");
            record.setResult(false);
            return;
        }
        LOGGER.info("Query file has been read.");

        // Get minimum acceptable count
        String minimum = definition.getInput("minimum");
        if (minimum == null || minimum.isEmpty()) {
            LOGGER.error("Could not find required 'minimum' input.");
            record.setResult(false);
            return;
        }
        LOGGER.info("Checking against 'minimum' of: " + minimum);
        

        try {
            // Initialise the client
            RemoteStoreClient kgClient = new RemoteStoreClient(endpoint);

            // Pass credentials (if present)
            if (definition.getUsername() != null && definition.getPassword() != null) {
                kgClient.setUser(definition.getUsername());
                kgClient.setPassword(definition.getPassword());
                LOGGER.info("Using credentials for KG access.");
            } else {
                LOGGER.info("No credentials set for KG access, skipping.");
            }

            // Run the query
            LOGGER.info("Running query.");
            JSONArray results = kgClient.executeQuery(query);
            LOGGER.info("Query finished.");

            // Parse the result
            JSONObject firstResult = results.getJSONObject(0);

            // Get the result
            int resultCount = firstResult.optInt("RESULT");
            LOGGER.info("'RESULT' field determined as: " + resultCount);

            // Store in record
            record.setResult(resultCount >= Integer.parseInt(minimum));
            if (record.getResult()) {
                LOGGER.info("Test passes successfully.");
            } else {
                LOGGER.error("Test has failed.");
            }

        } catch (Exception exception) {
            // Catch everything as the base library throws some strange stuff
            LOGGER.error("Exception occurred during test execution.", exception);
            record.setResult(false);
        }
    }

}
// End of class.
