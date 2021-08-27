package uk.ac.cam.cares.jps.agent.status.execute;

import org.apache.logging.log4j.ThreadContext;
import org.json.JSONArray;
import org.json.JSONObject;
import uk.ac.cam.cares.jps.agent.status.define.AvailabilityTestDefinition;
import uk.ac.cam.cares.jps.agent.status.define.TestDefinition;
import uk.ac.cam.cares.jps.agent.status.record.TestRecord;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

/**
 * Simple test class that checks that a given KG endpoint it accessible.
 *
 * @author Michael Hillman
 */
public class AvailabilityTestExecutor extends TestExecutor {

    /**
     * Initialise a new AvailabilityTestDefinition instance with the input test definition.
     *
     * @param definition Definition of test to run.
     */
    public AvailabilityTestExecutor(TestDefinition definition) {
        super(definition);
    }

    /**
     * Attempts to contact the KG endpoint and count the number of triples present via the JPS Base
     * Library.
     */
    @Override
    public void execute() {

        // Check that the store definition is of the correct type.
        // This is not ideal, but I couldn't get generics to work with the relfexsive
        // creation of testExecutor instances in the TestHandler class - Michael
        AvailabilityTestDefinition concreteDefinition = null;
        try {
            concreteDefinition = (AvailabilityTestDefinition) definition;
        } catch (ClassCastException exception) {
            LOGGER.error("Stored test definition was not an instance of AvailabilityTestDefinition!", exception);
            record.setResult(false);
            return;
        }

        // Generate the test record
        record = new TestRecord<>(definition);

        // Mark the test time
        record.markTime();

        // Set logging context
        ThreadContext.put("groupName", definition.getGroup());
        ThreadContext.put("testName", definition.getName());
        ThreadContext.put("testTime", record.getExecutionTime());

        try {
            // Initialise the client
            RemoteStoreClient kgClient = new RemoteStoreClient(concreteDefinition.getEndpoint());

            // Pass credentials (if present)
            if (concreteDefinition.getUsername() != null && concreteDefinition.getPassword() != null) {
                kgClient.setUser(concreteDefinition.getUsername());
                kgClient.setPassword(concreteDefinition.getPassword());
                LOGGER.info("Using credentials for KG access.");
            } else {
                LOGGER.info("No credentials set for KG access, skipping.");
            }

            // Run the query
            LOGGER.info("Running query.");
            JSONArray results = kgClient.executeQuery(AvailabilityTestDefinition.QUERY);
            LOGGER.info("Query finished.");

            // Parse the result
            LOGGER.info("Full result: " + results.toString());
            JSONObject firstResult = results.getJSONObject(0);

            // Count the triples
            int triples = firstResult.optInt("NO_OF_TRIPLES");
            LOGGER.info("Triple count determined as: " + triples);

            // Store in record
            record.setResult(triples > 0);

        } catch (Exception exception) {
            // Catch everything as the base library throws some strange stuff
            LOGGER.error("Exception occurred during test execution.", exception);
            record.setResult(false);
        }
    }

}
// End of class.
