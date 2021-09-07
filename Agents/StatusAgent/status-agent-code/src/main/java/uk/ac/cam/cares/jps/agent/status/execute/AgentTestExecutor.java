package uk.ac.cam.cares.jps.agent.status.execute;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.ThreadContext;
import org.apache.logging.log4j.core.LoggerContext;
import org.json.JSONArray;
import org.json.JSONObject;
import uk.ac.cam.cares.jps.agent.status.define.TestDefinition;
import uk.ac.cam.cares.jps.agent.status.record.TestRecord;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

/**
 * Simple test class that checks that an agent is accessible and can return content from the
 * "/status" URL pattern.
 *
 * @author Michael Hillman
 */
public class AgentTestExecutor extends TestExecutor {

    /**
     * Initialise a new AgentTestExecutor instance with the input test definition.
     *
     * @param definition Definition of test to run.
     */
    public AgentTestExecutor(TestDefinition definition) {
        super(definition);
    }

    /**
     * Attempts to contact the Agent's "/status" URL pattern and expects to see some content and an
     * acceptable return code.
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

        // Get the agent URL
        String url = definition.getInput("url");
        if (url == null || url.isEmpty()) {
            LOGGER.error("Could not find required 'url' input.");
            record.setResult(false);
            return;
        }
        url += (url.endsWith("/")) ? "status" : "/status";

        try {
            LOGGER.info("Running request.");
            String result = AgentCaller.executeGet(url);
            LOGGER.info("Request finished");

            // Parse the result
            LOGGER.info("Response length: " + ((result != null) ? result.length() : 0));
            record.setResult(result != null && !result.isBlank());

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

        ThreadContext.clearAll();
    }

}
// End of class.
