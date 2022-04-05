package uk.ac.cam.cares.jps.agent.status;

import java.util.HashMap;
import java.util.Map;
import uk.ac.cam.cares.jps.agent.status.define.TestType;
import uk.ac.cam.cares.jps.agent.status.execute.AgentTestExecutor;
import uk.ac.cam.cares.jps.agent.status.execute.AvailabilityTestExecutor;
import uk.ac.cam.cares.jps.agent.status.execute.QueryCountExecutor;
import uk.ac.cam.cares.jps.agent.status.execute.TestExecutor;

/**
 * Misc.
 *
 * @author Michael
 */
public class TestUtils {

    /**
     * Map for TestTypes to the Executor class used to run those tests.
     */
    private static final Map<TestType, Class<? extends TestExecutor>> EXECUTORS = new HashMap<>();

    /**
     * Static initializer
     */
    static {
        EXECUTORS.put(TestType.AVAILABILITY, AvailabilityTestExecutor.class);
        EXECUTORS.put(TestType.QUERY_COUNT, QueryCountExecutor.class);
        EXECUTORS.put(TestType.AGENT_STATUS, AgentTestExecutor.class);
    }

    /**
     * Return the TestExecutor class used for the input test type.
     *
     * @param type
     * @return
     */
    public static Class<? extends TestExecutor> getExecutorForType(TestType type) {
        return EXECUTORS.get(type);
    }

    /**
     *
     * @param type
     * @return
     */
    public static String getDescription(TestType type) {
        switch (type) {

            // Endpoint availability
            case AVAILABILITY:
                return "These tests use the JPS Base Library to perform a simple SPARQL query using "
                        + "the currently set 'endpoint'. This query returns the number of triples defined "
                        + "in the KG, and the test is considered a success if this number is above zero.";
                
            // Counting data sets
            case QUERY_COUNT:
                return "These tests use the JPS Base Library to perform a custom SPARQL query using "
                        + "the currently set 'endpoint'. This query counts matching triples and passes if "
                        + "the returned 'RESULT' field is above the set 'minimum' value.";

            // Agent status
            case AGENT_STATUS:
                return "These tests attempt to contact a remote JSP Agent using the currently set 'url'. The "
                        + "'/status' URL pattern is used and a JSON response (content does not matter) with "
                        + "a valid HTTP code is required for the test to pass.";
        }

        return "No description has been set for this test type.";
    }
}
