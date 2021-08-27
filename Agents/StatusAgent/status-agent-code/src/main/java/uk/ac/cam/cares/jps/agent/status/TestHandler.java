package uk.ac.cam.cares.jps.agent.status;

import java.lang.reflect.Constructor;
import java.util.ArrayList;
import java.util.List;
import org.apache.commons.lang.StringUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import uk.ac.cam.cares.jps.agent.status.define.TestDefinition;
import uk.ac.cam.cares.jps.agent.status.execute.TestExecutor;
import uk.ac.cam.cares.jps.agent.status.record.TestRecord;
import uk.ac.cam.cares.jps.agent.status.record.TestRecordStore;

/**
 * This class handles setting up the TestExecutor instances to run all registered tests in a serial
 * queue.
 *
 * @author Michael Hillman
 */
public class TestHandler {

    /**
     * Logger.
     */
    private static final Logger LOGGER = LogManager.getLogger(TestHandler.class);

    /**
     * List of tests to be run.
     */
    private static List<TestExecutor> PENDING_TESTS = new ArrayList<>();

    /**
     *
     */
    public static void runTests() {
        // Use reflection to create the correct TestExecutor for each definition 
        // and run the test.
        for (TestDefinition definition : TestRegistry.getDefinedTests()) {
            try {
                // Find the executor clas registered for that definition
                Class<? extends TestExecutor> executorClass = definition.getExecutorClass();

                // Create an instance of the executor
                Constructor<? extends TestExecutor> contrusctor = executorClass.getDeclaredConstructor(TestDefinition.class);
                TestExecutor executor = contrusctor.newInstance(new Object[]{definition});

                // Run the executor
                String message = "===== Executing '" + definition.getName() + "' test from '" + definition.getGroup() + "' group =====";
                System.out.println(message);
                executor.execute();
                System.out.println(StringUtils.leftPad("", message.length(), "="));
                System.out.println("");

                // Add the record to the store
                TestRecord record = executor.getRecord();
                if (record != null || record.getExecutionTime() != null) {
                    TestRecordStore.addRecord(record);
                }

            } catch (Exception exception) {
                LOGGER.error("Could not create/run TestExecutor instance for '" + definition.getName() + "' test.", exception);
            }
        }

        // Write the updated TestRecordStore to file
        TestRecordStore.writeRecords();
    }

    public static void main(String[] args) {
        runTests();
    }
}
// End of class.
