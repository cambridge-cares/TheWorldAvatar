package uk.ac.cam.cares.jps.agent.status;

import java.lang.reflect.Constructor;
import java.util.ArrayList;
import java.util.List;
import org.apache.commons.lang.StringUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import uk.ac.cam.cares.jps.agent.status.define.TestDefinition;
import uk.ac.cam.cares.jps.agent.status.execute.TestExecutor;
import uk.ac.cam.cares.jps.agent.status.execute.TestExecutorMap;
import uk.ac.cam.cares.jps.agent.status.record.TestRecord;
import uk.ac.cam.cares.jps.agent.status.record.TestRecordStore;
import uk.ac.cam.cares.jps.agent.status.record.TestRecordStoreMarshaller;

/**
 * This class handles setting up the TestExecutor instances to run all registered tests in a single,
 * serial queue.
 *
 * @author Michael Hillman
 */
public class TestHandler {

    /**
     * Logger.
     */
    private static final Logger LOGGER = LogManager.getLogger(TestHandler.class);

    /**
     * Historical test results.
     */
    private final TestRecordStore recordStore;

    /**
     * List of tests to be run.
     */
    private final List<TestExecutor> pendingTests = new ArrayList<>();

    /**
     * Initialise a new TestHandler instance.
     */
    public TestHandler() {
        recordStore = TestRecordStoreMarshaller.readRecords();
    }
    
    /**
     * 
     * @return 
     */
    public TestRecordStore getRecordStore() {
        return recordStore;
    }

    /**
     *
     */
    public void runTests() {
        for (TestDefinition definition : TestRegistry.getDefinedTests()) {

            try {
                // Find the executor class registered for that definition
                Class<? extends TestExecutor> executorClass = TestExecutorMap.getExecutorForType(definition.getType());

                // Create an instance of the executor
                Constructor<? extends TestExecutor> contrusctor = executorClass.getDeclaredConstructor(TestDefinition.class);
                TestExecutor executor = contrusctor.newInstance(new Object[]{definition});

                // Run the executor
                String message = "===== Executing '" + definition.getName() + "' from '" + definition.getType() + "' tests =====";
                System.out.println(message);
                executor.execute();
                System.out.println(StringUtils.leftPad("", message.length(), "="));
                System.out.println("");
                
                // Add the record to the store
                TestRecord record = executor.getRecord();
                if (record != null || record.getExecutionTime() != null) {
                    recordStore.addRecord(record);
                }

            } catch (Exception exception) {
                LOGGER.error("Could not create/run TestExecutor instance for '" + definition.getName() + "' test.", exception);
            }
        }

        // Write the updated TestRecordStore to file
        TestRecordStoreMarshaller.writeRecords(recordStore);
    }

    public static void main(String[] args) {
        new TestHandler().runTests();
    }
}
// End of class.
