package uk.ac.cam.cares.jps.agent.status;

import java.lang.reflect.Constructor;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.apache.logging.log4j.ThreadContext;
import org.apache.logging.log4j.core.LoggerContext;
import uk.ac.cam.cares.jps.agent.status.define.TestDefinition;
import uk.ac.cam.cares.jps.agent.status.define.TestType;
import uk.ac.cam.cares.jps.agent.status.execute.TestExecutor;
import uk.ac.cam.cares.jps.agent.status.record.TestRecord;
import uk.ac.cam.cares.jps.agent.status.record.TestRecordStore;
import uk.ac.cam.cares.jps.agent.status.record.TestRecordStoreMarshaller;

/**
 * This class handles setting up the TestExecutor instances to run tests.
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
     * Initialise a new TestHandler instance.
     */
    public TestHandler() {
        recordStore = TestRecordStoreMarshaller.readRecords();
    }

    /**
     * Return the current RecordStore instance.
     *
     * @return
     */
    public TestRecordStore getRecordStore() {
        return recordStore;
    }

    /**
     * Execute all registered tests in serial.
     *
     * @return false if ANY tests fail.
     */
    public synchronized boolean runAllTests() {
        boolean allSuccess = true;

        // Run all the tests
        for (TestDefinition definition : TestRegistry.getDefinedTests()) {
            boolean singleSuccess = runTest(definition);
            if (!singleSuccess) allSuccess = false;
        }

        return allSuccess;
    }

    /**
     * Run a single test.
     *
     * @param testName test name.
     * @param testType test type.
     * @return test result.
     */
    public synchronized boolean runTest(String testName, String testType) {
        TestDefinition definition = null;
        try {
            definition = TestRegistry.getDefinedTest(testName, TestType.valueOf(testType));
        } catch (Exception exception) {
            // Could happen if testType is an invalid value
            return false;
        }

        if (definition != null) {
            return runTest(definition);
        }
        return false;
    }

    /**
     * Run a single test.
     *
     * @param definition test definition.
     * @return test result.
     */
    public boolean runTest(TestDefinition definition) {
        try {
            // Find the executor class registered for that definition
            Class<? extends TestExecutor> executorClass = TestUtils.getExecutorForType(definition.getType());

            // Create an instance of the executor
            Constructor<? extends TestExecutor> contrusctor = executorClass.getDeclaredConstructor(TestDefinition.class);
            TestExecutor executor = contrusctor.newInstance(new Object[]{definition});

            // Run the executor
            LOGGER.info("Executing '" + definition.getName() + "' from '" + definition.getType() + "' tests");
            executor.execute();

            // Clear the logging context
            ThreadContext.clearAll();
            ((LoggerContext) LogManager.getContext(false)).reconfigure();

            // Add the record to the store
            TestRecord record = executor.getRecord();
            
            if (record != null && record.getExecutionTime() != null) {
                recordStore.addRecord(record);
                
                // Write the updated TestRecordStore to file
                TestRecordStoreMarshaller.writeRecords(recordStore);
                return record.getResult();
            }
            return false;

        } catch (Exception exception) {
            LOGGER.error("Could not create/run TestExecutor instance for '" + definition.getName() + "' test.", exception);
            return false;
        }
    }

}
// End of class.
