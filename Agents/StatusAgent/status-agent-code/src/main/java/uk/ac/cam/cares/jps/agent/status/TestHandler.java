package uk.ac.cam.cares.jps.agent.status;

import java.lang.reflect.Constructor;
import java.util.ArrayList;
import java.util.List;
import org.apache.commons.lang.StringUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.apache.logging.log4j.ThreadContext;
import uk.ac.cam.cares.jps.agent.status.define.TestDefinition;
import uk.ac.cam.cares.jps.agent.status.define.TestType;
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
    public synchronized void runAllTests() {
        for (TestDefinition definition : TestRegistry.getDefinedTests()) {
            runTest(definition);
        }
    }

    /**
     *
     * @param testName
     * @param testType
     * @return
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
     *
     * @param definition
     * @return
     */
    public boolean runTest(TestDefinition definition) {
        try {
            // Find the executor class registered for that definition
            Class<? extends TestExecutor> executorClass = TestExecutorMap.getExecutorForType(definition.getType());

            // Create an instance of the executor
            Constructor<? extends TestExecutor> contrusctor = executorClass.getDeclaredConstructor(TestDefinition.class);
            TestExecutor executor = contrusctor.newInstance(new Object[]{definition});

            // Run the executor
            LOGGER.info("Executing '" + definition.getName() + "' from '" + definition.getType() + "' tests");
            executor.execute();
            ThreadContext.clearAll();

            // Add the record to the store
            TestRecord record = executor.getRecord();
            if (record != null || record.getExecutionTime() != null) {
                recordStore.addRecord(record);
            }

            // Write the updated TestRecordStore to file
            TestRecordStoreMarshaller.writeRecords(recordStore);
            return true;

        } catch (Exception exception) {
            LOGGER.error("Could not create/run TestExecutor instance for '" + definition.getName() + "' test.", exception);
            return false;
        }
    }
    
    public static void main(String[] args) {
        TestHandler handler = new TestHandler();
        handler.runAllTests();
        
        System.out.println("ehwewehwh");
        handler.runTest("dev/ontogasgrid", "AVAILABILITY");
        
        System.out.println("done");
    }

}
// End of class.
