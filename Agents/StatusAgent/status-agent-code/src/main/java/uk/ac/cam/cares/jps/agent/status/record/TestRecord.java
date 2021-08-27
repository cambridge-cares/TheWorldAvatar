package uk.ac.cam.cares.jps.agent.status.record;

import uk.ac.cam.cares.jps.agent.status.define.TestDefinition;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.text.SimpleDateFormat;
import java.util.Date;

/**
 * This class describes the results of a single test execution.
 *
 * @author Michael Hillman
 */
public class TestRecord<T extends TestDefinition> {

    /**
     * Test definition used to generate this execution.
     */
    protected final T definition;

    /**
     * The time at which this test instance was executed.
     */
    protected String testTime;

    /**
     * Result of the test execution.
     */
    protected boolean result;

    /**
     * Initialise a new TestExecution instance.
     *
     * @param definition Test definition used to generate this execution.
     */
    public TestRecord(T definition) {
        this.definition = definition;
    }

    /**
     * Returns the test definition used to generate this execution.
     *
     * @return test definition.
     */
    public T getDefinition() {
        return definition;
    }

    /**
     * Returns the time at which the test was executed (or null).
     * 
     * @return test execution time.
     */
    public String getExecutionTime() {
        return testTime;
    }
    
    /**
     * Returns the result of the test execution.
     *
     * @return test result.
     */
    public boolean getResult() {
        return result;
    }

    /**
     * Sets the result of the test execution.
     *
     * @param result desired result.
     */
    public void setResult(boolean result) {
        this.result = result;
    }

    /**
     * Assuming that the committed log4j2 configuration hasn't changed, this returns the location of
     * the log file for this test execution.
     *
     * @return log file location
     */
    public String getLogFileLocation() {
        if (testTime == null) {
            throw new IllegalStateException("Test has not yet been executed!");
        }

        String userHome = System.getProperty("user.home");
        Path logFile = Paths.get(
                userHome,
                ".jps",
                "logs",
                definition.getGroup(),
                definition.getName(),
                testTime + ".log"
        );
        return logFile.toString();
    }

    /**
     * Store the current time.
     */
    public void markTime() {
        SimpleDateFormat formatter = new SimpleDateFormat("yyyy-MM-dd_HH-mm-ss");
        testTime = formatter.format(new Date());
    }

}
// End of class.
