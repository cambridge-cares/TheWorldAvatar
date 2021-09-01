package uk.ac.cam.cares.jps.agent.status.record;

import uk.ac.cam.cares.jps.agent.status.define.TestDefinition;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

/**
 * This class describes the results of a single test execution.
 *
 * @author Michael Hillman
 */
public final class TestRecord implements Comparable {

    /**
     * Logger.
     */
    private static final Logger LOGGER = LogManager.getLogger(TestRecord.class);

    /**
     * Test definition used to generate this execution.
     */
    protected TestDefinition definition;

    /**
     * The time at which this test instance was executed.
     */
    protected String testTime;

    /**
     * Result of the test execution.
     */
    protected boolean result;

    /**
     * Returns the test definition used to generate this execution.
     *
     * @return test definition.
     */
    public TestDefinition getDefinition() {
        return definition;
    }

    /**
     *
     * @param definition
     */
    public void setDefinition(TestDefinition definition) {
        this.definition = definition;
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
     *
     * @param testTime
     */
    public void setExecutionTime(String testTime) {
        this.testTime = testTime;
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
                definition.getClass().getSimpleName(),
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

    /**
     *
     * @param obj
     * @return
     */
    @Override
    public int compareTo(Object obj) {
        if (obj == null) return -1;
        if (!(obj instanceof TestRecord)) return -1;

        TestRecord that = (TestRecord) obj;
        SimpleDateFormat formatter = new SimpleDateFormat("yyyy-MM-dd_HH-mm-ss");

        try {
            Date thisDate = formatter.parse(this.getExecutionTime());
            Date thatDate = formatter.parse(that.getExecutionTime());
            return thisDate.compareTo(thatDate);
        } catch (NullPointerException | ParseException exception) {
            LOGGER.warn("Could not compare TestRecord instances!", exception);
            return 0;
        }
    }

}
// End of class.
