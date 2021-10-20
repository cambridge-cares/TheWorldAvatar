package uk.ac.cam.cares.jps.agent.status;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import org.junit.jupiter.api.Test;
import uk.ac.cam.cares.jps.agent.status.define.TestType;
import uk.ac.cam.cares.jps.agent.status.execute.TestExecutor;

/**
 *
 * @author Michael
 */
public class TestUtils_Test {

    /**
     * Checks that TestType has an associated TestExecutor class.
     */
    @Test
    public void checkExecutors() {
        for (TestType type : TestType.values()) {
            Class<? extends TestExecutor> clazz = TestUtils.getExecutorForType(type);
            assertNotNull(clazz, "Could not find a TestExecutor class registered for test type " + type);
        }
    }

    /**
     * Checks that TestType has an associated description;
     */
    @Test
    public void checkDescriptions() {
        for (TestType type : TestType.values()) {
            String description = TestUtils.getDescription(type);
            assertNotNull(description, "Could not find a description for test type " + type);
        }
    }
}
// End of class.
