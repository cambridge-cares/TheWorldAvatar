package uk.ac.cam.cares.jps.agent.status.execute;

import java.util.HashMap;
import java.util.Map;
import uk.ac.cam.cares.jps.agent.status.define.TestType;

/**
 * Map for TestTypes to the Executor class used to run those tests.
 *
 * @author Michael
 */
public class TestExecutorMap {

    /**
     * Map for TestTypes to the Executor class used to run those tests.
     */
    private static final Map<TestType, Class<? extends TestExecutor>> MAP = new HashMap<>();

    /**
     * Static initialier
     */
    static {
        MAP.put(TestType.AVAILABILITY, AvailabilityTestExecutor.class);
    }

    /**
     * Return the TestExecutor class used for the input test type.
     *
     * @param type
     * @return
     */
    public static Class<? extends TestExecutor> getExecutorForType(TestType type) {
        return MAP.get(type);
    }

}
