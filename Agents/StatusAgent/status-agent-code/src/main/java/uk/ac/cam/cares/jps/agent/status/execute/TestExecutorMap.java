package uk.ac.cam.cares.jps.agent.status.execute;

import java.util.HashMap;
import java.util.Map;
import uk.ac.cam.cares.jps.agent.status.define.TestType;

/**
 *
 * @author Michael
 */
public class TestExecutorMap {

    /**
     *
     */
    private static final Map<TestType, Class<? extends TestExecutor>> MAP = new HashMap<>();

    /**
     *
     */
    static {
        MAP.put(TestType.AVAILABILITY, AvailabilityTestExecutor.class);
    }

    /**
     *
     * @param type
     * @return
     */
    public static Class<? extends TestExecutor> getExecutorForType(TestType type) {
        return MAP.get(type);
    }

}
