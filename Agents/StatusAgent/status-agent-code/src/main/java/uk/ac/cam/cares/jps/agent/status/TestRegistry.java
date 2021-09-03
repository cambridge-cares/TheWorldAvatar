package uk.ac.cam.cares.jps.agent.status;

import java.util.Collections;
import java.util.LinkedHashSet;
import java.util.Set;
import uk.ac.cam.cares.jps.agent.status.define.TestDefinition;
import uk.ac.cam.cares.jps.agent.status.define.TestType;

/**
 * Stores tests to be run.
 *
 * @author Michael Hillman
 */
public class TestRegistry {

    /**
     * Tests to be executed.
     */
    private static final Set<TestDefinition> DEFINITIONS = new LinkedHashSet<>();

    // Generate definitions of tests to run. In future, these definitions should be read
    // from a file so that tests can be added without having to regenerate Docker images.
    static {

        // =========================================================== //
        // ===== Availability tests for development KG endpoints ===== //
        // =========================================================== //
        TestDefinition dev_ontogasgrid = new TestDefinition("dev/ontogasgrid", TestType.AVAILABILITY);
        dev_ontogasgrid.setInput("endpoint", "http://kg.cmclinnovations.com:81/blazegraph_geo/namespace/ontogasgrid/sparql");
        DEFINITIONS.add(dev_ontogasgrid);

        TestDefinition dev_landuse = new TestDefinition("dev/landuse", TestType.AVAILABILITY);
        dev_landuse.setInput("endpoint", "http://kg.cmclinnovations.com:81/blazegraph_geo/namespace/landuse/sparql");
        DEFINITIONS.add(dev_landuse);

        TestDefinition dev_backup = new TestDefinition("dev/ts_backup", TestType.AVAILABILITY);
        dev_backup.setInput("endpoint", "http://kg.cmclinnovations.com:81/blazegraph_geo/namespace/ts_backup/sparql");
        DEFINITIONS.add(dev_backup);

        // ========================================================== //
        // ===== Availability tests for production KG endpoints ===== //
        // ========================================================== //
        TestDefinition prod_ontogasgrid = new TestDefinition("prod/ontogasgrid", TestType.AVAILABILITY);
        prod_ontogasgrid.setInput("endpoint", "https://kg.cmclinnovations.com/blazegraph_geo/namespace/ontogasgrid/sparql");
        DEFINITIONS.add(prod_ontogasgrid);

        TestDefinition prod_landuse = new TestDefinition("prod/landuse", TestType.AVAILABILITY);
        prod_landuse.setInput("endpoint", "https://kg.cmclinnovations.com/blazegraph_geo/namespace/landuse/sparql");
        DEFINITIONS.add(prod_landuse);
    }

    /**
     * Returns the list of defined tests.
     *
     * @return defined tests.
     */
    public static synchronized Set<TestDefinition> getDefinedTests() {
        return Collections.unmodifiableSet(DEFINITIONS);
    }

    /**
     *
     * @param type
     * @return
     */
    public static synchronized Set<TestDefinition> getDefinedTests(TestType type) {
        Set<TestDefinition> matches = new LinkedHashSet<>();

        for (TestDefinition definition : DEFINITIONS) {
            if (definition.getType() == type) matches.add(definition);
        }

        return Collections.unmodifiableSet(matches);
    }

    /**
     *
     * @return
     */
    public static synchronized Set<TestType> getDefinedTypes() {
        Set<TestType> types = new LinkedHashSet<>();

        for (TestDefinition definition : DEFINITIONS) {
            types.add(definition.getType());
        }
        return types;
    }

    /**
     *
     * @param testName
     * @param testType
     * @return
     */
    public static synchronized TestDefinition getDefinedTest(String testName, String testType) {
        return getDefinedTest(testName, TestType.valueOf(testType));
    }

    /**
     *
     * @param testName
     * @param testType
     * @return
     */
    public static synchronized TestDefinition getDefinedTest(String testName, TestType testType) {
        for (TestDefinition definition : DEFINITIONS) {
            if (definition.getType().equals(testType) && definition.getName().equals(testName)) {
                return definition;
            }
        }
        return null;
    }
}
// End of class.
