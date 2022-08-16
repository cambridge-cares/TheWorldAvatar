package uk.ac.cam.cares.jps.agent.status;

import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Collections;
import java.util.LinkedHashSet;
import java.util.Set;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONArray;
import org.json.JSONObject;
import uk.ac.cam.cares.jps.agent.status.define.TestDefinition;
import uk.ac.cam.cares.jps.agent.status.define.TestType;

/**
 * Stores tests to be run.
 *
 * @author Michael Hillman
 */
public class TestRegistry {

    /**
     * Logger.
     */
    private static final Logger LOGGER = LogManager.getLogger(TestRegistry.class);

    /**
     * Tests to be executed.
     */
    private static final Set<TestDefinition> DEFINITIONS = new LinkedHashSet<>();

    /**
     * Read the JSON file that contains test definitions.
     */
    public static void readRegistryFile(Path registryFile) {
        if (Files.exists(registryFile)) {
            // Read the registry file
            DEFINITIONS.clear();

            try {
                String jsonContent = Files.readString(registryFile);
                JSONArray recordArray = new JSONArray(jsonContent);

                for (int i = 0; i < recordArray.length(); i++) {
                    JSONObject recordEntry = recordArray.getJSONObject(i);

                    // Get required info and build definition
                    String name = recordEntry.getString("name");
                    TestType type = TestType.valueOf(recordEntry.getString("type"));

                    TestDefinition definition = new TestDefinition(name, type);
                    DEFINITIONS.add(definition);

                    // Add all optional inputs
                    JSONObject inputs = recordEntry.getJSONObject("inputs");
                    if (inputs != null) {
                        for (String key : inputs.keySet()) {
                            definition.setInput(key, inputs.get(key).toString());
                        }
                    }
                }

                LOGGER.info("Registry file has been read successfully, " + DEFINITIONS.size() + " tests defined.");
            } catch (Exception exception) {
                LOGGER.error("Exception when reading 'test-registry.json' file!", exception);
            }
        } else {
            LOGGER.error("Could not find the 'test-registry.json' file in ~/.jps directory!");
        }
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
     * Returns all defined tests of the input type.
     *
     * @param type test type.
     * @return defined tests.
     */
    public static synchronized Set<TestDefinition> getDefinedTests(TestType type) {
        Set<TestDefinition> matches = new LinkedHashSet<>();

        for (TestDefinition definition : DEFINITIONS) {
            if (definition.getType() == type) matches.add(definition);
        }

        return Collections.unmodifiableSet(matches);
    }

    /**
     * Returns a set of all defined test types.
     *
     * @return test types.
     */
    public static synchronized Set<TestType> getDefinedTypes() {
        Set<TestType> types = new LinkedHashSet<>();

        for (TestDefinition definition : DEFINITIONS) {
            types.add(definition.getType());
        }
        return types;
    }

    /**
     * Returns the defined test.
     *
     * @param testName test name.
     * @param testType test type.
     * @return defined test.
     */
    public static synchronized TestDefinition getDefinedTest(String testName, String testType) {
        return getDefinedTest(testName, TestType.valueOf(testType));
    }

    /**
     * Returns the defined test.
     *
     * @param testName test name.
     * @param testType test type.
     * @return defined test.
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
