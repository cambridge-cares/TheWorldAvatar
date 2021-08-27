package uk.ac.cam.cares.jps.agent.status.define;

import uk.ac.cam.cares.jps.agent.status.execute.TestExecutor;

/**
 * Defines a single type of test within a named group.
 *
 * @author Michael Hillman
 */
public abstract class TestDefinition {

    /**
     * Test name.
     */
    protected final String name;

    /**
     * Group name.
     */
    protected final String group;

    /**
     * Initialise a new BaseTest instance.
     *
     * @param name test name.
     * @param group group name.
     */
    public TestDefinition(String name, String group) {
        this.name = name;
        this.group = group;
    }

    /**
     * Returns the test's user facing name
     *
     * @return test's public name.
     */
    public String getName() {
        return name;
    }

    /**
     * Return the name of the group the test is in.
     *
     * @return group name.
     */
    public String getGroup() {
        return group;
    }

    /**
     * Returns the TextExecutor class that should be used to run definitions of this type.
     *
     * @return TextExecutor class.
     */
    public abstract Class<? extends TestExecutor> getExecutorClass();

}
// End of class.
