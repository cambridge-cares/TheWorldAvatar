package uk.ac.cam.cares.jps.agent.status.define;

import java.util.HashMap;
import java.util.Map;
import java.util.Objects;

/**
 * Defines a single type of test within a named group.
 *
 * @author Michael Hillman
 */
public final class TestDefinition {

    /**
     * Test name.
     */
    private final String name;

    /**
     *
     */
    private final TestType type;

    /**
     * Optional named test inputs.
     */
    private final Map<String, String> inputs = new HashMap<>();

    /**
     *
     */
    private String username;

    /**
     *
     */
    private String password;

    /**
     * Initialise a new BaseTest instance.
     *
     * @param name test name.
     */
    public TestDefinition(String name, TestType type) {
        this.name = name;
        this.type = type;
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
     *
     * @return
     */
    public TestType getType() {
        return type;
    }

    /**
     *
     * @param name
     * @param value
     */
    public void setInput(String name, String value) {
        inputs.put(name, value);
    }

    /**
     *
     * @param name
     * @return
     */
    public String getInput(String name) {
        return inputs.get(name);
    }

    /**
     *
     * @return
     */
    public Map<String, String> getInputs() {
        return inputs;
    }

    /**
     *
     * @param username
     * @param password
     */
    public void setCredentials(String username, String password) {
        this.username = username;
        this.password = password;
    }

    /**
     *
     * @return
     */
    public String getUsername() {
        return username;
    }

    /**
     *
     * @return
     */
    public String getPassword() {
        return password;
    }

    /**
     *
     * @param obj
     * @return
     */
    @Override
    public boolean equals(Object obj) {
        if (obj == null) return false;
        if (!(obj instanceof TestDefinition)) return false;

        TestDefinition that = (TestDefinition) obj;
        if (!Objects.equals(this.name, that.name)) return false;
        if (!Objects.equals(this.type, that.type)) return false;
        if (!Objects.equals(this.inputs, that.inputs)) return false;
        return true;
    }

    /**
     *
     * @return
     */
    @Override
    public int hashCode() {
        int hash = 5;
        hash = 43 * hash + Objects.hashCode(this.name);
        hash = 43 * hash + Objects.hashCode(this.type);
        hash = 43 * hash + Objects.hashCode(this.inputs);
        return hash;
    }
}
// End of class.
