package uk.ac.cam.cares.jps.agent.status.define;

import uk.ac.cam.cares.jps.agent.status.execute.AvailabilityTestExecutor;
import uk.ac.cam.cares.jps.agent.status.execute.TestExecutor;

/**
 * Defines a single type of test within a named group.
 *
 * @author Michael Hillman
 */
public class AvailabilityTestDefinition extends TestDefinition {

    /**
     * SparQL query for testing.
     */
    public static final String QUERY = "SELECT (COUNT(*) AS ?NO_OF_TRIPLES) WHERE { ?x ?y ?z . }";

    /**
     * Location of KG end point to test.
     */
    private final String endpoint;

    /**
     * Optional username for KG endpoint.
     */
    private String username;

    /**
     * Optional password for KG endpoint.
     */
    private String password;

    /**
     * Initialise a new instance with the input KG endpoint.
     *
     * @param endpoint KG endpoint.
     */
    public AvailabilityTestDefinition(String name, String endpoint) {
        super(name, "basic-availability");
        this.endpoint = endpoint;
    }

    /**
     * Set credentials to use when querying the KG endpoint.
     *
     * @param username endpoint username.
     * @param password endpoint password.
     */
    public void setCredentials(String username, String password) {
        this.username = username;
        this.password = password;
    }

    /**
     * Returns the KG endpoint used in this test.
     *
     * @return KG endpoint.
     */
    public String getEndpoint() {
        return endpoint;
    }

    /**
     * Returns the username for the KG endpoint (if set).
     *
     * @return KG username.
     */
    public String getUsername() {
        return username;
    }

    /**
     * Returns the password for the KG endpoint (if set).
     *
     * @return KG password.
     */
    public String getPassword() {
        return password;
    }

     /**
     * Returns the TextExecutor class that should be used to run definitions of this type.
     *
     * @return TextExecutor class.
     */
    @Override
    public Class<? extends TestExecutor> getExecutorClass() {
        return AvailabilityTestExecutor.class;
    }

}
// End of class.
