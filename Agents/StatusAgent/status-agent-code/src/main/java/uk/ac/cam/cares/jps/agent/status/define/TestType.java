package uk.ac.cam.cares.jps.agent.status.define;

/**
 *
 * @author Michael
 */
public enum TestType {

    AVAILABILITY("basic-availability");

    /**
     *
     */
    private final String type;

    /**
     *
     * @param type
     */
    TestType(String type) {
        this.type = type;
    }

    /**
     *
     * @return
     */
    public String toString() {
        return type;
    }

}
// End of enum
