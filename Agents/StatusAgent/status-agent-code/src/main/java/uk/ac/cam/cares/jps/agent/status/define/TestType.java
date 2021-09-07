package uk.ac.cam.cares.jps.agent.status.define;

/**
 * Test type enum
 *
 * @author Michael
 */
public enum TestType {

    // Basic endpoint checks
    AVAILABILITY("AVAILABILITY");

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

    /**
     *
     * @param type
     * @return
     */
    public static String getFriendlyName(TestType type) {
        switch (type) {
            case AVAILABILITY:
                return "Basic Availability Tests";
        }

        return "";
    }

}
// End of enum
