package uk.ac.cam.cares.jps.agent.status.define;

/**
 * Test type enum
 *
 * @author Michael
 */
public enum TestType {

    // Basic endpoint checks
    AVAILABILITY,
    // Custom count query
    QUERY_COUNT,
    // Basic agent availability
    AGENT_STATUS;

    /**
     *
     * @param type
     * @return
     */
    public static String getFriendlyName(TestType type) {
        switch (type) {
            case AVAILABILITY:
                return "KG Availability";

            case QUERY_COUNT:
                return "KG Data Counts";

            case AGENT_STATUS:
                return "Agent Availability";
        }

        return "";
    }

}
// End of enum
