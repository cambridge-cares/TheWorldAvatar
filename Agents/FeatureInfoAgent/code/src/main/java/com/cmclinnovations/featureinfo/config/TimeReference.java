package com.cmclinnovations.featureinfo.config;

/**
 * Enumerator for allowed TimeReference types.
 */
public enum TimeReference {
    
    /**
     * Time limits are taken from current time.
     */
    NOW("now"),

    /**
     * Time limits are taken from latest data point.
     */
    LATEST("latest"),

    /**
     * Time limits are taken from the first data point.
     */
    FIRST("first"),

    /**
     * All time values are taken.
     */
    ALL("all");

    /**
     * Textual label.
     */
    private final String label;

    /**
     * Initialise a new TimeReference enumerator.
     * 
     * @param label textual label.
     */
    private TimeReference(String label) {
        this.label = label;
    }

    /**
     * Get string value.
     * 
     * @returns string value.
     */
    @Override
    public String toString() {
        return this.label;
    }

}
// End of enum.