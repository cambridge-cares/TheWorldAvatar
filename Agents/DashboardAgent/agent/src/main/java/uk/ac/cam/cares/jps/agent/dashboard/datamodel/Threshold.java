package uk.ac.cam.cares.jps.agent.dashboard.datamodel;

import java.util.*;

/**
 * This data model captures the min and max thresholds associated with a measure.
 *
 * @author qhouyee
 */
public class Threshold {
    private final Map<String, String[]> minMaxThresholds;

    /**
     * Standard Constructor.
     */
    protected Threshold() {
        this.minMaxThresholds = new HashMap<>();
    }

    /**
     * Stores minimum and maximum thresholds associated with a measure. Note that duplicate values will be ignored
     *
     * @param measureName  Name of the quantifiable attribute.
     * @param minThreshold Minimum threshold value for the measure.
     * @param minThreshold Maximum threshold value for the measure.
     */
    protected void addThreshold(String measureName, String minThreshold, String maxThreshold) {
        this.minMaxThresholds.computeIfAbsent(measureName, k -> new String[]{minThreshold, maxThreshold});
    }

    public boolean contains(String measureName) {return this.minMaxThresholds.containsKey(measureName);}

    public String[] getThreshold(String measureName) {return this.minMaxThresholds.getOrDefault(measureName, new String[]{});}
}
