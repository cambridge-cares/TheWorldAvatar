package uk.ac.cam.cares.jps.agent.dashboard.datamodel;

import java.util.*;

/**
 * This data model stores the items and thresholds by group within a facility for interactions with the knowledge graph.
 *
 * @author qhouyee
 */
class Facility {
    private final String facilityName;
    private final Map<String, Queue<String>> itemCatalog;
    private final Map<String, Threshold> itemThresholdCatalog;
    private final Set<String> existingItems;

    /**
     * Standard Constructor.
     *
     * @param facilityName The name of the facility.
     */
    protected Facility(String facilityName) {
        this.facilityName = facilityName;
        this.itemCatalog = new HashMap<>();
        this.itemThresholdCatalog = new HashMap<>();
        this.existingItems = new HashSet<>();
    }

    /**
     * A getter method to retrieve this facility's name
     */
    protected String getName() {
        return this.facilityName;
    }

    /**
     * Stores the item and item group found within the same facility.
     *
     * @param name      The name of the item within the facility.
     * @param itemGroup The item group.
     */
    protected void addItem(String name, String itemGroup) {
        if (!this.existingItems.contains(name)) {
            this.itemCatalog.computeIfAbsent(itemGroup, k -> new ArrayDeque<>()).offer(name);
            this.existingItems.add(name);
        }
    }

    /**
     * Stores the thresholds of a specific attribute and item group for the whole facility.
     * Note that duplicate measure and item groups are ignored.
     *
     * @param itemGroup    The item group.
     * @param measureName  The name of the quantifiable attribute belonging to this room.
     * @param minThreshold Minimum threshold value for the measure.
     * @param maxThreshold Maximum threshold value for the measure.
     */
    protected void addThresholds(String itemGroup, String measureName, String minThreshold, String maxThreshold) {
        this.itemThresholdCatalog.computeIfAbsent(itemGroup, k -> new Threshold())
                .addThreshold(measureName, minThreshold, maxThreshold);
    }

    /**
     * A getter method to retrieve all item groups such as assets, rooms, and systems within this facility.
     *
     * @return A queue of all available item groups.
     */
    protected Queue<String> getItemGroups() {
        Queue<String> groups = new ArrayDeque<>();
        if (!this.itemCatalog.isEmpty()) {
            groups.addAll(this.itemCatalog.keySet());
        }
        return groups;
    }

    /**
     * A getter method to retrieve all item names belonging to the specified item group within this facility.
     *
     * @return A queue of all available item names for that group.
     */
    protected Queue<String> getItemNames(String itemGroup) {return new ArrayDeque<>(this.itemCatalog.getOrDefault(itemGroup, new ArrayDeque<>()));}

    /**
     * A getter method to retrieve the thresholds associated with the specified item group in this facility.
     */
    protected Threshold getThresholds(String itemGroup) {return this.itemThresholdCatalog.getOrDefault(itemGroup, null);}
}
