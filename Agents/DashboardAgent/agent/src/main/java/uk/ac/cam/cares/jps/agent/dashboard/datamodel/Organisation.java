package uk.ac.cam.cares.jps.agent.dashboard.datamodel;

import uk.ac.cam.cares.jps.agent.dashboard.utils.StringHelper;

import java.util.*;

/**
 * A data model storing the required information to display the time series of all facilities associated with an organisation in the dashboard.
 * This will be the main interface with external classes.
 *
 * @author qhouyee
 */
public class Organisation {
    private boolean hasRooms = false;
    private boolean hasSystems = false;
    private final String organisationName;
    private final List<String> itemGroups;
    private final Map<String, ItemGroup> itemCatalog;
    private final Map<String, Facility> facilities;

    /**
     * Constructor to initialise an organisation object.
     */
    public Organisation(String organisationName) {
        this.organisationName = organisationName;
        this.itemGroups = new ArrayList<>();
        this.facilities = new HashMap<>();
        this.itemCatalog = new HashMap<>();
    }

    /**
     * A getter method to retrieve this organisation's name
     */
    public String getName() {
        return this.organisationName;
    }

    /**
     * Stores the metadata of an item found within the specified facility.
     *
     * @param facilityName  The name of the facility.
     * @param itemName      The name of the item within the facility.
     * @param itemGroup     The item group of interest.
     * @param measureName   The name of the quantifiable attribute belonging to this item.
     * @param unit          Unit symbol for the attribute.
     * @param measureIri    Corresponding dataIRI of the measure associated with the item.
     * @param timeSeriesIri Corresponding time series IRI of the measure associated with the item.
     */
    public void addFacilityItem(String facilityName, String itemName, String itemGroup, String measureName, String unit, String measureIri, String timeSeriesIri) {
        this.facilities.computeIfAbsent(facilityName, k -> new Facility(facilityName)).addItem(itemName, itemGroup);
        this.itemCatalog.computeIfAbsent(itemGroup, k -> new ItemGroup()).addMeasure(measureName, unit, itemName, measureIri, timeSeriesIri);
        if (itemGroup.equals(StringHelper.ROOM_KEY)) {
            this.hasRooms = true;
        } else if (itemGroup.equals(StringHelper.SYSTEM_KEY)) {
            this.hasSystems = true;
        }
    }

    /**
     * Stores the thresholds of a specific attribute and item group for the specified facility.
     * Note that duplicate measure and item groups are ignored.
     *
     * @param facilityName The name of the facility.
     * @param itemGroup    The item group of interest.
     * @param measureName  The name of the quantifiable attribute belonging to this room.
     * @param minThreshold Minimum threshold value for the measure.
     * @param maxThreshold Maximum threshold value for the measure.
     */
    public void addThresholds(String facilityName, String itemGroup, String measureName, String minThreshold, String maxThreshold) {
        this.facilities.get(facilityName).addThresholds(itemGroup, measureName, minThreshold, maxThreshold);
    }

    /**
     * Add the time series metadata for the specified item group and measure.
     *
     * @param itemGroup   The item group of interest.
     * @param measureName The name of the quantifiable attribute belonging to this item group.
     * @param itemName    The name for an item which has this quantifiable attribute.
     * @param column      Corresponding column name storing the measure associated with the item.
     * @param table       Corresponding table name storing the measure associated with the item.
     * @param database    Corresponding database name storing the measure associated with the item.
     */
    public void addTimeSeries(String itemGroup, String measureName, String itemName, String column, String table, String database) {
        this.itemCatalog.get(itemGroup).addTimeSeries(measureName, itemName, column, table, database);
    }

    /**
     * Retrieve the various facilities managed by this organisation.
     */
    public Queue<String> getFacilities() {
        Queue<String> results = new ArrayDeque<>();
        this.facilities.values().forEach(facility -> results.offer(facility.getName()));
        return results;
    }

    /**
     * Retrieve all available item groups managed under this organisation.
     */
    public List<String> getAllItemGroups() {
        // Only attempt to retrieve if it hasn't been processed before
        // Results should be cached to reduce expensive runtime calls
        if (this.itemGroups.isEmpty()) {
            Set<String> tempGroups = new HashSet<>();
            // Ensure that room and system are the first two results
            if (this.hasRooms) this.itemGroups.add(StringHelper.ROOM_KEY);
            if (this.hasSystems) this.itemGroups.add(StringHelper.SYSTEM_KEY);
            this.facilities.values().forEach(facility -> tempGroups.addAll(facility.getItemGroups()));
            tempGroups.forEach(current -> {
                if (!current.equals(StringHelper.ROOM_KEY) && !current.equals(StringHelper.SYSTEM_KEY)) {
                    this.itemGroups.add(current);
                }
            });
        }
        return this.itemGroups;
    }

    /**
     * Retrieve all the item names grouped by facility for the specified item group.
     *
     * @param itemGroup The item group of interest.
     * @return A queue of string arrays containing item and facility name in this sequence.
     */
    public Queue<String[]> getFacilityItemInventory(String itemGroup) {
        Queue<String[]> itemInventory = new ArrayDeque<>();
        this.facilities.values().forEach(facility -> {
            Queue<String> currentNames = facility.getItemNames(itemGroup);
            currentNames.forEach(itemName -> itemInventory.offer(new String[]{itemName, facility.getName()}));
        });
        return itemInventory;
    }

    /**
     * Retrieve all thresholds for the specified item group across all facilities managed under this organisation.
     *
     * @param itemGroup The item group of interest.
     */
    public Queue<Threshold> getThresholds(String itemGroup) {
        Queue<Threshold> thresholds = new ArrayDeque<>();
        this.facilities.values().forEach(facility -> {
            Threshold currentThreshold = facility.getThresholds(itemGroup);
            if (currentThreshold != null) thresholds.offer(currentThreshold);
        });
        return thresholds;
    }

    /**
     * Retrieve all available measures associated with the specified item group across all facilities managed under this organisation.
     *
     * @param itemGroup The item group of interest.
     */
    public Set<String> getAllMeasureNames(String itemGroup) {
        Set<String> measureNames = new HashSet<>();
        Queue<Measure> measures = this.itemCatalog.get(itemGroup).getMeasures();
        measures.forEach(measure -> measureNames.add(measure.getName()));
        return measureNames;
    }

    /**
     * Retrieve the specified measure associated with the specified item group across all facilities managed under this organisation.
     *
     * @param itemGroup   The item group of interest.
     * @param measureName The name of the quantifiable attribute belonging to this item.
     */
    public Measure getMeasure(String itemGroup, String measureName) {
        return this.itemCatalog.get(itemGroup).getMeasure(measureName);
    }
}
