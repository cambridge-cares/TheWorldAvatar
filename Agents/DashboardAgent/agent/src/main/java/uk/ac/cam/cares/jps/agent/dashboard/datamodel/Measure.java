package uk.ac.cam.cares.jps.agent.dashboard.datamodel;

import java.util.*;

/**
 * Represents a quantifiable attribute associated with a specific item category.
 * This data model stores additional metadata to support interactions with the knowledge graph for populating the dashboard.
 *
 * @author qhouyee
 */
public class Measure {
    // WIP: The current setup limits the SQL query to one table and database, but we could explore other solutions
    private String database = "";
    private String table = "";
    private final String measureName;
    private final String unit;
    private final Map<String, String[]> itemTimeSeriesIriCatalog;
    private final Map<String, String[]> itemTimeSeriesMetadataCatalog;
    private final ArrayDeque<String[]> timeSeriesData;

    /**
     * Standard Constructor.
     *
     * @param measureName Name of the quantifiable attribute.
     * @param unit        Unit symbol for this attribute.
     */
    public Measure(String measureName, String unit) {
        this.measureName = measureName;
        this.unit = unit;
        this.itemTimeSeriesIriCatalog = new HashMap<>();
        this.itemTimeSeriesMetadataCatalog = new HashMap<>();
        this.timeSeriesData = new ArrayDeque<>();
    }

    /**
     * A getter method for name.
     */
    public String getName() {
        return this.measureName;
    }

    /**
     * A getter method for unit.
     */
    public String getUnit() {
        return this.unit;
    }

    /**
     * A getter method to retrieve the database holding the time series data.
     */
    public String getTimeSeriesDatabase() {return this.database;}

    /**
     * A getter method to retrieve the table holding the time series data.
     */
    public String getTimeSeriesTable() {return this.table;}

    /**
     * A getter method that retrieves all time series IRIs available.
     *
     * @returns A queue containing all time series IRIs. Within the array, first position is item name; Second position is the dataIRI; Third position is time series IRI.
     */
    public Queue<String[]> getAllTimeSeriesIris() {
        Queue<String[]> results = new ArrayDeque<>();
        for (Map.Entry<String, String[]> measure : this.itemTimeSeriesIriCatalog.entrySet()) {
            results.offer(measure.getValue());
        }
        return results;
    }

    /**
     * A getter method that retrieves all time series data.
     *
     * @returns A queue containing all the item and column names for time series in this order.
     */
    public Queue<String[]> getTimeSeriesData() {
        // Only attempt to retrieve if it hasn't been processed before
        // Results should be cached to reduce expensive runtime calls
        if (this.timeSeriesData.isEmpty()) {
            itemTimeSeriesMetadataCatalog.values().forEach(this.timeSeriesData::offer);
        }
        // Ensure queued is clone so original does not become empty
        return this.timeSeriesData.clone();
    }

    /**
     * A protected method that adds the time series iris that will be required to retrieve their POSTGIS metadata.
     * This method can only be called within the `datamodel` subpackage.
     *
     * @param itemName      The name for an item which has this quantifiable attribute.
     * @param measureIri    Corresponding dataIRI of the measure associated with the item.
     * @param timeSeriesIri Corresponding time series IRI of the measure associated with the item.
     */
    public void addTimeSeriesIris(String itemName, String measureIri, String timeSeriesIri) {
        // Ignores the value if there are duplicates
        this.itemTimeSeriesIriCatalog.putIfAbsent(itemName, new String[]{itemName, measureIri, timeSeriesIri});
    }

    /**
     * A protected method that adds the time series metadata that will be required for the dashboard setup.
     * This method can only be called within the `datamodel` subpackage.
     *
     * @param itemName The name for an item which has this quantifiable attribute.
     * @param column   Corresponding column name storing the measure associated with the item.
     * @param table    Corresponding table name storing the measure associated with the item.
     * @param database Corresponding database name storing the measure associated with the item.
     */
    public void addTimeSeriesMetadata(String itemName, String column, String table, String database) {
        // Ignores the value if there are duplicates
        this.itemTimeSeriesMetadataCatalog.putIfAbsent(itemName, new String[]{itemName, column});
        if (this.table.isEmpty()) this.table = table;
        if (this.database.isEmpty()) this.database = database;
    }
}
