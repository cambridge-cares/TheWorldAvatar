package uk.ac.cam.cares.jps.agent.dashboard.datamodel;

import java.util.*;

/**
 * This data model captures time series metadata on a specific type of item group and their associated measures for interactions with the knowledge graph.
 *
 * @author qhouyee
 */
class ItemGroup {
    private final Map<String, Measure> measures;

    /**
     * Standard Constructor to create a new item group.
     */
    protected ItemGroup() {
        this.measures = new HashMap<>();
    }

    /**
     * Adds the measure to be stored within this instance.
     *
     * @param measureName   Name of the quantifiable attribute.
     * @param unit          Unit symbol for this attribute.
     * @param itemName      The name of the item which has this quantifiable attribute.
     * @param measureIri    Corresponding dataIRI of the measure associated with the item.
     * @param timeSeriesIri Corresponding time series IRI of the measure associated with the item.
     */
    protected void addMeasure(String measureName, String unit, String itemName, String measureIri, String timeSeriesIri) {
        this.measures.computeIfAbsent(measureName, k -> new Measure(measureName, unit))
                .addTimeSeriesIris(itemName, measureIri, timeSeriesIri);
    }

    /**
     * Adds the time series metadata to be stored within this instance.
     *
     * @param measureName Name of the quantifiable attribute.
     * @param itemName    The name for an item which has this quantifiable attribute.
     * @param column      Corresponding column name storing the measure associated with the item.
     * @param table       Corresponding table name storing the measure associated with the item.
     * @param database    Corresponding database name storing the measure associated with the item.
     */
    protected void addTimeSeries(String measureName, String itemName, String column, String table, String database) {
        this.measures.get(measureName).addTimeSeriesMetadata(itemName, column, table, database);
    }

    /**
     * A getter method for a queue of all measures available within this item group.
     */
    protected Queue<Measure> getMeasures() {
        return new ArrayDeque<>(this.measures.values());
    }

    /**
     * A getter method to retrieve the measure object associated with the measure name.
     */
    protected Measure getMeasure(String measureName) {
        return this.measures.getOrDefault(measureName, null);
    }
}
