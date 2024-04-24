package uk.ac.cam.cares.jps.agent.dashboard.json.panel.types;

import java.util.*;

/**
 * A helper to store multiple transformation options.
 *
 * @author qhouyee
 */
public class TransformationOptions {
    private final Queue<String> transformationOptionsQueue;

    /**
     * Standard Constructor to initialise this class.
     */
    public TransformationOptions() {
        this.transformationOptionsQueue = new ArrayDeque<>();
    }

    /**
     * Add a transformation to organise the fields. This transformation will rename the data columns in PostGIS into their
     * item names. It will also sort the item names alphabetically. This method assumes that there is no transformation on the columns.
     *
     * @param timeSeriesMetadata A list of items and their metadata for the specified measure.
     */
    public void addOrganizeTransformation(List<String[]> timeSeriesMetadata) {
        this.addOrganizeTransformation("", timeSeriesMetadata);
    }

    /**
     * Add a transformation to organise the fields. This transformation will rename the data columns in PostGIS into their
     * item names. It will also sort the item names alphabetically.
     *
     * @param nameSuffix         The suffix for the data column if the columns have been transformed and renamed.
     * @param timeSeriesMetadata A list of items and their metadata for the specified measure.
     */
    public void addOrganizeTransformation(String nameSuffix, List<String[]> timeSeriesMetadata) {
        StringBuilder transformationBuilder = new StringBuilder();
        StringBuilder fieldColumnIndex = new StringBuilder();
        StringBuilder fieldColumnMapping = new StringBuilder();
        // Process metadata into the required format
        fieldColumnIndex.append("\"time\": 0"); // Ensure time is added as the first index
        int indexCounter = 1; // Index should start from one and increment by 1
        for (String[] metadata : timeSeriesMetadata) {
            // Only append a comma at the start if it is not the first set
            if (fieldColumnMapping.length() != 0) fieldColumnMapping.append(",");
            // Append in format of \"columnName\":\"name\"
            // Name suffix added if transformations have been applied on the data columns
            fieldColumnMapping.append("\"").append(metadata[1]).append(nameSuffix).append("\":\"").append(metadata[0]).append("\"");
            // Append with a comma before in format of \"columnName\":indexNumber
            fieldColumnIndex.append(",\"").append(metadata[1]).append("\":").append(indexCounter++);
        }
        // Prepare the parsed format
        transformationBuilder.append("{\"id\":\"organize\",\"options\":{")
                .append("\"excludeByName\":{},")
                .append("\"indexByName\":{").append(fieldColumnIndex).append("},")
                .append("\"renameByName\":{").append(fieldColumnMapping).append("}")
                .append("}}");
        this.transformationOptionsQueue.offer(transformationBuilder.toString());
    }

    /**
     * Add a transformation to group the fields by one field and compute their specified aggregate.
     *
     * @param aggregate          The aggregate computation for the fields. Eg. range, mix, max...
     * @param timeSeriesMetadata A list of items and their metadata for the specified measure.
     */
    public void addGroupByTransformation(String aggregate, List<String[]> timeSeriesMetadata) {
        StringBuilder transformationBuilder = new StringBuilder();
        StringBuilder fieldAggregations = new StringBuilder();
        // Process metadata into the required format
        for (String[] metadata : timeSeriesMetadata) {
            // Only append a comma at the start if it is not the first set
            if (fieldAggregations.length() != 0) fieldAggregations.append(",");
            // Append in format of "columnName":{"aggregations" : [], "operation":"aggregate" }
            fieldAggregations.append("\"").append(metadata[1])
                    .append("\":{")
                    .append("\"aggregations\":[\"").append(aggregate)
                    .append("\"],\"operation\":\"aggregate\"")
                    .append("}");
        }
        // Prepare the parsed format
        transformationBuilder.append("{\"id\":\"groupBy\",\"options\":{\"fields\":{")
                // Group by the interval variable
                .append("\"interval\":{\"aggregations\":[],\"operation\":\"groupby\"},")
                .append(fieldAggregations)
                .append("}}}");
        this.transformationOptionsQueue.add(transformationBuilder.toString());
    }

    /**
     * Construct the transformations into the Grafana compliant JSON syntax.
     *
     * @return The JSON transformations syntax as a String.
     */
    public String construct() {
        StringBuilder builder = new StringBuilder();
        // Retrieves and add the transformations
        while (!this.transformationOptionsQueue.isEmpty()) {
            String transformation = this.transformationOptionsQueue.poll();
            // If there are preexisting transformations, add a comma
            if (builder.length() != 0) builder.append(",");
            builder.append(transformation);
        }
        return "[" + builder + "]";
    }
}
