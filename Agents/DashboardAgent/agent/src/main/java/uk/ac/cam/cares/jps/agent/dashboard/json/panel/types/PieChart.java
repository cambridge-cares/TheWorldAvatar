package uk.ac.cam.cares.jps.agent.dashboard.json.panel.types;

import uk.ac.cam.cares.jps.agent.dashboard.datamodel.Measure;
import uk.ac.cam.cares.jps.agent.dashboard.json.panel.layout.UnitMapper;
import uk.ac.cam.cares.jps.agent.dashboard.utils.StringHelper;

/**
 * A Java representation of a JSON-like model that encapsulates and enforces information
 * about pie chart syntax specific to Grafana dashboard.
 *
 * @author qhouyee
 */
public class PieChart extends DefaultGrafanaPanel {
    /**
     * Standard Constructor.
     *
     * @param measure    The data model for constructing this panel.
     * @param itemGroup  The item group for this measure.
     * @param databaseId The database connection ID generated by Grafana.
     */
    public PieChart(Measure measure, String itemGroup, String databaseId) {
        super("piechart", itemGroup, databaseId, measure);
        String measureName = measure.getName();
        String unit = measure.getUnit();
        // Sets the unit for retrieval
        super.setUnit(unit);
        // Title is: Latest Measure Distribution [Unit]
        String titleContent = "Latest " + StringHelper.addSpaceBetweenCapitalWords(measureName) + " distribution";
        titleContent = unit == null ? titleContent : titleContent + " [" + unit + "]"; // Unit is optional
        super.setTitle(titleContent);
        // Description should follow the measure name and item group
        String description = "A pie chart displaying the latest distribution for " + measureName.toLowerCase() + " of " + itemGroup.toLowerCase();
        super.setDescription(description);
    }

    /**
     * Construct the Pie Chart syntax as a String.
     *
     * @param height    Height of the panel.
     * @param width     Width of the panel.
     * @param xPosition X position within the dashboard.
     * @param yPosition Y position within the dashboard.
     * @return The chart syntax as a String.
     */
    @Override
    public String construct(int height, int width, int xPosition, int yPosition) {
        return "{" + super.genCommonJson(height, width, xPosition, yPosition) +
                // Plugin version
                "\"pluginVersion\":\"10.0.3\"," +
                // Field Configuration
                "\"fieldConfig\": {" +
                // Default field configuration
                "\"defaults\": {\"color\": {\"mode\": \"palette-classic\"}," +
                // Custom parts of field configurations
                "\"custom\":{" + "\"hideFrom\":{\"legend\":false,\"tooltip\":false,\"viz\":false}" +
                "}," + // End of custom part+
                "\"mappings\": []," +
                "\"unit\":\"" + UnitMapper.getUnitSyntax(super.getUnit()) + "\"" +
                "}," + // End of defaults
                "\"overrides\": []" +
                "}," + // End of field configuration
                // Options
                "\"options\":{" +
                // Legend options
                "\"legend\":{\"displayMode\":\"list\",\"placement\":\"right\",\"showLegend\":true}," +
                // Tooltip options
                "\"tooltip\":{\"mode\":\"single\",\"sort\":\"none\"}," +
                // Other options
                "\"displayLabels\":[\"percent\"]," +
                "\"pieType\":\"donut\"," +
                "\"reduceOptions\": {\"calcs\":[\"lastNotNull\"],\"fields\":\"\",\"values\":false}" +
                "}" + // end of options
                "}";
    }
}
