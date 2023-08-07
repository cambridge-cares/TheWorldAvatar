package uk.ac.cam.cares.jps.agent.dashboard.json.panel;

import java.util.ArrayDeque;
import java.util.List;
import java.util.Map;
import java.util.Queue;

/**
 * A Java representation of a JSON-like model that encapsulates and enforces information
 * about the panel syntax specific to Grafana dashboard.
 *
 * @author qhouyee
 */
public class PanelModel {
    private final StringBuilder PANEL_SYNTAX = new StringBuilder();
    private static final int CHART_HEIGHT = 8;
    private static final int CHART_WIDTH = 12;

    /**
     * Constructor that process customisable options for the panels in Grafana's JSON model.
     *
     * @param databaseConnectionMap A map linking each database to its connection ID.
     * @param assets                A map of all assets mapped to their asset types.
     */
    public PanelModel(Map<String, String> databaseConnectionMap, Map<String, Map<String, List<String[]>>> assets) {
        // Initialise a queue to store these panels
        Queue<TimeSeriesChart> panelQueue = new ArrayDeque<>();
        // Generate a panel for each measure of each asset type available
        for (String assetType : assets.keySet()) {
            // Retrieve the map of measures to their asset and time series metadata
            Map<String, List<String[]>> measures = assets.get(assetType);
            // For each of the measures, create a chart
            for (String measure : measures.keySet()) {
                // Take note to exclude the assets key as that is not required
                if (!measure.equals("assets")) {
                    // Retrieve the relevant table name, database name and ID from the first item
                    // Assumes that each measure of a specific asset type belongs to only one database and table
                    String tableName = measures.get(measure).get(0)[2];
                    String database = measures.get(measure).get(0)[3];
                    String databaseID = databaseConnectionMap.get(database);
                    // Creates a chart object and add it to the queue
                    TimeSeriesChart chart = new TimeSeriesChart(measure, assetType, tableName, databaseID);
                    panelQueue.offer(chart);
                }
            }
        }
        // Chart number to compute position
        int chartNumber = 1;
        // While there are still items in the queue,
        while (!panelQueue.isEmpty()) {
            // Append a comma before if it is not the first panel
            if (this.PANEL_SYNTAX.length() != 0) this.PANEL_SYNTAX.append(",");
            // Retrieve the panel and remove it from the queue
            TimeSeriesChart currentPanel = panelQueue.poll();
            // Generate a dashboard with two column layout
            // If chart number is even, x position must be chart width. Odd charts will have 0 as x position
            int xPosition = chartNumber % 2 == 0 ? CHART_WIDTH : 0;
            // Y position will increment by chart height for every two panels
            int yPosition = CHART_HEIGHT * ((chartNumber - 1) / 2);
            // Construct its syntax and append it
            this.PANEL_SYNTAX.append(currentPanel.construct(CHART_HEIGHT, CHART_WIDTH, xPosition, yPosition));
            // Increment this number as it increase
            chartNumber++;
        }
    }

    /**
     * Construct the JSON model as a String.
     *
     * @return The JSON model syntax as a String.
     */
    public String construct() {
        return this.PANEL_SYNTAX.toString();
    }
}
