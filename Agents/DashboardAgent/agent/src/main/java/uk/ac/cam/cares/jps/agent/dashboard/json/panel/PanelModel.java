package uk.ac.cam.cares.jps.agent.dashboard.json.panel;

import uk.ac.cam.cares.jps.agent.dashboard.utils.StringHelper;

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
     * @param timeSeries            A map of all assets and rooms mapped to their time series.
     */
    public PanelModel(Map<String, String> databaseConnectionMap, Map<String, Map<String, List<String[]>>> timeSeries) {
        // Row numbers to compute x positions; Starts from 0
        int rowNumber = 0; // Each row correspond to one item group
        // Generate the syntax for all room-related panels if available
        if (timeSeries.containsKey(StringHelper.ROOM_KEY)) {
            // Room-related panels are designed to be at the top based on their topological hierarchy
            // Generate all the panels and store them into a queue
            Queue<TimeSeriesChart> panelQueue = genMeasurePanelsForItemGroup(StringHelper.ROOM_KEY, timeSeries.get(StringHelper.ROOM_KEY), databaseConnectionMap);
            // Compute the ending y position for the room's charts - assumes that there are two panel per row
            int lastYGroupPosition = rowNumber + (CHART_HEIGHT * (panelQueue.size() / 2));
            // Append the room panels to the stored syntax
            this.PANEL_SYNTAX.append(genPanelSyntax(rowNumber, panelQueue));
            // Remove the room values once it has been processed
            timeSeries.remove(StringHelper.ROOM_KEY);
            // Update the row number accordingly as the last step to ensure the processing is not impacted
            // Ensure that it is incremented by 1 so that the next panel starts below
            rowNumber = lastYGroupPosition + 1;
        }
        // For each item group that is not a room
        for (String currentItemGroup : timeSeries.keySet()) {
            Queue<TimeSeriesChart> panelQueue = genMeasurePanelsForItemGroup(currentItemGroup, timeSeries.get(currentItemGroup), databaseConnectionMap);
            groupPanelsAsRow(rowNumber, currentItemGroup, panelQueue);
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

    /**
     * Generates the various measure panels that can be found in the input item group.
     *
     * @param itemGroup             The item group of interest. May be rooms or asset types.
     * @param itemMeasures          A map containing all measures and their metadata to construct each panel.
     * @param databaseConnectionMap A map linking each database to its connection ID.
     * @return A queue storing all measure panels associated with this item group.
     */
    private Queue<TimeSeriesChart> genMeasurePanelsForItemGroup(String itemGroup, Map<String, List<String[]>> itemMeasures, Map<String, String> databaseConnectionMap) {
        Queue<TimeSeriesChart> panelQueue = new ArrayDeque<>();
        // For each of the measures, create a chart
        for (String measure : itemMeasures.keySet()) {
            // Take note to exclude the assets and rooms key as that is not required
            if (!measure.equals(StringHelper.ASSET_KEY) && !measure.equals(StringHelper.ROOM_KEY)) {
                // Retrieve the relevant database name and ID from the first item
                // Assumes that each measure of a specific asset type belongs to only one database
                String database = itemMeasures.get(measure).get(0)[3];
                String databaseID = databaseConnectionMap.get(database);
                // Assume the unit of each measure for each asset type is consistent
                String unit = itemMeasures.get(measure).get(0)[4];
                // Creates a chart object and add it to the queue
                TimeSeriesChart chart = new TimeSeriesChart(measure, itemGroup, unit, databaseID, itemMeasures.get(measure));
                panelQueue.offer(chart);
            }
        }
        return panelQueue;
    }

    /**
     * Generates all available panels for each item group into the required Json format.
     *
     * @param rowNumber  The current row number.
     * @param panelQueue A collection containing all the required charts to be appended to this row/group.
     * @return The group of panel syntax as a StringBuilder.
     */
    private StringBuilder genPanelSyntax(int rowNumber, Queue<TimeSeriesChart> panelQueue) {
        StringBuilder builder = new StringBuilder();
        // Initialise chart number when handling different asset types to compute position
        int chartNumber = 1;
        // While there are still items in the queue,
        while (!panelQueue.isEmpty()) {
            // Append a comma before if it is not the first panel
            if (builder.length() != 0) builder.append(",");
            // Retrieve the panel and remove it from the queue
            TimeSeriesChart currentPanel = panelQueue.poll();
            // Generate a dashboard with two column layout
            // If chart number is even, x position must be chart width. Odd charts will have 0 as x position
            int xPosition = chartNumber % 2 == 0 ? CHART_WIDTH : 0;
            // Y position will increment by chart height for every two panels. Note that it will always start from the row number
            int yPosition = rowNumber + (CHART_HEIGHT * ((chartNumber - 1) / 2));
            // Construct its syntax and append it
            builder.append(currentPanel.construct(CHART_HEIGHT, CHART_WIDTH, xPosition, yPosition));
            // Increment this number as it increase
            chartNumber++;
        }
        return builder;
    }

    /**
     * When necessary, this method supports the grouping of panels into a row for the dashboard in JSON format.
     *
     * @param rowNumber  The current row number.
     * @param itemGroup  The item group of interest. Should only accommodate asset types at the moment.
     * @param panelQueue A collection containing all the required charts to be appended to this row.
     */
    private void groupPanelsAsRow(int rowNumber, String itemGroup, Queue<TimeSeriesChart> panelQueue) {
        // Append a comma before if it is not the first row
        if (this.PANEL_SYNTAX.length() != 0) this.PANEL_SYNTAX.append(",");
        // Generate the row panel syntax
        this.PANEL_SYNTAX.append("{")
                .append("\"id\":null, \"type\":\"row\", \"collapsed\":true,")
                .append("\"title\": \"").append(StringHelper.addSpaceBetweenCapitalWords(itemGroup)).append("\",")
                .append(" \"gridPos\": {\"h\": 1,\"w\": ").append(CHART_WIDTH * 2)
                .append(",\"x\": 0,\"y\": ").append(rowNumber++).append("},")
                .append("\"panels\": [").append(genPanelSyntax(rowNumber, panelQueue))
                .append("]}");
        // Increment the row number once this row for the item group has been set up
        rowNumber++;
    }
}
