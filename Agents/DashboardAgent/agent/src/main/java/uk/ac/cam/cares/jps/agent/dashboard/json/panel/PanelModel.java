package uk.ac.cam.cares.jps.agent.dashboard.json.panel;

import uk.ac.cam.cares.jps.agent.dashboard.utils.StringHelper;

import java.util.*;

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
            Queue<TemplatePanel[]> panelQueue = genMeasurePanelsForItemGroup(StringHelper.ROOM_KEY, timeSeries.get(StringHelper.ROOM_KEY), databaseConnectionMap);
            rowNumber = separateRoomMeasurePerRow(rowNumber, panelQueue);
            // Remove the room values once it has been processed
            timeSeries.remove(StringHelper.ROOM_KEY);
        }
        // For each item group that is not a room
        for (String currentItemGroup : timeSeries.keySet()) {
            Queue<TemplatePanel[]> panelQueue = genMeasurePanelsForItemGroup(currentItemGroup, timeSeries.get(currentItemGroup), databaseConnectionMap);
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
    private Queue<TemplatePanel[]> genMeasurePanelsForItemGroup(String itemGroup, Map<String, List<String[]>> itemMeasures, Map<String, String> databaseConnectionMap) {
        // Generate an empty queue of arrays for generating panels (at most three to accommodate screen resolutions)
        // At the moment, one horizontal row (encapsulated by one array) is designed to contain only at most have 3 panels
        Queue<TemplatePanel[]> panelQueue = new ArrayDeque<>();
        TemplatePanel[] panelArr;
        // For non-rooms, thresholds should be empty
        Map<String, String[]> thresholdMap = new HashMap<>();
        // If this item group is rooms
        if (itemGroup.equals(StringHelper.ROOM_KEY)) {
            // Retrieve the thresholds and process it for generating the charts
            List<String[]> thresholdList = itemMeasures.get(StringHelper.THRESHOLD_KEY);
            thresholdMap = processThresholdsToMap(thresholdList);
            itemMeasures.remove(StringHelper.THRESHOLD_KEY); // remove the threshold after processing
        }
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
                // Retrieves the thresholds if it is available, else, it should return an empty array
                String[] thresholds = thresholdMap.isEmpty() ? new String[]{} :
                        thresholdMap.containsKey(measure) ? thresholdMap.get(measure) : new String[]{};
                // Create the panel objects and add it to the queue
                Gauge gaugePanel = new Gauge(measure, itemGroup, unit, databaseID, itemMeasures.get(measure), thresholds);
                TimeSeriesChart tsChart = new TimeSeriesChart(measure, itemGroup, unit, databaseID, itemMeasures.get(measure));
                // If this row is created for the rooms
                if (itemGroup.equals(StringHelper.ROOM_KEY)) {
                    // It should have an additional gauge panel displaying the average of all time series
                    Gauge averageGaugePanel = new Gauge(measure, itemGroup, unit, databaseID, itemMeasures.get(measure), thresholds, true);
                    panelArr = new TemplatePanel[3]; // Three panels should be included
                    panelArr[0] = averageGaugePanel;
                    panelArr[1] = gaugePanel;
                    panelArr[2] = tsChart;
                } else {
                    // For other item groups, we only expect 2 panels
                    panelArr = new TemplatePanel[]{gaugePanel, tsChart};
                }
                panelQueue.offer(panelArr);
            }
        }
        return panelQueue;
    }

    /**
     * Process the threshold list into a map so that it is easier to retrieve thresholds from.
     *
     * @param thresholdList A list of the threshold metadata.
     * @return A map in the form of {measure: [min, max]}.
     */
    private Map<String, String[]> processThresholdsToMap(List<String[]> thresholdList) {
        Map<String, String[]> thresholdMap = new HashMap<>();
        for (String[] threshold : thresholdList) {
            thresholdMap.put(threshold[0], new String[]{threshold[1], threshold[2]});
        }
        return thresholdMap;
    }

    /**
     * When necessary, this method supports the grouping of panels into a row for the dashboard in JSON format.
     * Namely, it will group the panels associated with one room measure into a row. Each measure for these rooms
     * will have a separate row for improved organisation.
     *
     * @param rowNumber  The current row number.
     * @param panelQueue A collection containing all the required charts to be appended to these rows.
     * @return row number.
     */
    private int separateRoomMeasurePerRow(int rowNumber, Queue<TemplatePanel[]> panelQueue) {
        while (!panelQueue.isEmpty()) {
            // Retrieve the current room panels and their measure names
            TemplatePanel[] roomPanels = panelQueue.poll();
            String measureName = roomPanels[0].getMeasure();
            // Populate a new empty queue with only one array for this measure
            Queue<TemplatePanel[]> intermediateQueue = new ArrayDeque<>();
            intermediateQueue.offer(roomPanels);
            // Append a comma before if it is not the first row
            if (this.PANEL_SYNTAX.length() != 0) this.PANEL_SYNTAX.append(",");
            // Generate the row panel syntax
            this.PANEL_SYNTAX.append("{")
                    .append("\"id\":null, \"type\":\"row\", \"collapsed\":true,")
                    // Title should be the measure name of these rooms
                    .append("\"title\": \"").append(StringHelper.addSpaceBetweenCapitalWords(measureName)).append("\",")
                    .append(" \"gridPos\": {\"h\": 1,\"w\": ").append(CHART_WIDTH * 2)
                    .append(",\"x\": 0,\"y\": ").append(rowNumber).append("},")
                    .append("\"panels\": [").append(genPanelSyntax(rowNumber, intermediateQueue))
                    .append("]}");
            // Increment the row number for each measure of the rooms
            rowNumber++;
        }
        return rowNumber;
    }

    /**
     * When necessary, this method supports the grouping of panels into a row for the dashboard in JSON format.
     *
     * @param rowNumber  The current row number.
     * @param itemGroup  The item group of interest. Should only accommodate asset types at the moment.
     * @param panelQueue A collection containing all the required charts to be appended to this row.
     */
    private void groupPanelsAsRow(int rowNumber, String itemGroup, Queue<TemplatePanel[]> panelQueue) {
        // Append a comma before if it is not the first row
        if (this.PANEL_SYNTAX.length() != 0) this.PANEL_SYNTAX.append(",");
        // Generate the row panel syntax
        this.PANEL_SYNTAX.append("{")
                .append("\"id\":null, \"type\":\"row\", \"collapsed\":true,")
                .append("\"title\": \"").append(StringHelper.addSpaceBetweenCapitalWords(itemGroup)).append("\",")
                .append(" \"gridPos\": {\"h\": 1,\"w\": ").append(CHART_WIDTH * 2)
                .append(",\"x\": 0,\"y\": ").append(rowNumber).append("},")
                .append("\"panels\": [").append(genPanelSyntax(rowNumber, panelQueue))
                .append("]}");
        // Increment the row number once this row for the item group has been set up
        rowNumber++;
    }

    /**
     * Generates all available panels for each item group into the required Json format.
     *
     * @param rowNumber  The current row number.
     * @param panelQueue A collection containing all the required charts to be appended to this row/group.
     * @return The group of panel syntax as a StringBuilder.
     */
    private StringBuilder genPanelSyntax(int rowNumber, Queue<TemplatePanel[]> panelQueue) {
        StringBuilder builder = new StringBuilder();
        // While there are still items in the queue,
        while (!panelQueue.isEmpty()) {
            // Append a comma before if it is not the first panel
            if (builder.length() != 0) builder.append(",");
            // Retrieve the panel and remove it from the queue
            TemplatePanel[] currentPanelArr = panelQueue.poll();
            // For a two panel row
            if (currentPanelArr.length == 2) {
                // They should have the same height, width and y-position but different xPosition
                builder.append(currentPanelArr[0].construct(CHART_HEIGHT, CHART_WIDTH, 0, rowNumber + 1))
                        .append(",")
                        .append(currentPanelArr[1].construct(CHART_HEIGHT, CHART_WIDTH, CHART_WIDTH, rowNumber + 1));
                // For a three panel row, they should fit in 4-8-12 format
            } else if (currentPanelArr.length == 3) {
                int firstChartWidth = 4;
                builder.append(currentPanelArr[0].construct(CHART_HEIGHT, firstChartWidth, 0, rowNumber + 1))
                        .append(",")
                        .append(currentPanelArr[1].construct(CHART_HEIGHT, 8, firstChartWidth, rowNumber + 1))
                        .append(",")
                        .append(currentPanelArr[2].construct(CHART_HEIGHT, CHART_WIDTH, CHART_WIDTH, rowNumber + 1));
            }
        }
        return builder;
    }
}
