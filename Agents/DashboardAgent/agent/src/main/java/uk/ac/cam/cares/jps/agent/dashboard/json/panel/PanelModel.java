package uk.ac.cam.cares.jps.agent.dashboard.json.panel;

import uk.ac.cam.cares.jps.agent.dashboard.json.panel.layout.LayoutTemplate;
import uk.ac.cam.cares.jps.agent.dashboard.json.panel.types.TemplatePanel;
import uk.ac.cam.cares.jps.agent.dashboard.utils.StringHelper;

import java.util.*;

/**
 * A Java representation of a JSON-like model that encapsulates and enforces information
 * about the panel syntax specific to Grafana dashboard.
 *
 * @author qhouyee
 */
public class PanelModel {
    // Requires an object variable to keep track of the same number in different methods
    private int rowNumber;
    private final StringBuilder panelSyntax = new StringBuilder();
    private static final int CHART_HEIGHT = 8;
    private static final int CHART_WIDTH = 12;

    /**
     * Constructor that process customisable options for the panels in Grafana's JSON model.
     *
     * @param databaseConnectionMap A map linking each database to its connection ID.
     * @param timeSeries            A map of all assets and rooms mapped to their time series.
     */
    public PanelModel(Map<String, String> databaseConnectionMap, Map<String, Map<String, List<String[]>>> timeSeries) {
        // Row numbers to compute y positions; Initialise from 0
        this.rowNumber = 0; // Each row correspond to one item group
        // Generate the syntax for all room-related panels if available
        // Note to start with room as it has additional thresholds keys
        if (timeSeries.containsKey(StringHelper.ROOM_KEY)) {
            // Room-related panels are designed to be at the top based on their topological hierarchy
            // Generate all the panels and store them into a queue
            Queue<TemplatePanel[]> panelQueue = LayoutTemplate.genRoomLayoutTemplate(timeSeries.get(StringHelper.ROOM_KEY), databaseConnectionMap);
            separateRoomMeasurePerRow(panelQueue);
            // Remove the room values once it has been processed
            timeSeries.remove(StringHelper.ROOM_KEY);
        }
        // Generate the syntax for all system-related panels if available
        if (timeSeries.containsKey(StringHelper.SYSTEM_KEY)) {
            // Room-related panels are designed to be at the top based on their topological hierarchy
            // Generate all the panels and store them into a queue
            Queue<TemplatePanel[]> panelQueue = LayoutTemplate.genSystemsLayoutTemplate(timeSeries.get(StringHelper.SYSTEM_KEY), databaseConnectionMap);
            groupPanelsAsRow(StringHelper.SYSTEM_KEY, panelQueue);
            // Remove the systems values once it has been processed
            timeSeries.remove(StringHelper.SYSTEM_KEY);
        }
        // For each item group that is not a room or system
        for (Map.Entry<String, Map<String, List<String[]>>> entry : timeSeries.entrySet()) {
            String currentItemGroup = entry.getKey();
            Map<String, List<String[]>> currentItemMeasureMap = entry.getValue();
            Queue<TemplatePanel[]> panelQueue = LayoutTemplate.genAssetLayoutTemplate(currentItemGroup, currentItemMeasureMap, databaseConnectionMap);
            groupPanelsAsRow(currentItemGroup, panelQueue);
        }
    }

    /**
     * Construct the JSON model as a String.
     *
     * @return The JSON model syntax as a String.
     */
    public String construct() {
        return this.panelSyntax.toString();
    }


    /**
     * When necessary, this method supports the grouping of panels into a row for the dashboard in JSON format.
     * Namely, it will group the panels associated with one room measure into a row. Each measure for these rooms
     * will have a separate row for improved organisation.
     *
     * @param panelQueue A collection containing all the required charts to be appended to these rows.
     * @return row number.
     */
    private void separateRoomMeasurePerRow(Queue<TemplatePanel[]> panelQueue) {
        while (!panelQueue.isEmpty()) {
            // Retrieve the current room panels and their measure names
            TemplatePanel[] roomPanels = panelQueue.poll();
            String measureName = roomPanels[0].getMeasure();
            // Generate a row title, which may include unit if available
            String title = StringHelper.addSpaceBetweenCapitalWords(measureName);
            title = roomPanels[2].getUnit().equals("null") ? title : title + "[" + roomPanels[2].getUnit() + "]";
            // Populate a new empty queue with only one array for this measure
            Queue<TemplatePanel[]> intermediateQueue = new ArrayDeque<>();
            intermediateQueue.offer(roomPanels);
            // Append a comma before if it is not the first row
            if (this.panelSyntax.length() != 0) this.panelSyntax.append(",");
            // Generate the row panel syntax
            this.panelSyntax.append("{")
                    .append("\"id\":null, \"type\":\"row\", \"collapsed\":true,")
                    // Title should be the measure name of these rooms
                    .append("\"title\": \"").append(title).append("\",")
                    .append(" \"gridPos\": {\"h\": 1,\"w\": ").append(CHART_WIDTH * 2)
                    .append(",\"x\": 0,\"y\": ").append(this.rowNumber).append("},")
                    .append("\"panels\": [").append(genPanelSyntax(this.rowNumber, intermediateQueue))
                    .append("]}");
            // Increment the row number for each measure of the rooms
            this.rowNumber++;
        }
    }

    /**
     * When necessary, this method supports the grouping of panels into a row for the dashboard in JSON format.
     *
     * @param itemGroup  The item group of interest. Should only accommodate asset types at the moment.
     * @param panelQueue A collection containing all the required charts to be appended to this row.
     */
    private void groupPanelsAsRow(String itemGroup, Queue<TemplatePanel[]> panelQueue) {
        // Append a comma before if it is not the first row
        if (this.panelSyntax.length() != 0) this.panelSyntax.append(",");
        String title = itemGroup.equals(StringHelper.SYSTEM_KEY) ? "Smart Meter" : StringHelper.addSpaceBetweenCapitalWords(itemGroup);
        // Generate the row panel syntax
        this.panelSyntax.append("{")
                .append("\"id\":null, \"type\":\"row\", \"collapsed\":true,")
                .append("\"title\": \"").append(title).append("\",")
                .append(" \"gridPos\": {\"h\": 1,\"w\": ").append(CHART_WIDTH * 2)
                .append(",\"x\": 0,\"y\": ").append(this.rowNumber).append("},")
                .append("\"panels\": [").append(genPanelSyntax(this.rowNumber, panelQueue))
                .append("]}");
        // Increment the row number once this row for the item group has been set up
        this.rowNumber++;
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
            rowNumber++;
        }
        return builder;
    }
}
