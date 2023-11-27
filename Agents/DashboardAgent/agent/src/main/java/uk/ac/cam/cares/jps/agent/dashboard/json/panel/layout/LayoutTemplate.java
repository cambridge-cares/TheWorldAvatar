package uk.ac.cam.cares.jps.agent.dashboard.json.panel.layout;

import uk.ac.cam.cares.jps.agent.dashboard.json.panel.types.BarChart;
import uk.ac.cam.cares.jps.agent.dashboard.json.panel.types.Gauge;
import uk.ac.cam.cares.jps.agent.dashboard.json.panel.types.TemplatePanel;
import uk.ac.cam.cares.jps.agent.dashboard.json.panel.types.TimeSeriesChart;
import uk.ac.cam.cares.jps.agent.dashboard.utils.StringHelper;

import java.util.*;

/**
 * A class that offers different layout templates depending on the item group (assets, rooms, or systems).
 *
 * @author qhouyee
 */
public class LayoutTemplate {

    /**
     * Private constructor to prevent instantiation.
     */
    private LayoutTemplate() {
    }

    /**
     * Generates the layout template for each asset type and all their associated measures. One gauge chart and one time series chart
     * will be generated, which display the specified measure for the individual assets of the specified type.
     *
     * @param assetType             The asset type.
     * @param assetMeasures         A map containing all measures and their metadata to construct the panels for this asset.
     * @param databaseConnectionMap A map linking each database to its connection ID.
     * @return A queue containing all the measure panels available for each asset type.
     */
    public static Queue<TemplatePanel[]> genAssetLayoutTemplate(String assetType, Map<String, List<String[]>> assetMeasures, Map<String, String> databaseConnectionMap) {
        // Generate an empty queue of arrays for generating panels (at most three to accommodate screen resolutions)
        // At the moment, one horizontal row (encapsulated by one array) is designed to contain only at most have 3 panels
        Queue<TemplatePanel[]> panelQueue = new ArrayDeque<>();
        // For each of the measures, create a set of chart
        for (String measure : assetMeasures.keySet()) {
            // Take note to exclude the asset key as that is not required
            if (!measure.equals(StringHelper.ASSET_KEY)) {
                // Retrieve the relevant database name and ID from the first item
                // Assumes that each measure of a specific systems belongs to only one database
                String database = assetMeasures.get(measure).get(0)[3];
                String databaseID = databaseConnectionMap.get(database);
                // Assume the unit of each measure for the systems is consistent
                String unit = assetMeasures.get(measure).get(0)[4];
                // Generate a gauge and time series chart with no thresholds
                Gauge gaugePanel = new Gauge(measure, assetType, unit, databaseID, assetMeasures.get(measure), new String[]{});
                TimeSeriesChart tsChart = new TimeSeriesChart(measure, assetType, unit, databaseID, assetMeasures.get(measure), new String[]{});
                TemplatePanel[] panelArr = new TemplatePanel[]{gaugePanel, tsChart};
                panelQueue.offer(panelArr);
            }
        }
        return panelQueue;
    }

    /**
     * Generates the layout template for the rooms and all their associated measures. Two gauge charts and one time series chart
     * will be generated per measure of rooms. The first gauge chart displays an average latest value of all available rooms,
     * whereas the second displays the individual latest values.
     *
     * @param roomMeasures          A map containing all measures and their metadata to construct the panels for rooms.
     * @param databaseConnectionMap A map linking each database to its connection ID.
     * @return A queue containing all the measure panels available for the rooms.
     */
    public static Queue<TemplatePanel[]> genRoomLayoutTemplate(Map<String, List<String[]>> roomMeasures, Map<String, String> databaseConnectionMap) {
        // Generate an empty queue of arrays for generating panels (at most three to accommodate screen resolutions)
        // At the moment, one horizontal row (encapsulated by one array) is designed to contain only at most have 3 panels
        Queue<TemplatePanel[]> panelQueue = new ArrayDeque<>();
        // For non-rooms, thresholds should be empty
        Map<String, String[]> thresholdMap = new HashMap<>();
        // If this item group is rooms and have thresholds available, process them
        if (roomMeasures.containsKey(StringHelper.THRESHOLD_KEY)) {
            // Retrieve the thresholds and process it for generating the charts
            List<String[]> thresholdList = roomMeasures.get(StringHelper.THRESHOLD_KEY);
            thresholdMap = processThresholdsToMap(thresholdList);
            roomMeasures.remove(StringHelper.THRESHOLD_KEY); // remove the threshold after processing
        }
        // For each of the measures, create a set of chart
        for (String measure : roomMeasures.keySet()) {
            // Take note to exclude the rooms key as that is not required
            if (!measure.equals(StringHelper.ROOM_KEY)) {
                // Retrieve the relevant database name and ID from the first item
                // Assumes that each measure of a specific asset type belongs to only one database
                String database = roomMeasures.get(measure).get(0)[3];
                String databaseID = databaseConnectionMap.get(database);
                // Assume the unit of each measure for the rooms is consistent
                String unit = roomMeasures.get(measure).get(0)[4];
                // Retrieves the thresholds if it is available, else, it should return an empty array
                String[] thresholds = thresholdMap.isEmpty() ? new String[]{} :
                        thresholdMap.containsKey(measure) ? thresholdMap.get(measure) : new String[]{};
                // Generate a gauge panel displaying the average of all time series
                Gauge averageGaugePanel = new Gauge(measure, StringHelper.ROOM_KEY, unit, databaseID, roomMeasures.get(measure), thresholds, true);
                // Generate a gauge and time series chart
                Gauge gaugePanel = new Gauge(measure, StringHelper.ROOM_KEY, unit, databaseID, roomMeasures.get(measure), thresholds);
                TimeSeriesChart tsChart = new TimeSeriesChart(measure, StringHelper.ROOM_KEY, unit, databaseID, roomMeasures.get(measure), thresholds);
                TemplatePanel[] panelArr = new TemplatePanel[]{averageGaugePanel, gaugePanel, tsChart};
                panelQueue.offer(panelArr);
            }
        }
        return panelQueue;
    }

    /**
     * Generates the layout template for the systems/smart meters and all their associated measures. Two gauge charts and one time series chart
     * will be generated per measure of systems. The first gauge chart displays an average latest value of all available systems,
     * whereas the second displays the individual latest values.
     *
     * @param systemMeasures        A map containing all measures and their metadata to construct the panels for systems.
     * @param databaseConnectionMap A map linking each database to its connection ID.
     * @return A queue containing all the measure panels available for the systems.
     */
    public static Queue<TemplatePanel[]> genSystemsLayoutTemplate(Map<String, List<String[]>> systemMeasures, Map<String, String> databaseConnectionMap) {
        // Generate an empty queue of arrays for generating panels (at most three to accommodate screen resolutions)
        // At the moment, one horizontal row (encapsulated by one array) is designed to contain only at most have 3 panels
        Queue<TemplatePanel[]> panelQueue = new ArrayDeque<>();
        // For each of the measures, create a set of chart
        for (String measure : systemMeasures.keySet()) {
            // Take note to exclude the systems key as that is not required
            if (!measure.equals(StringHelper.SYSTEM_KEY)) {
                // Retrieve the relevant database name and ID from the first item
                // Assumes that each measure of a specific systems belongs to only one database
                String database = systemMeasures.get(measure).get(0)[3];
                String databaseID = databaseConnectionMap.get(database);
                // Assume the unit of each measure for the systems is consistent
                String unit = systemMeasures.get(measure).get(0)[4];
                // Generate a gauge panel displaying the average of all time series with no thresholds
                Gauge averageGaugePanel = new Gauge(measure, StringHelper.SYSTEM_KEY, unit, databaseID, systemMeasures.get(measure), new String[]{}, true);
                // Generate a gauge and time series chart with no thresholds
                Gauge gaugePanel = new Gauge(measure, StringHelper.SYSTEM_KEY, unit, databaseID, systemMeasures.get(measure), new String[]{});
                BarChart barChart = new BarChart(measure, StringHelper.SYSTEM_KEY, unit, databaseID, systemMeasures.get(measure));
                TemplatePanel[] panelArr = new TemplatePanel[]{averageGaugePanel, gaugePanel, barChart};
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
    private static Map<String, String[]> processThresholdsToMap(List<String[]> thresholdList) {
        Map<String, String[]> thresholdMap = new HashMap<>();
        for (String[] threshold : thresholdList) {
            thresholdMap.put(threshold[0], new String[]{threshold[1], threshold[2]});
        }
        return thresholdMap;
    }
}
