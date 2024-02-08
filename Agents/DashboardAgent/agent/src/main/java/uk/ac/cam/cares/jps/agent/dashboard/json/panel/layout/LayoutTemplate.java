package uk.ac.cam.cares.jps.agent.dashboard.json.panel.layout;

import uk.ac.cam.cares.jps.agent.dashboard.datamodel.Measure;
import uk.ac.cam.cares.jps.agent.dashboard.datamodel.Organisation;
import uk.ac.cam.cares.jps.agent.dashboard.datamodel.Threshold;
import uk.ac.cam.cares.jps.agent.dashboard.json.panel.types.*;
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
     * @param organisation          A data model containing all time series information within the specified organisation.
     * @param databaseConnectionMap A map linking each database to its connection ID.
     * @return A queue containing all the measure panels available for each asset type.
     */
    public static Queue<TemplatePanel[]> genAssetLayoutTemplate(String assetType, Organisation organisation, Map<String, String> databaseConnectionMap) {
        // Generate an empty queue of arrays for generating panels (at most three to accommodate screen resolutions)
        // At the moment, one horizontal row (encapsulated by one array) is designed to contain only at most have 3 panels
        Queue<TemplatePanel[]> panelQueue = new ArrayDeque<>();
        // For each of the measures, create a set of charts
        organisation.getAllMeasureNames(assetType).forEach(measure -> {
            Measure currentMeasure = organisation.getMeasure(assetType, measure);
            // Retrieve the relevant database name and ID from any item
            // Assumes that each measure of a specific item group belongs to only one database
            String databaseID = databaseConnectionMap.get(currentMeasure.getTimeSeriesDatabase());
            // Generate a gauge and time series chart with no thresholds
            Gauge gaugePanel = new Gauge(currentMeasure, assetType, databaseID, new String[]{}, false);
            TimeSeriesChart tsChart = new TimeSeriesChart(currentMeasure, assetType, databaseID, new String[]{});
            TemplatePanel[] panelArr = new TemplatePanel[]{gaugePanel, tsChart};
            panelQueue.offer(panelArr);
        });
        return panelQueue;
    }

    /**
     * Generates the layout template for the rooms and all their associated measures. Two gauge charts and one time series chart
     * will be generated per measure of rooms. The first gauge chart displays an average latest value of all available rooms,
     * whereas the second displays the individual latest values.
     *
     * @param organisation          A data model containing all time series information within the specified organisation.
     * @param databaseConnectionMap A map linking each database to its connection ID.
     * @return A queue containing all the measure panels available for the rooms.
     */
    public static Queue<TemplatePanel[]> genRoomLayoutTemplate(Organisation organisation, Map<String, String> databaseConnectionMap) {
        // Generate an empty queue of arrays for generating panels (at most three to accommodate screen resolutions)
        // At the moment, one horizontal row (encapsulated by one array) is designed to contain only at most 3 panels
        Queue<TemplatePanel[]> panelQueue = new ArrayDeque<>();
        // Assumes that thresholds are the same for all facilities as  Grafana can only show one set of threshold per panel
        // WIP: Figure out a better way to display thresholds for the same measure and item group but different facilities
        Queue<Threshold> thresholdQueue = organisation.getThresholds(StringHelper.ROOM_KEY);
        Threshold threshold = thresholdQueue.size()>0 ? thresholdQueue.poll() : null;
        // For each of the measures, create a set of charts
        organisation.getAllMeasureNames(StringHelper.ROOM_KEY).forEach(measure -> {
            Measure currentMeasure = organisation.getMeasure(StringHelper.ROOM_KEY, measure);
            // Retrieve the relevant database name and ID from any item
            // Assumes that each measure of a specific item group belongs to only one database
            String databaseID = databaseConnectionMap.get(currentMeasure.getTimeSeriesDatabase());
            // Retrieves the thresholds if it is available, else, it should return an empty array
            String[] thresholds = new String[]{};
            if (threshold!=null && threshold.contains(measure)) {
                thresholds = threshold.getThreshold(measure);
            }
            // Generate a gauge panel displaying the average of all time series
            Gauge averageGaugePanel = new Gauge(currentMeasure, StringHelper.ROOM_KEY, databaseID, thresholds, true);
            // Generate a gauge and time series chart
            Gauge gaugePanel = new Gauge(currentMeasure, StringHelper.ROOM_KEY, databaseID, thresholds, false);
            TimeSeriesChart tsChart = new TimeSeriesChart(currentMeasure, StringHelper.ROOM_KEY, databaseID, thresholds);
            TemplatePanel[] panelArr = new TemplatePanel[]{averageGaugePanel, gaugePanel, tsChart};
            panelQueue.offer(panelArr);
        });
        return panelQueue;
    }

    /**
     * Generates the layout template for the systems/smart meters and all their associated measures. One pie chart, three bar charts, and one variable panel
     * will be generated per measure of systems. The first pie chart displays the distribution of the systems for that measures. The second bar chart displays the current month measure.
     * The third variable panel enables users to select the interval periods for the fourth and fifth charts. The fourth chart displays the trends of the measures for the last period.
     * The fifth displays the current period trends.
     *
     * @param organisation          A data model containing all time series information within the specified organisation.
     * @param databaseConnectionMap A map linking each database to its connection ID.
     * @return A queue containing all the measure panels available for the systems.
     */
    public static Queue<TemplatePanel[]> genSystemsLayoutTemplate(Organisation organisation, Map<String, String> databaseConnectionMap) {
        // Generate an empty queue of arrays for generating panels (at most three to accommodate screen resolutions)
        Queue<TemplatePanel[]> panelQueue = new ArrayDeque<>();
        String intervalVarChartDescription = "Select the required time interval for the current and last period trends on the right.";
        String refMonthChartDescription = "Select the reference month for comparison with the current month through the charts on the right.";
        // For each of the measures, create a set of chart
        organisation.getAllMeasureNames(StringHelper.SYSTEM_KEY).forEach(measure -> {
            Measure currentMeasure = organisation.getMeasure(StringHelper.SYSTEM_KEY, measure);
            // Retrieve the relevant database name and ID from any item
            // Assumes that each measure of a specific item group belongs to only one database
            String databaseID = databaseConnectionMap.get(currentMeasure.getTimeSeriesDatabase());
            // Generate related panels
            PieChart distributionPanel = new PieChart(currentMeasure, StringHelper.SYSTEM_KEY, databaseID);
            BarChart currentMonthMeasureChart = new BarChart(currentMeasure, StringHelper.SYSTEM_KEY, databaseID, 1);
            TemplatePanel[] panelArr = new TemplatePanel[]{distributionPanel, currentMonthMeasureChart};
            panelQueue.offer(panelArr);
            VariablePanel timeIntervalChart = new VariablePanel(StringHelper.INTERVAL_VARIABLE_NAME, intervalVarChartDescription);
            BarChart lastPeriodMeasureChart = new BarChart(currentMeasure, StringHelper.SYSTEM_KEY, databaseID, 2);
            BarChart currentPeriodMeasureChart = new BarChart(currentMeasure, StringHelper.SYSTEM_KEY, databaseID, 3);
            panelArr = new TemplatePanel[]{timeIntervalChart, lastPeriodMeasureChart, currentPeriodMeasureChart};
            panelQueue.offer(panelArr);
            VariablePanel refMonthChart = new VariablePanel(StringHelper.REF_MONTH_VARIABLE_NAME, refMonthChartDescription);
            BarChart dailyComparisonChart = new BarChart(currentMeasure, StringHelper.SYSTEM_KEY, databaseID, 4);
            BarChart weeklyComparisonChart = new BarChart(currentMeasure, StringHelper.SYSTEM_KEY, databaseID, 5);
            panelArr = new TemplatePanel[]{refMonthChart, dailyComparisonChart, weeklyComparisonChart};
            panelQueue.offer(panelArr);
        });
        return panelQueue;
    }
}
