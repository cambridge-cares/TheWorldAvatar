package uk.ac.cam.cares.jps.agent.dashboard.json.templating;

import uk.ac.cam.cares.jps.agent.dashboard.datamodel.Measure;
import uk.ac.cam.cares.jps.agent.dashboard.datamodel.Organisation;
import uk.ac.cam.cares.jps.agent.dashboard.json.panel.layout.TemporalInterval;
import uk.ac.cam.cares.jps.agent.dashboard.utils.StringHelper;

import java.util.*;

/**
 * A Java representation of a JSON-like model that encapsulates and enforces information
 * about the variable syntax specific to Grafana dashboard.
 *
 * @author qhouyee
 */
public class TemplatingModel {
    private final StringBuilder variablesSyntax = new StringBuilder();
    private static final String TIME_INTERVAL_FILTER_DESCRIPTION = "A filter to display the time interval requested by the user in the trend related charts.";
    private static final String FACILITY_FILTER_DESCRIPTION = "A filter at the facility level to view the specified facilities and their associated measures.";

    /**
     * Constructor that process customisable options for the templating variable in Grafana's JSON model.
     *
     * @param databaseConnectionMap A map linking each database to its connection ID.
     * @param organisation          The organisation's time series data model.
     */
    public TemplatingModel(Map<String, String> databaseConnectionMap, Organisation organisation) {
        // Retrieve the first connection ID, as the postgres variables in Grafana requires a connection ID to function
        // but any ID will do and does not matter
        String connectionID = databaseConnectionMap.values().iterator().next();
        this.genTemporalSelectors(connectionID);
        this.genFacilityItemFilters(organisation, connectionID);
        this.genItemMeasureFilters(organisation, connectionID);
    }

    /**
     * Construct the JSON model as a String.
     *
     * @return The JSON model syntax as a String.
     */
    public String construct() {
        // Enable templating in the dashboard
        return "{\"enable\": true," +
                // List of all variables
                "\"list\": [" + this.variablesSyntax + "]" +
                "}";
    }

    /**
     * Generate all temporal-related selectors, allowing the related chart to display according to the selected option.
     * The first selector is for daily, weekly, or monthly intervals that the trends-related chart should display.
     * The second selector is for comparing the reference month with the current month trends.
     */
    private void genTemporalSelectors(String connectionID) {
        Queue<String> temporalIntervals = new ArrayDeque<>();
        temporalIntervals.offer(TemporalInterval.DAILY_OVER_WEEK);
        temporalIntervals.offer(TemporalInterval.DAILY_OVER_MONTH);
        temporalIntervals.offer(TemporalInterval.WEEKLY_OVER_MONTH);
        temporalIntervals.offer(TemporalInterval.MONTHLY);
        CustomVariable intervalSelector = new CustomVariable(StringHelper.INTERVAL_VARIABLE_NAME, TIME_INTERVAL_FILTER_DESCRIPTION,
                temporalIntervals, 2, false, false);
        addVariable(intervalSelector);
        PostgresVariable refMonthSelector = new PostgresVariable(StringHelper.REF_MONTH_VARIABLE_NAME, connectionID, TemporalInterval.getMonthMap());
        addVariable(refMonthSelector);
    }

    /**
     * Generate the facility and item group filters for the dashboard.
     *
     * @param organisation The organisation's time series data model.
     * @param connectionId A Grafana connection ID.
     */
    private void genFacilityItemFilters(Organisation organisation, String connectionId) {
        // Generate the facility filter variable
        Queue<String> facilityNames = organisation.getFacilities();
        CustomVariable facilityFilterOptions = new CustomVariable("Facilities", FACILITY_FILTER_DESCRIPTION, facilityNames, 0);
        addVariable(facilityFilterOptions);
        // Generate the item group filter variable for each item group
        organisation.getAllItemGroups().forEach(itemGroup -> {
            Queue<String[]> items = organisation.getFacilityItemInventory(itemGroup);
            PostgresVariable itemFilterOptions = new PostgresVariable(itemGroup, items, connectionId);
            addVariable(itemFilterOptions);
        });
    }

    /**
     * Generate the item measure filters for the dashboard.
     *
     * @param organisation The organisation's time series data model.
     * @param connectionId A Grafana connection ID.
     */
    private void genItemMeasureFilters(Organisation organisation, String connectionId) {
        // Retrieve all associated items and their measures for each group
        organisation.getAllItemGroups().forEach(group -> {
            organisation.getAllMeasureNames(group).forEach(measure -> {
                // Stores the metadata of item name and column name
                Measure currentMeasure = organisation.getMeasure(group, measure);
                Queue<String[]> availableMeasureMetadata = currentMeasure.getTimeSeriesData();
                // Only create a new variable for each measure of an item group if any metadata is available
                if (!availableMeasureMetadata.isEmpty()) {
                    PostgresVariable postgresVariable = new PostgresVariable(measure, group, availableMeasureMetadata, connectionId);
                    addVariable(postgresVariable);
                }
            });
        });
    }

    /**
     * Add a variable syntax to the field storing this information.
     *
     * @param variable The template variable to append.
     */
    private void addVariable(TemplateVariable variable) {
        // Append a comma before that variable if it is not the first variable
        if (this.variablesSyntax.length() != 0) this.variablesSyntax.append(",");
        // Construct its syntax and append it to the key syntax
        this.variablesSyntax.append(variable.construct());
    }
}
