package uk.ac.cam.cares.jps.agent.dashboard.json.templating;

import uk.ac.cam.cares.jps.agent.dashboard.json.panel.layout.TemporalInterval;
import uk.ac.cam.cares.jps.agent.dashboard.utils.StringHelper;

import java.util.*;
import java.util.stream.Stream;

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
     * @param timeSeries            A map of all assets and rooms mapped to their time series.
     */
    public TemplatingModel(Map<String, String> databaseConnectionMap, Map<String, Map<String, List<String[]>>> timeSeries) {
        // Initialise a queue to store these template variables
        Queue<TemplateVariable> variableQueue = new ArrayDeque<>();
        if (!timeSeries.isEmpty()) {
            genTrendFilter();
            // If there are values, retrieve the first connection ID, as the postgres variables in Grafana requires a connection ID to function
            // But for processing facility items, any ID will do and does not matter
            genFacilityItemFilters(timeSeries, databaseConnectionMap.values().iterator().next());
        }
        // For each asset type or rooms available
        for (Map.Entry<String, Map<String, List<String[]>>> entry : timeSeries.entrySet()) {
            String item = entry.getKey();
            Map<String, List<String[]>> measures = entry.getValue();
            // For each of the measures, create a postgres variable that is tied to their asset type or room custom variable
            for (Map.Entry<String, List<String[]>> measureEntry : measures.entrySet()) {
                String measure = measureEntry.getKey();
                List<String[]> measureList = measureEntry.getValue();
                // Take note to exclude the assets, rooms, systems, and threshold keys as they are not required
                if (!measure.equals(StringHelper.ASSET_KEY) && !measure.equals(StringHelper.ROOM_KEY) && !measure.equals(StringHelper.SYSTEM_KEY) && !measure.equals(StringHelper.THRESHOLD_KEY)) {
                    // Retrieve the relevant database and database ID from the first item
                    // Assumes that each measure of a specific asset type belongs to only one database
                    String database = measureList.get(0)[3];
                    PostgresVariable postgresVariable = new PostgresVariable(measure, item, databaseConnectionMap.get(database), measureList);
                    variableQueue.offer(postgresVariable);
                }
            }
        }
        // While there are still items in the queue,
        while (!variableQueue.isEmpty()) {
            // Retrieve the variable to be added, as well as remove it from the queue
            addVariable(variableQueue.poll());
        }
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
     * Generate the filter for daily, weekly, or monthly intervals that the trends-related chart should display.
     */
    private void genTrendFilter() {
        String[] temporalIntervals = new String[]{TemporalInterval.DAILY_OVER_WEEK, TemporalInterval.DAILY_OVER_MONTH, TemporalInterval.WEEKLY_OVER_MONTH, TemporalInterval.MONTHLY};
        CustomVariable intervalFilterOptions = new CustomVariable(StringHelper.INTERVAL_VARIABLE_NAME, TIME_INTERVAL_FILTER_DESCRIPTION,
                temporalIntervals, 0, false, false);
        addVariable(intervalFilterOptions);
    }

    /**
     * Generate the facility and item type filters for the dashboard.
     *
     * @param timeSeries   A map of all assets and rooms mapped to their time series.
     * @param connectionId A Grafana connection ID.
     */
    private void genFacilityItemFilters(Map<String, Map<String, List<String[]>>> timeSeries, String connectionId) {
        Map<String, List<String[]>> facilityMapping = timeSeries.get(StringHelper.FACILITY_KEY);
        // Remove the facility key as it is no longer required
        timeSeries.remove(StringHelper.FACILITY_KEY);
        // Create a new custom variable for all facilities
        // Retrieve all keys for the mappings and transformed it into an array for the input
        CustomVariable facilityFilterOptions = new CustomVariable("Facilities", FACILITY_FILTER_DESCRIPTION, facilityMapping.keySet().toArray(String[]::new), 0);
        addVariable(facilityFilterOptions);

        // The next goal is to create a map for item type - either an asset type or room, that is mapped to their facility and individual elements
        // Format {assetType: {facility1:[asset1, asset2], facility2:[asset3,asset4]}}
        // This map is necessary to make it easier to generate a postgres variable from the mappings
        Map<String, Map<String, List<String>>> typeFacilityItemMapping = new HashMap<>();
        // Inverse the facility mapping so that it is much faster to access
        Map<String, List<String>> itemToFacilityMapping = inverseMap(facilityMapping);
        // Iterate through the assets and rooms by type for the mapping
        for (Map.Entry<String, Map<String, List<String[]>>> entry : timeSeries.entrySet()) {
            String itemType = entry.getKey();
            Map<String, List<String[]>> itemTypeContents = entry.getValue();
            typeFacilityItemMapping.put(itemType, new HashMap<>()); // Initialise an empty map for this item
            Map<String, List<String>> facilityItemMapping = typeFacilityItemMapping.get(itemType);
            // Seek to retrieve either the list of individual rooms, assets, or systems associated with this type
            String nestedKey = StringHelper.ASSET_KEY;
            if (itemType.equals(StringHelper.ROOM_KEY)) {
                nestedKey = StringHelper.ROOM_KEY;
            } else if (itemType.equals(StringHelper.SYSTEM_KEY)) {
                nestedKey = StringHelper.SYSTEM_KEY;
            }
            // Process the list into an array of items
            String[] itemsArray = itemTypeContents.get(nestedKey).stream().flatMap(Stream::of).distinct().toArray(String[]::new);
            // Iterate through each item and add into the mapping
            for (String itemName : itemsArray) {
                List<String> facilityNames = itemToFacilityMapping.get(itemName); // The facilities that this item belongs to
                // For each facility
                for (String facilityName : facilityNames) {
                    // Initialise a new array list if there is no pre-existing key
                    if (!facilityItemMapping.containsKey(facilityName))
                        facilityItemMapping.put(facilityName, new ArrayList<>());
                    // Add the item accordingly to the list
                    facilityItemMapping.get(facilityName).add(itemName);
                }
            }
        }

        // Create a postgres variable for each item type that maps each of its items to its associated facility
        typeFacilityItemMapping.forEach((itemType, associatedItems) -> {
            PostgresVariable itemFilterOptions = new PostgresVariable(itemType, associatedItems, connectionId);
            addVariable(itemFilterOptions);
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

    /**
     * Inverse the input map to get the mappings for each item to its key. Note that as one item may belong to multiple keys;
     * the keys are stored as a list instead.
     *
     * @param keyToItemsMap A key to items map. Assumes that there is only one array in the list.
     * @return the inverse map mapping each item to its list of key(s).
     */
    private Map<String, List<String>> inverseMap(Map<String, List<String[]>> keyToItemsMap) {
        Map<String, List<String>> itemToKeyMap = new HashMap<>();
        for (Map.Entry<String, List<String[]>> entry : keyToItemsMap.entrySet()) {
            String key = entry.getKey();
            List<String[]> associatedItems = entry.getValue();
            String[] values = associatedItems.get(0);
            for (String item : values) {
                // Initialise a new list if the key does not exist
                itemToKeyMap.computeIfAbsent(item, keyList -> new ArrayList<>());
                // Append directly to the list
                itemToKeyMap.get(item).add(key);
            }
        }
        return itemToKeyMap;
    }
}
