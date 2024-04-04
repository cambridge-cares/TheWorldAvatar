package uk.ac.cam.cares.jps.agent.dashboard.json.templating;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import uk.ac.cam.cares.jps.agent.dashboard.TestUtils;
import uk.ac.cam.cares.jps.agent.dashboard.json.panel.layout.TemporalInterval;
import uk.ac.cam.cares.jps.agent.dashboard.utils.StringHelper;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.*;

public class TemplatingModelTest {
    private static Map<String, String> sampleDbConnectionIdMap;
    private static Map<String, Map<String, List<String[]>>> sampleAssets;
    private static Map<String, Map<String, List<String[]>>> sampleRooms;
    private static Map<String, Map<String, List<String[]>>> sampleSystems;
    private static final String TIME_INTERVAL_FILTER_DESCRIPTION = "A filter to display the time interval requested by the user in the trend related charts.";
    private static final String FACILITY_FILTER_DESCRIPTION = "A filter at the facility level to view the specified facilities and their associated measures.";

    @BeforeAll
    static void genSampleData() {
        sampleAssets = TestUtils.genSampleAssetMeasureMap();
        sampleAssets = TestUtils.addSampleFacilityData(sampleAssets, true, false, false);
        sampleDbConnectionIdMap = TestUtils.genSampleDatabaseConnectionMap();
        sampleRooms = TestUtils.genSampleRoomMeasureMap(true);
        sampleRooms = TestUtils.addSampleFacilityData(sampleRooms, false, true, false);
        sampleSystems = TestUtils.genSampleSystemMeasureMap();
        sampleSystems = TestUtils.addSampleFacilityData(sampleSystems, false, false, true, true);
    }

    @Test
    void testConstruct_AssetsOnly() {
        // Construct and execute the method
        String result = new TemplatingModel(sampleDbConnectionIdMap, sampleAssets).construct();
        // Test outputs
        Map<String, Map<String, List<String[]>>> sampleMap = TestUtils.genSampleAssetMeasureMap();
        sampleMap = TestUtils.addSampleFacilityData(sampleMap, true, false, false);
        assertEquals(genExpectedJsonSyntax(sampleDbConnectionIdMap, sampleMap), result);
    }

    @Test
    void testConstruct_RoomsOnly() {
        // Construct and execute the method
        String result = new TemplatingModel(sampleDbConnectionIdMap, sampleRooms).construct();
        // Test outputs
        Map<String, Map<String, List<String[]>>> sampleMap = TestUtils.genSampleRoomMeasureMap(true);
        sampleMap = TestUtils.addSampleFacilityData(sampleMap, false, true, false);
        assertEquals(genExpectedJsonSyntax(sampleDbConnectionIdMap, sampleMap), result);
    }

    @Test
    void testConstruct_SystemsOnly() {
        // Construct and execute the method
        String result = new TemplatingModel(sampleDbConnectionIdMap, sampleSystems).construct();
        // Test outputs
        Map<String, Map<String, List<String[]>>> sampleMap = TestUtils.genSampleSystemMeasureMap();
        sampleMap = TestUtils.addSampleFacilityData(sampleMap, false, false, true, true);
        assertEquals(genExpectedJsonSyntax(sampleDbConnectionIdMap, sampleMap), result);
    }

    public static String genExpectedJsonSyntax(Map<String, String> databaseConnectionMap, Map<String, Map<String, List<String[]>>> organisationMapping) {
        StringBuilder builder = new StringBuilder();
        StringBuilder tempBuilder = new StringBuilder();
        builder.append("{\"enable\": true,\"list\": [");
        // Only generate these variables if there are values
        if (!organisationMapping.isEmpty()) {
            genTrendFilter(tempBuilder);
            genFacilityItemTypeVariables(tempBuilder, organisationMapping, databaseConnectionMap.values().iterator().next());
        }
        for (String itemType : organisationMapping.keySet()) {
            Map<String, List<String[]>> itemMeasures = organisationMapping.get(itemType);
            for (String measure : itemMeasures.keySet()) {
                if (!measure.equals(StringHelper.ASSET_KEY) && !measure.equals(StringHelper.ROOM_KEY) && !measure.equals(StringHelper.SYSTEM_KEY) && !measure.equals(StringHelper.THRESHOLD_KEY)) {
                    List<String[]> itemMeasureList = itemMeasures.get(measure);
                    String dbName = itemMeasureList.get(0)[3];
                    tempBuilder.append(",")
                            .append(PostgresVariableTest.genExpectedPostgresVarSyntaxForMeasureFilter(measure, itemType, databaseConnectionMap.get(dbName), itemMeasureList));
                }
            }
        }
        builder.append(tempBuilder)
                .append("]}");
        return builder.toString();
    }

    private static void genTrendFilter(StringBuilder tempBuilder) {
        String[] temporalIntervals = new String[]{TemporalInterval.DAILY_OVER_WEEK, TemporalInterval.DAILY_OVER_MONTH, TemporalInterval.WEEKLY_OVER_MONTH, TemporalInterval.MONTHLY};
        tempBuilder.append(CustomVariableTest.genExpectedCustomVariableSyntax(StringHelper.INTERVAL_VARIABLE_NAME, TIME_INTERVAL_FILTER_DESCRIPTION, temporalIntervals, 0, false, false))
                .append(",");
    }

    private static void genFacilityItemTypeVariables(StringBuilder tempBuilder, Map<String, Map<String, List<String[]>>> organisationMapping, String connectionId) {
        Map<String, List<String[]>> facilityMapping = organisationMapping.get(StringHelper.FACILITY_KEY);
        // Remove the facility key as it is no longer required
        organisationMapping.remove(StringHelper.FACILITY_KEY);
        // Generate a custom variable for all facilities
        tempBuilder.append(CustomVariableTest.genExpectedCustomVariableSyntax("Facilities", FACILITY_FILTER_DESCRIPTION, facilityMapping.keySet().toArray(String[]::new), 0, true, true));

        // Generates a mapping in Format {assetType: {facility1:[asset1, asset2], facility2:[asset3,asset4]}}
        Map<String, Map<String, List<String>>> typeFacilityItemMapping = new HashMap<>();
        // Inverse the facility mapping so that it is much faster to access which item belongs to a facility
        Map<String, List<String>> itemToFacilityMapping = new HashMap<>();
        for (String key : facilityMapping.keySet()) {
            String[] values = facilityMapping.get(key).get(0);
            for (String item : values) {
                // Initialise a new list if the key does not exist
                if (!itemToFacilityMapping.containsKey(item)) itemToFacilityMapping.put(item, new ArrayList<>());
                // Append directly to the list
                itemToFacilityMapping.get(item).add(key);
            }
        }

        // Iterate through the assets and rooms by type for the mapping
        for (String itemType : organisationMapping.keySet()) {
            typeFacilityItemMapping.put(itemType, new HashMap<>()); // Initialise an empty map for this item
            Map<String, List<String>> facilityItemMapping = typeFacilityItemMapping.get(itemType);
            // Seek to retrieve either the list of individual items associated with this type
            String nestedKey = itemType.equals(StringHelper.ROOM_KEY) ? StringHelper.ROOM_KEY :
                    itemType.equals(StringHelper.SYSTEM_KEY) ? StringHelper.SYSTEM_KEY : StringHelper.ASSET_KEY; // The key name will vary depending on their type
            // Process the list into an array of items
            String[] itemsArray = organisationMapping.get(itemType).get(nestedKey).stream().flatMap(Stream::of).distinct().toArray(String[]::new);
            // Iterate through each item and add into the mapping
            for (String itemName : itemsArray) {
                List<String> facilityNames = itemToFacilityMapping.get(itemName); // The facility that this item belongs to
                for (String facilityName : facilityNames) {// Initialise a new array list if there is no pre-existing key
                    if (!facilityItemMapping.containsKey(facilityName))
                        facilityItemMapping.put(facilityName, new ArrayList<>());
                    // Add the item accordingly to the list
                    facilityItemMapping.get(facilityName).add(itemName);
                }
            }
        }

        // Add postgres variables to filter each item type
        typeFacilityItemMapping.forEach((itemType, nestedMap) -> {
            tempBuilder.append(",").append(PostgresVariableTest.genExpectedPostgresVarSyntaxForItemFilter(itemType, connectionId, nestedMap));
        });
    }
}