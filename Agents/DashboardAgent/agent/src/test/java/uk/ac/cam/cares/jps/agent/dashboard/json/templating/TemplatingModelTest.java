package uk.ac.cam.cares.jps.agent.dashboard.json.templating;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import uk.ac.cam.cares.jps.agent.dashboard.TestUtils;
import uk.ac.cam.cares.jps.agent.dashboard.utils.StringHelper;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.*;

public class TemplatingModelTest {
    private static Map<String, String> SAMPLE_DB_CONNECTION_ID_MAP;
    private static Map<String, Map<String, List<String[]>>> SAMPLE_ASSETS;
    private static Map<String, Map<String, List<String[]>>> SAMPLE_ROOMS;


    @BeforeAll
    static void genSampleData() {
        SAMPLE_ASSETS = TestUtils.genSampleAssetMeasureMap();
        SAMPLE_ASSETS = TestUtils.addSampleFacilityData(SAMPLE_ASSETS, true, false);
        SAMPLE_DB_CONNECTION_ID_MAP = TestUtils.genSampleDatabaseConnectionMap();
        SAMPLE_ROOMS = TestUtils.genSampleRoomMeasureMap(true);
        SAMPLE_ROOMS = TestUtils.addSampleFacilityData(SAMPLE_ROOMS, false, true);
    }

    @Test
    void testConstruct_AssetsOnly() {
        // Construct and execute the method
        String result = new TemplatingModel(SAMPLE_DB_CONNECTION_ID_MAP, SAMPLE_ASSETS).construct();
        // Test outputs
        Map<String, Map<String, List<String[]>>> sampleMap = TestUtils.genSampleAssetMeasureMap();
        sampleMap = TestUtils.addSampleFacilityData(sampleMap, true, false);
        assertEquals(genExpectedJsonSyntax(SAMPLE_DB_CONNECTION_ID_MAP, sampleMap), result);
    }

    @Test
    void testConstruct_RoomsOnly() {
        // Construct and execute the method
        String result = new TemplatingModel(SAMPLE_DB_CONNECTION_ID_MAP, SAMPLE_ROOMS).construct();
        // Test outputs
        Map<String, Map<String, List<String[]>>> sampleMap = TestUtils.genSampleRoomMeasureMap(true);
        sampleMap = TestUtils.addSampleFacilityData(sampleMap, false, true);
        assertEquals(genExpectedJsonSyntax(SAMPLE_DB_CONNECTION_ID_MAP, sampleMap), result);
    }

    public static String genExpectedJsonSyntax(Map<String, String> databaseConnectionMap, Map<String, Map<String, List<String[]>>> organisationMapping) {
        StringBuilder builder = new StringBuilder();
        StringBuilder tempBuilder = new StringBuilder();
        builder.append("{\"enable\": true,\"list\": [");
        // Only generate these variables if there are values
        if (!organisationMapping.isEmpty()) genFacilityItemTypeVariables(tempBuilder, organisationMapping, databaseConnectionMap.values().iterator().next());
        for (String itemType : organisationMapping.keySet()) {
            Map<String, List<String[]>> itemMeasures = organisationMapping.get(itemType);
            for (String measure : itemMeasures.keySet()) {
                if (!measure.equals(StringHelper.ASSET_KEY) && !measure.equals(StringHelper.ROOM_KEY) && !measure.equals(StringHelper.THRESHOLD_KEY)) {
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

    private static void genFacilityItemTypeVariables(StringBuilder tempBuilder, Map<String, Map<String, List<String[]>>> organisationMapping, String connectionId) {
        Map<String, List<String[]>> facilityMapping = organisationMapping.get(StringHelper.FACILITY_KEY);
        // Remove the facility key as it is no longer required
        organisationMapping.remove(StringHelper.FACILITY_KEY);
        // Generate a custom variable for all facilities
        tempBuilder.append(CustomVariableTest.genExpectedCustomVariableSyntax("Facilities", facilityMapping.keySet().toArray(String[]::new), 0));

        // Generates a mapping in Format {assetType: {facility1:[asset1, asset2], facility2:[asset3,asset4]}}
        Map<String, Map<String, List<String>>> typeFacilityItemMapping = new HashMap<>();
        // Inverse the facility mapping so that it is much faster to access which item belongs to a facility
        Map<String, String> itemToFacilityMapping = new HashMap<>();
        for (String key : facilityMapping.keySet()) {
            String[] values = facilityMapping.get(key).get(0);
            for (String item : values) {
                itemToFacilityMapping.put(item, key);
            }
        }

        // Iterate through the assets and rooms by type for the mapping
        for (String itemType : organisationMapping.keySet()) {
            typeFacilityItemMapping.put(itemType, new HashMap<>()); // Initialise an empty map for this item
            Map<String, List<String>> facilityItemMapping = typeFacilityItemMapping.get(itemType);
            // Seek to retrieve either the list of individual rooms or assets associated with this type
            String nestedKey = itemType.equals(StringHelper.ROOM_KEY) ? StringHelper.ROOM_KEY : StringHelper.ASSET_KEY; // The key name will vary depending on if it is a room or asset
            // Process the list into an array of items
            String[] itemsArray = organisationMapping.get(itemType).get(nestedKey).stream().flatMap(Stream::of).distinct().toArray(String[]::new);
            // Iterate through each item and add into the mapping
            for (String itemName : itemsArray) {
                String facilityName = itemToFacilityMapping.get(itemName); // The facility that this item belongs to
                // Initialise a new array list if there is no pre-existing key
                if (!facilityItemMapping.containsKey(facilityName))
                    facilityItemMapping.put(facilityName, new ArrayList<>());
                // Add the item accordingly to the list
                facilityItemMapping.get(facilityName).add(itemName);
            }
        }

        // Add postgres variables to filter each item type
        typeFacilityItemMapping.forEach((itemType, nestedMap) -> {
            tempBuilder.append(",").append(PostgresVariableTest.genExpectedPostgresVarSyntaxForItemFilter(itemType, connectionId, nestedMap));
        });
    }
}