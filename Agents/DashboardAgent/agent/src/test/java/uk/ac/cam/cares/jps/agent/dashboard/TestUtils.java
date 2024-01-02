package uk.ac.cam.cares.jps.agent.dashboard;

import uk.ac.cam.cares.jps.agent.dashboard.utils.StringHelper;

import java.io.File;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class TestUtils {
    private static final String DASHBOARD_USER = "dashboard.user";
    private static final String DASHBOARD_PASSWORD = "dashboard.pass";
    public static final String BASE_URI = "http://www.test.org/example/";
    public static final String FACILITY_ONE = "Home";
    public static final String FACILITY_TWO = "Bakery";
    public static final String ASSET_TYPE_ONE = "Lamp";
    public static final String ASSET_LAMP_ONE = "L1";
    public static final String ASSET_LAMP_TWO = "L2";
    public static final String ASSET_TYPE_TWO = "Oven";
    public static final String ASSET_OVEN = "O-1";
    public static final String MEASURE_COMMON = "Electricity Consumption";
    public static final String MEASURE_HEAT = "Heat Consumption";
    public static final String MEASURE_THERMAL = "Thermal Consumption";
    public static final String DATABASE_ELEC = "electricity";
    public static final String DATABASE_ELEC_ID = "aisud781je";
    public static final String TABLE_ELEC = "vas91rujfe8";
    public static final String DATABASE_HEAT = "heat";
    public static final String DATABASE_HEAT_ID = "9vsa87";
    public static final String TABLE_HEAT = "e1ud8fyah";
    public static final String COLUMN_ELEC_LAMP_ONE = "column1";
    public static final String COLUMN_ELEC_LAMP_TWO = "column2";
    public static final String COLUMN_ELEC_OVEN = "column4";
    public static final String COLUMN_HEAT_OVEN = "column5";
    public static final String ROOM_ONE = "Kitchen";
    public static final String ROOM_TWO = "Bedroom";
    public static final String ELEC_UNIT = "kwh";
    public static final String COLUMN_ELEC_ROOM_ONE = "column14";
    public static final String COLUMN_HEAT_ROOM_ONE = "column13";
    public static final String COLUMN_ELEC_ROOM_TWO = "column12";
    public static final String COLUMN_HEAT_ROOM_TWO = "column15";
    public static final String MIN_HEAT_THRESHOLD = "1.0";
    public static final String MAX_HEAT_THRESHOLD = "2.0";
    public static final String SYSTEM_ONE = "HVAC";
    public static final String SUBSYSTEM_ONE = "MAU";
    public static final String TABLE_THERMAL = "21497ash1-a71n7";
    public static final String COLUMN_THERMAL_SYSTEM_ONE = "column1";
    public static final int CHART_HEIGHT = 8;
    public static final int CHART_WIDTH = 12;
    public static final String[] LAMP_ONE_COMMON_MEASURE_METADATA = new String[]{ASSET_LAMP_ONE, COLUMN_ELEC_LAMP_ONE, TABLE_ELEC, DATABASE_ELEC, "null"};
    public static final String[] LAMP_TWO_COMMON_MEASURE_METADATA = new String[]{ASSET_LAMP_TWO, COLUMN_ELEC_LAMP_TWO, TABLE_ELEC, DATABASE_ELEC, "null"};
    public static final String[] OVEN_COMMON_MEASURE_METADATA = new String[]{ASSET_OVEN, COLUMN_ELEC_OVEN, TABLE_ELEC, DATABASE_ELEC, "null"};
    public static final String[] OVEN_HEAT_MEASURE_METADATA = new String[]{ASSET_OVEN, COLUMN_HEAT_OVEN, TABLE_HEAT, DATABASE_HEAT, "null"};


    /**
     * Generate a sample credentials file with the required username and password.
     *
     * @param isComplete    A boolean indicating if a complete set of credentials is required for this test.
     * @param dashboardUser Dashboard username.
     * @param dashboardPass Dashboard password.
     * @return A file object so that it can be deleted after testing.
     * @throws IOException
     */
    public static File genSampleCredFile(boolean isComplete, String dashboardUser, String dashboardPass) throws IOException {
        File file = new File(System.getProperty("user.dir") + "/config/credentials.properties");
        // Check if the directory exists, create it if it doesn't
        if (!file.getParentFile().exists()) file.getParentFile().mkdirs();
        // Create a new file
        file.createNewFile();
        // Write the lines required
        PrintWriter writer = new PrintWriter(file);
        writer.println(DASHBOARD_USER + "=" + dashboardUser);
        // Only include if we wish to use the complete version
        if (isComplete) writer.println(DASHBOARD_PASSWORD + "=" + dashboardPass);
        writer.close();
        return file;
    }

    /**
     * Generates a sample measure map containing both rooms and assets for testing. See the genSampleAssetMeasureMap() and genSampleRoomMeasureMap() format.
     *
     * @param reqThresholds A boolean indicating if thresholds are required.
     * @return The sample complex measure map.
     */
    public static Map<String, Map<String, List<String[]>>> genSampleComplexMeasureMap(boolean reqThresholds) {
        return genSampleComplexMeasureMap(reqThresholds, false);
    }

    /**
     * Generates a sample measure map containing both rooms and assets for testing. See the genSampleAssetMeasureMap() and genSampleRoomMeasureMap() format.
     *
     * @param reqThresholds   A boolean indicating if thresholds are required.
     * @param requireFacility A boolean indicating if facility data are required.
     * @return The sample complex measure map.
     */
    public static Map<String, Map<String, List<String[]>>> genSampleComplexMeasureMap(boolean reqThresholds, boolean requireFacility) {
        Map<String, Map<String, List<String[]>>> sampleMap = new HashMap<>();
        sampleMap.putAll(genSampleAssetMeasureMap());
        sampleMap.putAll(genSampleRoomMeasureMap(reqThresholds));
        if (requireFacility) addSampleFacilityData(sampleMap, true, true, false);
        return sampleMap;
    }

    /**
     * Generates a sample asset measure map in the following format for testing:
     * { Lamp: {
     * assets: [L1, L2],
     * "Electricity Consumption": [[L1, column1, elecTableName, electricity, unit(null)],[L2, column2, elecTableName, electricity, null]],
     * },
     * Oven: {
     * assets: [O-1],
     * "Electricity Consumption": [[O-1, column4, elecTableName, electricity, null]],
     * "Heat Consumption": [[O-1, column5, heatTableName, heat, null]],
     * }
     * }
     *
     * @return The sample asset measure map.
     */
    public static Map<String, Map<String, List<String[]>>> genSampleAssetMeasureMap() {
        // Initialise empty collections for what we need to do
        Map<String, Map<String, List<String[]>>> sampleMap = new HashMap<>();
        Map<String, List<String[]>> measures = new HashMap<>();
        List<String[]> values = new ArrayList<>();
        // For the lamp assets,
        // First generate a list of their assets and append it to the "assets" key
        values.add(new String[]{ASSET_LAMP_ONE});
        values.add(new String[]{ASSET_LAMP_TWO});
        measures.put(StringHelper.ASSET_KEY, values);
        // Generate the related electricity consumption metadata and append it to its measure key
        values = new ArrayList<>(); // Clear old data
        values.add(LAMP_ONE_COMMON_MEASURE_METADATA);
        values.add(LAMP_TWO_COMMON_MEASURE_METADATA);
        measures.put(MEASURE_COMMON, values);
        // Put the measures map into the bigger map
        sampleMap.put(ASSET_TYPE_ONE, measures);
        // For the oven asset, clear old data first
        measures = new HashMap<>();
        values = new ArrayList<>();
        // Generate a list of their assets and append it to the "assets" key
        values.add(new String[]{ASSET_OVEN});
        measures.put(StringHelper.ASSET_KEY, values);
        // Generate the related electricity consumption metadata and append it to its measure key
        values = new ArrayList<>(); // Clear old data
        values.add(OVEN_COMMON_MEASURE_METADATA);
        measures.put(MEASURE_COMMON, values);
        // Generate the related heat consumption metadata and append it to its measure key
        values = new ArrayList<>(); // Clear old data
        values.add(OVEN_HEAT_MEASURE_METADATA);
        measures.put(MEASURE_HEAT, values);
        // Put the measures map into the bigger map
        sampleMap.put(ASSET_TYPE_TWO, measures);
        return sampleMap;
    }

    /**
     * Generates a sample asset measure map in the following format for testing:
     * { Rooms: {
     * Rooms: [[Kitchen], [Bedroom]],
     * threshold: [["Heat Consumption", min, max]], * Only generated if indicated
     * "Electricity Consumption": [[Kitchen, column12, elecTableName, electricity, kwh],[Bedroom, column14, elecTableName, electricity, kwh]],
     * "Heat Consumption": [[Kitchen, column13, heatTableName, heat, unit(null)], [Bedroom, column15, heatTableName, heat, unit(null)]],
     * }
     * }
     *
     * @param reqThresholds A boolean indicating if thresholds are required.
     * @return The sample asset measure map.
     */
    public static Map<String, Map<String, List<String[]>>> genSampleRoomMeasureMap(boolean reqThresholds) {
        // Initialise empty collections for what we need to do
        Map<String, Map<String, List<String[]>>> sampleMap = new HashMap<>();
        Map<String, List<String[]>> measures = new HashMap<>();
        List<String[]> values = new ArrayList<>();
        // For the rooms,
        // First generate a list of their rooms and append it to the "Rooms" key
        values.add(new String[]{ROOM_ONE});
        values.add(new String[]{ROOM_TWO});
        measures.put(StringHelper.ROOM_KEY, values);
        if (reqThresholds) {
            // Generate thresholds
            values = new ArrayList<>(); // Clear old data
            values.add(new String[]{MEASURE_HEAT, MIN_HEAT_THRESHOLD, MAX_HEAT_THRESHOLD});
            measures.put(StringHelper.THRESHOLD_KEY, values);
        }
        // Generate the related electricity consumption metadata and append it to its measure key
        values = new ArrayList<>(); // Clear old data
        values.add(new String[]{ROOM_ONE, COLUMN_ELEC_ROOM_ONE, TABLE_ELEC, DATABASE_ELEC, ELEC_UNIT});
        values.add(new String[]{ROOM_TWO, COLUMN_ELEC_ROOM_TWO, TABLE_ELEC, DATABASE_ELEC, ELEC_UNIT});
        measures.put(MEASURE_COMMON, values);
        // Rerun the same steps for the second measure for rooms
        values = new ArrayList<>();
        // Generate the related heat consumption metadata and append it to its measure key
        values.add(new String[]{ROOM_ONE, COLUMN_HEAT_ROOM_ONE, TABLE_HEAT, DATABASE_HEAT, "null"});
        values.add(new String[]{ROOM_TWO, COLUMN_HEAT_ROOM_TWO, TABLE_HEAT, DATABASE_HEAT, "null"});
        measures.put(MEASURE_HEAT, values);
        // Put the measures map into the bigger map
        sampleMap.put(StringHelper.ROOM_KEY, measures);
        return sampleMap;
    }

    /**
     * Generates a sample asset measure map in the following format for testing:
     * { systems: {
     * systems: [[HVAC], [MAU]],
     * "Electricity Consumption": [[HVAC, column12, elecTableName, electricity, kwh],[MAU, column14, elecTableName, electricity, kwh]],
     * "Thermal Consumption": [[HVAC, column1, thermalTableName, heat, kwh]],
     * }
     * }
     *
     * @return The sample system measure map.
     */
    public static Map<String, Map<String, List<String[]>>> genSampleSystemMeasureMap() {
        // Initialise empty collections for what we need to do
        Map<String, Map<String, List<String[]>>> sampleMap = new HashMap<>();
        Map<String, List<String[]>> measures = new HashMap<>();
        List<String[]> values = new ArrayList<>();
        // For the rooms,
        // First generate a list of their rooms and append it to the "Rooms" key
        values.add(new String[]{SYSTEM_ONE});
        values.add(new String[]{SUBSYSTEM_ONE});
        measures.put(StringHelper.SYSTEM_KEY, values);
        // Generate the related electricity consumption metadata and append it to its measure key
        values = new ArrayList<>(); // Clear old data
        values.add(new String[]{SYSTEM_ONE, COLUMN_ELEC_ROOM_ONE, TABLE_ELEC, DATABASE_ELEC, ELEC_UNIT});
        values.add(new String[]{SUBSYSTEM_ONE, COLUMN_ELEC_ROOM_TWO, TABLE_ELEC, DATABASE_ELEC, ELEC_UNIT});
        measures.put(MEASURE_COMMON, values);
        // Rerun the same steps for the second measure for rooms
        values = new ArrayList<>();
        // Generate the related heat consumption metadata and append it to its measure key
        values.add(new String[]{SYSTEM_ONE, COLUMN_THERMAL_SYSTEM_ONE, TABLE_THERMAL, DATABASE_HEAT, ELEC_UNIT});
        measures.put(MEASURE_THERMAL, values);
        // Put the measures map into the bigger map
        sampleMap.put(StringHelper.SYSTEM_KEY, measures);
        return sampleMap;
    }

    /**
     * An overloaded method to add sample facility data to the map depending on requirement.
     *
     * @param sampleMap  A map to append this facility info.
     * @param reqAssets  A boolean indicating if asset info is required.
     * @param reqRooms   A boolean indicating if room info is required.
     * @param reqSystems A boolean indicating if system info is required.
     * @return The sample facilities map.
     */
    public static Map<String, Map<String, List<String[]>>> addSampleFacilityData(Map<String, Map<String, List<String[]>>> sampleMap, boolean reqAssets, boolean reqRooms, boolean reqSystems) {
        return addSampleFacilityData(sampleMap, reqAssets, reqRooms, reqSystems, false);
    }

    /**
     * Adds sample facility data to the map depending on requirement
     *
     * @param sampleMap        A map to append this facility info.
     * @param reqAssets        A boolean indicating if asset info is required.
     * @param reqRooms         A boolean indicating if room info is required.
     * @param reqSystems       A boolean indicating if system info is required.
     * @param reqComplexSystem A boolean indicating if a more complex form of system is required.
     * @return The sample facilities map.
     */
    public static Map<String, Map<String, List<String[]>>> addSampleFacilityData(Map<String, Map<String, List<String[]>>> sampleMap, boolean reqAssets, boolean reqRooms, boolean reqSystems, boolean reqComplexSystem) {
        List<String> facilityOne = new ArrayList<>();
        List<String> facilityTwo = new ArrayList<>();
        // For assets
        if (reqAssets) {
            // Home facility contains lamps
            facilityOne = List.of(ASSET_LAMP_ONE, ASSET_LAMP_TWO);
            // Bakery facility contains oven
            facilityTwo = List.of(ASSET_OVEN);
        }
        // For rooms
        if (reqRooms) {
            // Home facility contains a bedroom
            List<String> home = List.of(ROOM_TWO);
            // Bakery facility contains a kitchen
            List<String> bakery = List.of(ROOM_ONE);
            if (facilityOne.size() == 0) {
                facilityOne = home;
                facilityTwo = bakery;
            } else {
                facilityOne = Stream.concat(facilityOne.stream(), home.stream())
                        .collect(Collectors.toList());
                facilityTwo = Stream.concat(facilityTwo.stream(), bakery.stream())
                        .collect(Collectors.toList());
            }
        }
        // For systems
        if (reqSystems) {
            // Home facility contains a HVAC and MAU system
            List<String> home = List.of(SYSTEM_ONE, SUBSYSTEM_ONE);
            if (facilityOne.size() == 0) {
                facilityOne = home;
                if (reqComplexSystem) facilityTwo = home;
            } else {
                facilityOne = Stream.concat(facilityOne.stream(), home.stream())
                        .collect(Collectors.toList());
                if (reqComplexSystem) facilityTwo = Stream.concat(facilityTwo.stream(), home.stream())
                        .collect(Collectors.toList());
            }
        }
        Map<String, List<String[]>> measures = new HashMap<>();
        // For the home facility
        List<String[]> values = new ArrayList<>();
        values.add(facilityOne.toArray(String[]::new));
        measures.put(FACILITY_ONE, values);
        // For the bakery facility
        values = new ArrayList<>(); // clear old data first
        values.add(facilityTwo.toArray(String[]::new));
        if (!values.isEmpty()) measures.put(FACILITY_TWO, values);
        sampleMap.put(StringHelper.FACILITY_KEY, measures);
        return sampleMap;
    }

    /**
     * Generates a valid Instance for testing from its input concept.
     *
     * @param concept The concept to be instantiated.
     * @return The instance as a string.
     */
    public static String genInstance(String concept) {
        return BASE_URI + concept + "_" + UUID.randomUUID();
    }

    /**
     * Generates a time series instance for testing.
     */
    public static String genTimeSeriesInstance() {
        return genInstance("TimeSeries");
    }

    /**
     * Generates a sample database connection map for testing.
     * Connects the two database to their dashboard generated ID, but is hardcoded for testing requirements.
     *
     * @return The sample database connection map.
     */
    public static Map<String, String> genSampleDatabaseConnectionMap() {
        // Initialise empty collections for what we need to do
        Map<String, String> sampleMap = new HashMap<>();
        sampleMap.put(DATABASE_ELEC, DATABASE_ELEC_ID);
        sampleMap.put(DATABASE_HEAT, DATABASE_HEAT_ID);
        return sampleMap;
    }

    /**
     * An overloaded method to generate the common template panel json model with no query.
     *
     * @return The partial json model in string format.
     */
    public static String genExpectedCommonTemplatePanelJson(String title, String description, String transformations, String[] metadata, int[] geometryPositions, List<String[]> itemDetails) {
        return genExpectedCommonTemplatePanelJson(title, description, transformations, metadata, geometryPositions, itemDetails, "");
    }

    /**
     * Generates the common template panel json model.
     *
     * @return The partial json model in string format.
     */
    public static String genExpectedCommonTemplatePanelJson(String title, String description, String transformations, String[] metadata, int[] geometryPositions, List<String[]> itemDetails, String query) {
        String formattedMeasure = metadata[0].toLowerCase().replaceAll("\\s", "");
        String formattedItemGroup = metadata[1].toLowerCase().replaceAll("\\s", "");
        String rawSql = query.isEmpty() ? "SELECT time AS \\\"time\\\", ${" + formattedMeasure + formattedItemGroup + ":csv} FROM \\\"" + metadata[2] + "\\\" WHERE $__timeFilter(time)"
                : query;
        StringBuilder results = new StringBuilder();
        results.append("\"id\": null,")
                .append("\"title\": \"").append(title).append("\",")
                .append("\"description\": \"").append(description).append("\",")
                .append("\"datasource\": {\"type\": \"postgres\", \"uid\": \"").append(metadata[3]).append("\"},")
                .append("\"targets\": [")
                .append("{\"datasource\":{\"type\":\"postgres\",\"uid\":\"").append(metadata[3]).append("\"}, ")
                .append("\"editorMode\":\"code\",\"format\":\"table\",\"rawQuery\":true,\"refId\":\"A\",")
                .append("\"sql\":{\"columns\": [{\"parameters\": [],\"type\":\"function\"}], ")
                .append("\"groupBy\": [{\"property\":{\"type\":\"string\"},\"type\":\"groupBy\"}],\"limit\":50},")
                .append("\"rawSql\":\"").append(rawSql).append("\"")
                .append("}],")
                .append("\"gridPos\":{\"h\":").append(geometryPositions[0]).append(",")
                .append("\"w\":").append(geometryPositions[1]).append(",")
                .append("\"x\":").append(geometryPositions[2]).append(",")
                .append("\"y\":").append(geometryPositions[3]).append("},")
                .append("\"transformations\":").append(transformations);
        return results.toString();
    }
}
