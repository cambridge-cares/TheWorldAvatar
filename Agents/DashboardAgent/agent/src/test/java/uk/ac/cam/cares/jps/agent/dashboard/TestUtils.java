package uk.ac.cam.cares.jps.agent.dashboard;

import uk.ac.cam.cares.jps.agent.dashboard.utils.StringHelper;

import java.io.File;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.*;

public class TestUtils {
    private static final String DASHBOARD_USER = "dashboard.user";
    private static final String DASHBOARD_PASSWORD = "dashboard.pass";
    public static final String BASE_URI = "http://www.test.org/example/";
    public static final String ASSET_TYPE_ONE = "Lamp";
    public static final String ASSET_LAMP_ONE = "L1";
    public static final String ASSET_LAMP_TWO = "L2";
    public static final String ASSET_TYPE_TWO = "Oven";
    public static final String ASSET_OVEN = "O-1";
    public static final String MEASURE_COMMON = "Electricity Consumption";
    public static final String MEASURE_HEAT = "Heat Consumption";
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
    public static final String COLUMN_ELEC_ROOM_ONE = "column12";
    public static final String COLUMN_HEAT_ROOM_ONE = "column13";
    public static final String COLUMN_ELEC_ROOM_TWO = "column14";
    public static final String COLUMN_HEAT_ROOM_TWO = "column15";
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
     * @return The sample complex measure map.
     */
    public static Map<String, Map<String, List<String[]>>> genSampleComplexMeasureMap() {
        Map<String, Map<String, List<String[]>>> sampleMap = new HashMap<>();
        sampleMap.putAll(genSampleAssetMeasureMap());
        sampleMap.putAll(genSampleRoomMeasureMap());
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
     * Rooms: [Kitchen, Bedroom],
     * "Electricity Consumption": [[Kitchen, column12, elecTableName, electricity, kwh],[Bedroom, column14, elecTableName, electricity, kwh]],
     * "Heat Consumption": [[Kitchen, column13, heatTableName, heat, unit(null)], [Bedroom, column15, heatTableName, heat, unit(null)]],
     * }
     * }
     *
     * @return The sample asset measure map.
     */
    public static Map<String, Map<String, List<String[]>>> genSampleRoomMeasureMap() {
        // Initialise empty collections for what we need to do
        Map<String, Map<String, List<String[]>>> sampleMap = new HashMap<>();
        Map<String, List<String[]>> measures = new HashMap<>();
        List<String[]> values = new ArrayList<>();
        // For the rooms,
        // First generate a list of their rooms and append it to the "Rooms" key
        values.add(new String[]{ROOM_ONE});
        values.add(new String[]{ROOM_TWO});
        measures.put(StringHelper.ROOM_KEY, values);
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
}
