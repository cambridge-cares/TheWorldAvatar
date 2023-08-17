package uk.ac.cam.cares.jps.agent.dashboard;

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
    public static final String MEASURE_OVEN = "Heat Consumption";
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
     * Generates a sample asset measure map in the following format for testing:
     * { Lamp: {
     * assets: [L1, L2],
     * "Electricity Consumption": [[L1, column1, elecTableName, electricity],[L2, column2, elecTableName, electricity]],
     * },
     * Oven: {
     * assets: [O-1],
     * "Electricity Consumption": [[O-1, column4, elecTableName, electricity]],
     * "Heat Consumption": [[O-1, column5, heatTableName, heat]],
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
        measures.put("assets", values);
        // Generate the related electricity consumption metadata and append it to its measure key
        values = new ArrayList<>(); // Clear old data
        values.add(new String[]{ASSET_LAMP_ONE, COLUMN_ELEC_LAMP_ONE, TABLE_ELEC, DATABASE_ELEC});
        values.add(new String[]{ASSET_LAMP_TWO, COLUMN_ELEC_LAMP_TWO, TABLE_ELEC, DATABASE_ELEC});
        measures.put(MEASURE_COMMON, values);
        // Put the measures map into the bigger map
        sampleMap.put(ASSET_TYPE_ONE, measures);

        // For the oven asset, first clear old data first
        measures = new HashMap<>();
        values = new ArrayList<>();
        // Generate a list of their assets and append it to the "assets" key
        values.add(new String[]{ASSET_OVEN});
        measures.put("assets", values);
        // Generate the related electricity consumption metadata and append it to its measure key
        values = new ArrayList<>(); // Clear old data
        values.add(new String[]{ASSET_OVEN, COLUMN_ELEC_OVEN, TABLE_ELEC, DATABASE_ELEC});
        measures.put(MEASURE_COMMON, values);
        // Generate the related heat consumption metadata and append it to its measure key
        values = new ArrayList<>(); // Clear old data
        values.add(new String[]{ASSET_OVEN, COLUMN_HEAT_OVEN, TABLE_HEAT, DATABASE_HEAT});
        measures.put(MEASURE_OVEN, values);
        // Put the measures map into the bigger map
        sampleMap.put(ASSET_TYPE_TWO, measures);
        return sampleMap;
    }

    /**
     * Generates a valid Instance for testing from its input concept.
     *
     * @param concept The concept to be instantiated.
     * @return The instance as a string.
     */
    public static String genInstance(String concept) {return BASE_URI + concept + "_" + UUID.randomUUID();}

    /**
     * Generates a time series instance for testing.
     */
    public static String genTimeSeriesInstance() {return genInstance("TimeSeries");}

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
