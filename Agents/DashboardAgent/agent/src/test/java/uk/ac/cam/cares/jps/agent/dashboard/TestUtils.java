package uk.ac.cam.cares.jps.agent.dashboard;

import uk.ac.cam.cares.jps.agent.dashboard.datamodel.Measure;
import uk.ac.cam.cares.jps.agent.dashboard.datamodel.Organisation;
import uk.ac.cam.cares.jps.agent.dashboard.json.panel.options.DataSourceTest;
import uk.ac.cam.cares.jps.agent.dashboard.stack.PostGisClientTest;
import uk.ac.cam.cares.jps.agent.dashboard.stack.SparqlClientTest;
import uk.ac.cam.cares.jps.agent.dashboard.utils.StringHelper;

import java.io.File;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.*;

public class TestUtils {
    private static final String DASHBOARD_USER = "dashboard.user";
    private static final String DASHBOARD_PASSWORD = "dashboard.pass";
    public static final String BASE_URI = "http://www.test.org/example/";
    public static final String ORGANISATION_NAME = "CARES";
    public static final String ASSET_TYPE_ONE = "Lamp";
    public static final String ASSET_LAMP_ONE = "L1";
    public static final String MEASURE_COMMON = "Electricity Consumption";
    public static final String DATABASE_ELEC = "electricity";
    public static final String DATABASE_ELEC_ID = "aisud781je";
    public static final String TABLE_ELEC = "vas91rujfe8";
    public static final String DATABASE_HEAT = "heat";
    public static final String DATABASE_HEAT_ID = "9vsa87";
    public static final String COLUMN_ELEC_LAMP_ONE = "column1";
    public static final String ELEC_UNIT = "kwh";
    public static final String SAMPLE_DATA_IRI = genInstance("Measure");
    public static final String SAMPLE_TS_IRI = genTimeSeriesInstance();
    public static final int CHART_HEIGHT = 8;
    public static final int ROW_WITH_TWO_CHART_WIDTH = 12;
    public static final int ROW_OF_THREE_FIRST_CHART_WIDTH = 4;
    public static final int ROW_OF_THREE_DUAL_CHART_WIDTH = 10;


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
     * Generates a sample organisation with the following assets and their measures for testing:
     * Smart Sensor: [
     * column, tempTable, database, degree, Temperature
     * ]
     *
     * @param organisation An existing organisation data model if available.
     * @return The organisation data model with the sample asset measures.
     */
    public static Organisation genSampleAssetMeasures(Organisation organisation) {
        // If there are existing organisation data model, append the data to the model
        Organisation sample = organisation == null ? new Organisation(ORGANISATION_NAME) : organisation;
        sample.addFacilityItem(SparqlClientTest.SAMPLE_LAB_NAME, SparqlClientTest.SAMPLE_LAB_SMART_SENSOR_NAME, SparqlClientTest.SAMPLE_LAB_SMART_SENSOR_TYPE,
                SparqlClientTest.TEMPERATURE, SparqlClientTest.TEMPERATURE_UNIT, SparqlClientTest.SMART_SENSOR_ONE_TEMPERATURE_MEASURE, SparqlClientTest.SMART_SENSOR_ONE_TEMPERATURE_TIME_SERIES_IRI);
        sample.addTimeSeries(SparqlClientTest.SAMPLE_LAB_SMART_SENSOR_TYPE, SparqlClientTest.TEMPERATURE, SparqlClientTest.SAMPLE_LAB_SMART_SENSOR_NAME,
                PostGisClientTest.SAMPLE_SMART_SENSOR_ONE_TEMPERATURE_COLUMN, PostGisClientTest.SAMPLE_TEMPERATURE_TABLE, PostGisClientTest.TEMPERATURE_SQL_DATABASE);
        return sample;
    }

    /**
     * Generates a sample organisation with the following assets and their measures for testing:
     * Kitchen [Room]: [
     * column12, elecTableName, electricity, kwh, Electricity Consumption
     * column13, heatTableName, heat, unit(null), Heat Consumption
     * ]
     * Bedroom [Room]: [
     * column14, elecTableName, electricity, kwh, Electricity Consumption
     * column15, heatTableName, heat, unit(null), Heat Consumption
     * ]
     * threshold: "Heat Consumption", min, max // Only generated if indicated
     *
     * @param organisation  An existing organisation data model if available.
     * @param reqThresholds A boolean indicating if thresholds are required.
     * @return The organisation data model with the sample room measures.
     */
    public static Organisation genSampleRoomMeasures(Organisation organisation, boolean reqThresholds) {
        // If there are existing organisation data model, append the data to the model
        Organisation sample = organisation == null ? new Organisation(ORGANISATION_NAME) : organisation;
        sample.addFacilityItem(SparqlClientTest.SAMPLE_OFFICE_NAME, SparqlClientTest.SAMPLE_OFFICE_DIRECTOR_ROOM_NAME, StringHelper.ROOM_KEY,
                SparqlClientTest.TEMPERATURE, SparqlClientTest.TEMPERATURE_UNIT, SparqlClientTest.SAMPLE_OFFICE_DIRECTOR_ROOM_TEMPERATURE_MEASURE, SparqlClientTest.SAMPLE_OFFICE_DIRECTOR_ROOM_TEMPERATURE_TIME_SERIES_IRI);
        sample.addTimeSeries(StringHelper.ROOM_KEY, SparqlClientTest.TEMPERATURE, SparqlClientTest.SAMPLE_OFFICE_DIRECTOR_ROOM_NAME,
                PostGisClientTest.SAMPLE_OFFICE_DIRECTOR_ROOM_TEMPERATURE_COLUMN, PostGisClientTest.SAMPLE_TEMPERATURE_TABLE, PostGisClientTest.TEMPERATURE_SQL_DATABASE);
        sample.addFacilityItem(SparqlClientTest.SAMPLE_OFFICE_NAME, SparqlClientTest.SAMPLE_OFFICE_STAFF_ROOM_NAME, StringHelper.ROOM_KEY,
                SparqlClientTest.TEMPERATURE, SparqlClientTest.TEMPERATURE_UNIT, SparqlClientTest.SAMPLE_OFFICE_STAFF_ROOM_TEMPERATURE_MEASURE, SparqlClientTest.SAMPLE_OFFICE_STAFF_ROOM_TEMPERATURE_TIME_SERIES_IRI);
        sample.addTimeSeries(StringHelper.ROOM_KEY, SparqlClientTest.TEMPERATURE, SparqlClientTest.SAMPLE_OFFICE_STAFF_ROOM_NAME,
                PostGisClientTest.SAMPLE_OFFICE_STAFF_ROOM_TEMPERATURE_COLUMN, PostGisClientTest.SAMPLE_TEMPERATURE_TABLE, PostGisClientTest.TEMPERATURE_SQL_DATABASE);
        sample.addFacilityItem(SparqlClientTest.SAMPLE_OFFICE_NAME, SparqlClientTest.SAMPLE_OFFICE_STAFF_ROOM_NAME, StringHelper.ROOM_KEY,
                SparqlClientTest.RELATIVE_HUMIDITY, null, SparqlClientTest.SAMPLE_OFFICE_STAFF_ROOM_REL_HUMIDITY_MEASURE, SparqlClientTest.SAMPLE_OFFICE_STAFF_ROOM_REL_HUMIDITY_TIME_SERIES_IRI);
        sample.addTimeSeries(StringHelper.ROOM_KEY, SparqlClientTest.RELATIVE_HUMIDITY, SparqlClientTest.SAMPLE_OFFICE_STAFF_ROOM_NAME,
                PostGisClientTest.SAMPLE_OFFICE_STAFF_ROOM_HUMIDITY_COLUMN, PostGisClientTest.SAMPLE_ROOM_HUMIDITY_TABLE, PostGisClientTest.ALL_SQL_DATABASE);
        if (reqThresholds) {
            sample.addThresholds(SparqlClientTest.SAMPLE_OFFICE_NAME, StringHelper.ROOM_KEY, SparqlClientTest.RELATIVE_HUMIDITY, String.valueOf(SparqlClientTest.RELATIVE_HUMIDITY_MIN_THRESHOLD), String.valueOf(SparqlClientTest.RELATIVE_HUMIDITY_MAX_THRESHOLD));
            sample.addThresholds(SparqlClientTest.SAMPLE_OFFICE_NAME, StringHelper.ROOM_KEY, SparqlClientTest.TEMPERATURE, String.valueOf(SparqlClientTest.TEMPERATURE_MIN_THRESHOLD), String.valueOf(SparqlClientTest.TEMPERATURE_MAX_THRESHOLD));
        }
        return sample;
    }

    /**
     * Generates a sample organisation with the following assets and their measures for testing:
     * HVAC [System]: [
     * column, elecTableName, electricity, kwh, Electricity Consumption
     * ]
     * BTU unit [System]: [
     * column, elecTableName, electricity, kwh, Electricity Consumption
     * ]
     *
     * @param organisation An existing organisation data model if available.
     * @return The organisation data model with the sample system measures.
     */
    public static Organisation genSampleSystemMeasures(Organisation organisation) {
        // If there are existing organisation data model, append the data to the model
        Organisation sample = organisation == null ? new Organisation(ORGANISATION_NAME) : organisation;
        sample.addFacilityItem(SparqlClientTest.SAMPLE_OFFICE_NAME, SparqlClientTest.SAMPLE_OFFICE_SYSTEM_NAME, StringHelper.SYSTEM_KEY,
                SparqlClientTest.ELECTRICITY_CONSUMPTION, SparqlClientTest.ELECTRICITY_CONSUMPTION_UNIT, SparqlClientTest.SAMPLE_OFFICE_SYSTEM_ELECTRICITY_CONSUMPTION_MEASURE, SparqlClientTest.SAMPLE_OFFICE_SYSTEM_ELECTRICITY_CONSUMPTION_TIME_SERIES_IRI);
        sample.addTimeSeries(StringHelper.SYSTEM_KEY, SparqlClientTest.ELECTRICITY_CONSUMPTION, SparqlClientTest.SAMPLE_OFFICE_SYSTEM_NAME,
                PostGisClientTest.SAMPLE_OFFICE_SYSTEM_ELEC_CONSUMPTION_COLUMN, PostGisClientTest.SAMPLE_SYSTEM_ELEC_CONSUMPTION_TABLE, PostGisClientTest.ALL_SQL_DATABASE);
        sample.addFacilityItem(SparqlClientTest.SAMPLE_OFFICE_NAME, SparqlClientTest.SAMPLE_OFFICE_SUB_SYSTEM_NAME, StringHelper.SYSTEM_KEY,
                SparqlClientTest.ELECTRICITY_CONSUMPTION, SparqlClientTest.ELECTRICITY_CONSUMPTION_UNIT, SparqlClientTest.SAMPLE_OFFICE_SUB_SYSTEM_ELECTRICITY_CONSUMPTION_MEASURE, SparqlClientTest.SAMPLE_OFFICE_SUB_SYSTEM_ELECTRICITY_CONSUMPTION_TIME_SERIES_IRI);
        sample.addTimeSeries(StringHelper.SYSTEM_KEY, SparqlClientTest.ELECTRICITY_CONSUMPTION, SparqlClientTest.SAMPLE_OFFICE_SUB_SYSTEM_NAME,
                PostGisClientTest.SAMPLE_OFFICE_SUB_SYSTEM_ELEC_CONSUMPTION_COLUMN, PostGisClientTest.SAMPLE_SYSTEM_ELEC_CONSUMPTION_TABLE, PostGisClientTest.ALL_SQL_DATABASE);
        return sample;
    }

    /**
     * Generates a sample measure of Electricity consumption with no unit with the following assets and their measures for testing:
     * HVAC [System]: [
     * column12, elecTableName, electricity, kwh, Electricity Consumption
     * column1, thermalTableName, heat, kwh, Heat Consumption
     * ]
     * MAU [System]: column14, elecTableName, electricity, kwh, Electricity Consumption
     *
     * @return The measure data model with some sample values.
     */
    public static Measure genSampleMeasure(boolean reqUnit) {
        String unit = reqUnit ? ELEC_UNIT : null;
        Measure sample = new Measure(MEASURE_COMMON, unit);
        sample.addTimeSeriesIris(ASSET_LAMP_ONE, SAMPLE_DATA_IRI, SAMPLE_TS_IRI);
        sample.addTimeSeriesMetadata(ASSET_LAMP_ONE, COLUMN_ELEC_LAMP_ONE, TABLE_ELEC, DATABASE_ELEC);
        return sample;
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
     * Generates the common default grafana panel json model.
     *
     * @return The partial json model in string format.
     */
    public static String genExpectedCommonDefaultGrafanaPanelJson(String title, String description, String panelType, String transformations,
                                                                  String databaseId, String itemGroup, Measure measure, int[] geometryPositions, String query) {
        String formattedMeasure = measure.getName().toLowerCase().replaceAll("\\s", "");
        String formattedItemGroup = itemGroup.toLowerCase().replaceAll("\\s", "");
        String rawSql = query.isEmpty() ? "SELECT time AS \\\"time\\\", ${" + formattedMeasure + formattedItemGroup + ":csv} FROM \\\"" + measure.getTimeSeriesTable() + "\\\" WHERE $__timeFilter(time)"
                : query;
        StringBuilder results = new StringBuilder();
        results.append(genExpectedCommonTemplatePanelJson(title, description, panelType, databaseId, geometryPositions))
                .append("\"targets\": [{")
                .append(DataSourceTest.genExpectedDataSource(databaseId))
                .append("\"editorMode\":\"code\",\"format\":\"table\",\"rawQuery\":true,\"refId\":\"A\",")
                .append("\"sql\":{\"columns\": [{\"parameters\": [],\"type\":\"function\"}], ")
                .append("\"groupBy\": [{\"property\":{\"type\":\"string\"},\"type\":\"groupBy\"}],\"limit\":50},")
                .append("\"rawSql\":\"").append(rawSql).append("\"")
                .append("}],")
                .append("\"transformations\":").append(transformations).append(",");
        return results.toString();
    }

    /**
     * Generates the common template panel json model with the default data source.
     *
     * @return The partial json model in string format.
     */
    public static String genExpectedCommonTemplatePanelJson(String title, String description, String panelType, int[] geometryPositions) {
        StringBuilder results = new StringBuilder();
        results.append("\"id\": null,")
                .append("\"title\": \"").append(title).append("\",")
                .append("\"description\": \"").append(description).append("\",")
                .append("\"type\": \"").append(panelType).append("\",")
                .append(DataSourceTest.genExpectedDataSource())
                .append("\"gridPos\":{\"h\":").append(geometryPositions[0]).append(",")
                .append("\"w\":").append(geometryPositions[1]).append(",")
                .append("\"x\":").append(geometryPositions[2]).append(",")
                .append("\"y\":").append(geometryPositions[3]).append("},");
        return results.toString();
    }

    /**
     * Generates the common template panel json model.
     *
     * @return The partial json model in string format.
     */
    public static String genExpectedCommonTemplatePanelJson(String title, String description, String panelType, String databaseConnectionId, int[] geometryPositions) {
        StringBuilder results = new StringBuilder();
        results.append("\"id\": null,")
                .append("\"title\": \"").append(title).append("\",")
                .append("\"description\": \"").append(description).append("\",")
                .append("\"type\": \"").append(panelType).append("\",")
                .append(DataSourceTest.genExpectedDataSource(databaseConnectionId))
                .append("\"gridPos\":{\"h\":").append(geometryPositions[0]).append(",")
                .append("\"w\":").append(geometryPositions[1]).append(",")
                .append("\"x\":").append(geometryPositions[2]).append(",")
                .append("\"y\":").append(geometryPositions[3]).append("},");
        return results.toString();
    }
}
