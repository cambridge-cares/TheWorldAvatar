package uk.ac.cam.cares.jps.agent.dashboard.json;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import uk.ac.cam.cares.jps.agent.dashboard.TestUtils;
import uk.ac.cam.cares.jps.agent.dashboard.json.panel.PanelModelTest;
import uk.ac.cam.cares.jps.agent.dashboard.json.templating.TemplatingModelTest;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;

class GrafanaModelTest {
    private static final String SAMPLE_TITLE = "Overview";

    private static Map<String, String> SAMPLE_DB_CONNECTION_ID_MAP;
    private static Map<String, Map<String, List<String[]>>> SAMPLE_ASSETS;
    private static Map<String, Map<String, List<String[]>>> SAMPLE_ROOMS;
    private static Map<String, Map<String, List<String[]>>> SAMPLE_SYSTEMS;

    @BeforeAll
    static void genSampleData() {
        SAMPLE_ASSETS = TestUtils.genSampleAssetMeasureMap();
        SAMPLE_ASSETS = TestUtils.addSampleFacilityData(SAMPLE_ASSETS, true, false, false);
        SAMPLE_DB_CONNECTION_ID_MAP = TestUtils.genSampleDatabaseConnectionMap();
        SAMPLE_ROOMS = TestUtils.genSampleRoomMeasureMap(true);
        SAMPLE_ROOMS = TestUtils.addSampleFacilityData(SAMPLE_ROOMS, false, true, false);
        SAMPLE_SYSTEMS = TestUtils.genSampleSystemMeasureMap();
        SAMPLE_SYSTEMS = TestUtils.addSampleFacilityData(SAMPLE_SYSTEMS, false, false, true);
    }

    @Test
    void testConstruct_EmptyTimeSeries() {
        GrafanaModel sampleModel = new GrafanaModel(SAMPLE_TITLE, SAMPLE_DB_CONNECTION_ID_MAP, new HashMap<>());
        assertEquals(genExpectedResults(SAMPLE_TITLE, SAMPLE_DB_CONNECTION_ID_MAP, new HashMap<>()), sampleModel.construct());
    }

    @Test
    void testConstruct_OnlyAssets() {
        GrafanaModel sampleModel = new GrafanaModel(SAMPLE_TITLE, SAMPLE_DB_CONNECTION_ID_MAP, SAMPLE_ASSETS);
        Map<String, Map<String, List<String[]>>> sampleMap = TestUtils.genSampleAssetMeasureMap();
        sampleMap = TestUtils.addSampleFacilityData(sampleMap, true, false, false);
        assertEquals(genExpectedResults(SAMPLE_TITLE, SAMPLE_DB_CONNECTION_ID_MAP, sampleMap), sampleModel.construct());
    }

    @Test
    void testConstruct_OnlyRooms() {
        GrafanaModel sampleModel = new GrafanaModel(SAMPLE_TITLE, SAMPLE_DB_CONNECTION_ID_MAP, SAMPLE_ROOMS);
        Map<String, Map<String, List<String[]>>> sampleMap = TestUtils.genSampleRoomMeasureMap(true);
        sampleMap = TestUtils.addSampleFacilityData(sampleMap, false, true, false);
        assertEquals(genExpectedResults(SAMPLE_TITLE, SAMPLE_DB_CONNECTION_ID_MAP, sampleMap), sampleModel.construct());
    }

    @Test
    void testConstruct_OnlySystems() {
        GrafanaModel sampleModel = new GrafanaModel(SAMPLE_TITLE, SAMPLE_DB_CONNECTION_ID_MAP, SAMPLE_SYSTEMS);
        Map<String, Map<String, List<String[]>>> sampleMap = TestUtils.genSampleSystemMeasureMap();
        sampleMap = TestUtils.addSampleFacilityData(sampleMap, false, false, true);
        assertEquals(genExpectedResults(SAMPLE_TITLE, SAMPLE_DB_CONNECTION_ID_MAP, sampleMap), sampleModel.construct());
    }

    @Test
    void testConstruct_RoomsAndAssets() {
        GrafanaModel sampleModel = new GrafanaModel(SAMPLE_TITLE, SAMPLE_DB_CONNECTION_ID_MAP, TestUtils.genSampleComplexMeasureMap(true, true));
        assertEquals(genExpectedResults(SAMPLE_TITLE, SAMPLE_DB_CONNECTION_ID_MAP, TestUtils.genSampleComplexMeasureMap(true, true)), sampleModel.construct());
    }

    @Test
    void testConstruct_RoomsAndAssetsNoThresholds() {
        GrafanaModel sampleModel = new GrafanaModel(SAMPLE_TITLE, SAMPLE_DB_CONNECTION_ID_MAP, TestUtils.genSampleComplexMeasureMap(false, true));
        assertEquals(genExpectedResults(SAMPLE_TITLE, SAMPLE_DB_CONNECTION_ID_MAP, TestUtils.genSampleComplexMeasureMap(false, true)), sampleModel.construct());
    }

    @Test
    void testSetExistingIds() {
        // Set up
        int sampleId = 5;
        String sampleUid = "h18jrna";
        GrafanaModel sampleModel = new GrafanaModel(SAMPLE_TITLE, SAMPLE_DB_CONNECTION_ID_MAP, TestUtils.genSampleComplexMeasureMap(false, true));
        // Execute method
        sampleModel.setExistingIds(sampleId, sampleUid);
        // Verify results
        assertEquals(genExpectedResults(String.valueOf(sampleId), sampleUid, SAMPLE_TITLE, "20s", "Initialised dashboard", SAMPLE_DB_CONNECTION_ID_MAP, TestUtils.genSampleComplexMeasureMap(false, true), true), sampleModel.construct());
    }


    private static String genExpectedResults(String title, Map<String, String> databaseConnectionMap, Map<String, Map<String, List<String[]>>> timeSeries) {
        return genExpectedResults(null, "null", title, "20s", "Initialised dashboard", databaseConnectionMap, timeSeries, false);
    }

    private static String genExpectedResults(String dashboardID, String dashboardUID, String title, String refreshRate, String comment, Map<String, String> databaseConnectionMap, Map<String, Map<String, List<String[]>>> timeSeries, boolean overwrite) {
        dashboardUID = dashboardUID.equals("null") ? dashboardUID : "\"" + dashboardUID + "\"";
        StringBuilder builder = new StringBuilder();
        builder.append("{\"dashboard\": {")
                // generate new id and uid using null
                .append("\"id\":").append(dashboardID).append(",")
                .append("\"uid\":").append(dashboardUID).append(",")
                // The dashboard title
                .append("\"title\": \"").append(title).append("\",")
                // Templating
                .append("\"templating\": ").append(TemplatingModelTest.genExpectedJsonSyntax(databaseConnectionMap, timeSeries)).append(",")
                // Panel
                .append("\"panels\": [").append(PanelModelTest.genExpectedRowSyntax(databaseConnectionMap, timeSeries)).append("],")
                // Disable any editing by non-admin users
                .append("\"editable\": false,")
                .append("\"timezone\": \"browser\",")
                // Default view - time frame
                .append("\"time\": {\"from\": \"now-3h\", \"to\": \"now\"},")
                .append("\"refresh\": \"").append(refreshRate).append("\"")
                .append("},")
                // Comments for each update/ version
                .append("\"message\": \"").append(comment).append("\",")
                .append("\"overwrite\": ").append(overwrite).append("}");
        return builder.toString();
    }
}