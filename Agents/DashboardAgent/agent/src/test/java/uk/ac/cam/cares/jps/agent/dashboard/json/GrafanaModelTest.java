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

    private static Map<String, String> sampleDbConnectionIdMap;
    private static Map<String, Map<String, List<String[]>>> sampleAssets;
    private static Map<String, Map<String, List<String[]>>> sampleRooms;
    private static Map<String, Map<String, List<String[]>>> sampleSystems;

    @BeforeAll
    static void genSampleData() {
        sampleAssets = TestUtils.genSampleAssetMeasureMap();
        sampleAssets = TestUtils.addSampleFacilityData(sampleAssets, true, false, false);
        sampleDbConnectionIdMap = TestUtils.genSampleDatabaseConnectionMap();
        sampleRooms = TestUtils.genSampleRoomMeasureMap(true);
        sampleRooms = TestUtils.addSampleFacilityData(sampleRooms, false, true, false);
        sampleSystems = TestUtils.genSampleSystemMeasureMap();
        sampleSystems = TestUtils.addSampleFacilityData(sampleSystems, false, false, true);
    }

    @Test
    void testConstruct_EmptyTimeSeries() {
        GrafanaModel sampleModel = new GrafanaModel(SAMPLE_TITLE, sampleDbConnectionIdMap, new HashMap<>());
        assertEquals(genExpectedResults(SAMPLE_TITLE, sampleDbConnectionIdMap, new HashMap<>()), sampleModel.construct());
    }

    @Test
    void testConstruct_OnlyAssets() {
        GrafanaModel sampleModel = new GrafanaModel(SAMPLE_TITLE, sampleDbConnectionIdMap, sampleAssets);
        Map<String, Map<String, List<String[]>>> sampleMap = TestUtils.genSampleAssetMeasureMap();
        sampleMap = TestUtils.addSampleFacilityData(sampleMap, true, false, false);
        assertEquals(genExpectedResults(SAMPLE_TITLE, sampleDbConnectionIdMap, sampleMap), sampleModel.construct());
    }

    @Test
    void testConstruct_OnlyRooms() {
        GrafanaModel sampleModel = new GrafanaModel(SAMPLE_TITLE, sampleDbConnectionIdMap, sampleRooms);
        Map<String, Map<String, List<String[]>>> sampleMap = TestUtils.genSampleRoomMeasureMap(true);
        sampleMap = TestUtils.addSampleFacilityData(sampleMap, false, true, false);
        assertEquals(genExpectedResults(SAMPLE_TITLE, sampleDbConnectionIdMap, sampleMap), sampleModel.construct());
    }

    @Test
    void testConstruct_OnlySystems() {
        GrafanaModel sampleModel = new GrafanaModel(SAMPLE_TITLE, sampleDbConnectionIdMap, sampleSystems);
        Map<String, Map<String, List<String[]>>> sampleMap = TestUtils.genSampleSystemMeasureMap();
        sampleMap = TestUtils.addSampleFacilityData(sampleMap, false, false, true);
        assertEquals(genExpectedResults(SAMPLE_TITLE, sampleDbConnectionIdMap, sampleMap), sampleModel.construct());
    }

    @Test
    void testConstruct_RoomsAndAssets() {
        GrafanaModel sampleModel = new GrafanaModel(SAMPLE_TITLE, sampleDbConnectionIdMap, TestUtils.genSampleComplexMeasureMap(true, true));
        assertEquals(genExpectedResults(SAMPLE_TITLE, sampleDbConnectionIdMap, TestUtils.genSampleComplexMeasureMap(true, true)), sampleModel.construct());
    }

    @Test
    void testConstruct_RoomsAndAssetsNoThresholds() {
        GrafanaModel sampleModel = new GrafanaModel(SAMPLE_TITLE, sampleDbConnectionIdMap, TestUtils.genSampleComplexMeasureMap(false, true));
        assertEquals(genExpectedResults(SAMPLE_TITLE, sampleDbConnectionIdMap, TestUtils.genSampleComplexMeasureMap(false, true)), sampleModel.construct());
    }

    @Test
    void testSetExistingIds() {
        // Set up
        int sampleId = 5;
        String sampleUid = "h18jrna";
        GrafanaModel sampleModel = new GrafanaModel(SAMPLE_TITLE, sampleDbConnectionIdMap, TestUtils.genSampleComplexMeasureMap(false, true));
        // Execute method
        sampleModel.setExistingIds(sampleId, sampleUid);
        // Verify results
        assertEquals(genExpectedResults(String.valueOf(sampleId), sampleUid, SAMPLE_TITLE, "20s", "Initialised dashboard", sampleDbConnectionIdMap, TestUtils.genSampleComplexMeasureMap(false, true), true), sampleModel.construct());
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