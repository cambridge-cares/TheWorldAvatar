package uk.ac.cam.cares.jps.agent.dashboard.json;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import uk.ac.cam.cares.jps.agent.dashboard.TestUtils;
import uk.ac.cam.cares.jps.agent.dashboard.datamodel.Organisation;
import uk.ac.cam.cares.jps.agent.dashboard.json.panel.PanelModelTest;
import uk.ac.cam.cares.jps.agent.dashboard.json.templating.TemplatingModelTest;

import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;

class GrafanaModelTest {
    private static final String SAMPLE_TITLE = "Overview";

    private static Map<String, String> sampleDbConnectionIdMap;

    @BeforeAll
    static void genSampleData() {
        sampleDbConnectionIdMap = TestUtils.genSampleDatabaseConnectionMap();
    }

    @Test
    void testConstruct_OnlyAssets() {
        Organisation organisation = TestUtils.genSampleAssetMeasures(null);
        GrafanaModel sampleModel = new GrafanaModel(SAMPLE_TITLE, sampleDbConnectionIdMap, organisation);
        assertEquals(genExpectedResults(SAMPLE_TITLE, sampleDbConnectionIdMap, organisation), sampleModel.construct());
    }

    @Test
    void testConstruct_OnlyRooms() {
        Organisation organisation = TestUtils.genSampleRoomMeasures(null, false);
        GrafanaModel sampleModel = new GrafanaModel(SAMPLE_TITLE, sampleDbConnectionIdMap, organisation);
        assertEquals(genExpectedResults(SAMPLE_TITLE, sampleDbConnectionIdMap, organisation), sampleModel.construct());
    }

    @Test
    void testConstruct_OnlySystems() {
        Organisation organisation = TestUtils.genSampleSystemMeasures(null);
        GrafanaModel sampleModel = new GrafanaModel(SAMPLE_TITLE, sampleDbConnectionIdMap, organisation);
        assertEquals(genExpectedResults(SAMPLE_TITLE, sampleDbConnectionIdMap, organisation), sampleModel.construct());
    }

    @Test
    void testConstruct_RoomsAndAssets() {
        Organisation organisation = TestUtils.genSampleAssetMeasures(null);
        organisation = TestUtils.genSampleRoomMeasures(organisation, false);
        GrafanaModel sampleModel = new GrafanaModel(SAMPLE_TITLE, sampleDbConnectionIdMap, organisation);
        assertEquals(genExpectedResults(SAMPLE_TITLE, sampleDbConnectionIdMap, organisation), sampleModel.construct());
    }

    @Test
    void testConstruct_RoomsAndAssetsNoThresholds() {
        Organisation organisation = TestUtils.genSampleAssetMeasures(null);
        organisation = TestUtils.genSampleRoomMeasures(organisation, false);
        GrafanaModel sampleModel = new GrafanaModel(SAMPLE_TITLE, sampleDbConnectionIdMap, organisation);
        assertEquals(genExpectedResults(SAMPLE_TITLE, sampleDbConnectionIdMap, organisation), sampleModel.construct());
    }

    @Test
    void testConstruct_RoomsSystemsAndAssetsNoThresholds() {
        Organisation organisation = TestUtils.genSampleAssetMeasures(null);
        organisation = TestUtils.genSampleRoomMeasures(organisation, false);
        organisation = TestUtils.genSampleSystemMeasures(organisation);
        GrafanaModel sampleModel = new GrafanaModel(SAMPLE_TITLE, sampleDbConnectionIdMap, organisation);
        assertEquals(genExpectedResults(SAMPLE_TITLE, sampleDbConnectionIdMap, organisation), sampleModel.construct());
    }

    @Test
    void testSetExistingIds() {
        // Set up
        int sampleId = 5;
        String sampleUid = "h18jrna";
        Organisation organisation = TestUtils.genSampleAssetMeasures(null);
        GrafanaModel sampleModel = new GrafanaModel(SAMPLE_TITLE, sampleDbConnectionIdMap, organisation);
        // Execute method
        sampleModel.setExistingIds(sampleId, sampleUid);
        // Verify results
        assertEquals(genExpectedResults(String.valueOf(sampleId), sampleUid, SAMPLE_TITLE, "20s", "Initialised dashboard", sampleDbConnectionIdMap, organisation, true), sampleModel.construct());
    }


    private static String genExpectedResults(String title, Map<String, String> databaseConnectionMap, Organisation organisation) {
        return genExpectedResults(null, "null", title, "20s", "Initialised dashboard", databaseConnectionMap, organisation, false);
    }

    private static String genExpectedResults(String dashboardID, String dashboardUID, String title, String refreshRate, String comment, Map<String, String> databaseConnectionMap, Organisation organisation, boolean overwrite) {
        dashboardUID = dashboardUID.equals("null") ? dashboardUID : "\"" + dashboardUID + "\"";
        StringBuilder builder = new StringBuilder();
        builder.append("{\"dashboard\": {")
                // generate new id and uid using null
                .append("\"id\":").append(dashboardID).append(",")
                .append("\"uid\":").append(dashboardUID).append(",")
                // The dashboard title
                .append("\"title\": \"").append(title).append("\",")
                // Templating
                .append("\"templating\": ").append(TemplatingModelTest.genExpectedJsonSyntax(databaseConnectionMap, organisation)).append(",")
                // Panel
                .append("\"panels\": [").append(PanelModelTest.genExpectedRowSyntax(databaseConnectionMap, organisation)).append("],")
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