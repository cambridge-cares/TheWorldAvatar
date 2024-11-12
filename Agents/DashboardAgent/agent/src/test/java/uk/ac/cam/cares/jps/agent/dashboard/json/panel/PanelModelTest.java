package uk.ac.cam.cares.jps.agent.dashboard.json.panel;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import uk.ac.cam.cares.jps.agent.dashboard.TestUtils;
import uk.ac.cam.cares.jps.agent.dashboard.datamodel.Organisation;
import uk.ac.cam.cares.jps.agent.dashboard.json.panel.layout.LayoutTemplateTest;
import uk.ac.cam.cares.jps.agent.dashboard.utils.StringHelper;

import java.util.*;

import static org.junit.jupiter.api.Assertions.*;

public class PanelModelTest {
    private static Map<String, String> sampleDbConnectionIdMap;
    // A global counter will ensure value can be increased even in nested methods
    private static int rowNumber;

    @BeforeAll
    static void genSampleData() {
        sampleDbConnectionIdMap = TestUtils.genSampleDatabaseConnectionMap();
    }

    @Test
    void testConstruct_AssetsOnly() {
        // Prepare test setup
        Organisation organisation = TestUtils.genSampleAssetMeasures(null);
        // Construct and execute the method
        String result = new PanelModel(sampleDbConnectionIdMap, organisation).construct();
        // Test outputs
        assertEquals(genExpectedRowSyntax(sampleDbConnectionIdMap, organisation), result);
    }

    @Test
    void testConstruct_RoomsOnlyNoThresholds() {
        // Prepare test setup
        Organisation organisation = TestUtils.genSampleRoomMeasures(null, false);
        // Construct and execute the method
        String result = new PanelModel(sampleDbConnectionIdMap, organisation).construct();
        // Test outputs
        assertEquals(genExpectedRowSyntax(sampleDbConnectionIdMap, organisation), result);
    }

    @Test
    void testConstruct_RoomsOnlyThresholds() {
        // Prepare test setup
        Organisation organisation = TestUtils.genSampleRoomMeasures(null, true);
        // This test only include thresholds for one of the room measures as thresholds are not mandatory
        // Construct and execute the method
        String result = new PanelModel(sampleDbConnectionIdMap, organisation).construct();
        // Test outputs
        assertEquals(genExpectedRowSyntax(sampleDbConnectionIdMap, organisation), result);
    }

    @Test
    void testConstruct_SystemsOnly() {
        // This test only include thresholds for one of the room measures as thresholds are not mandatory
        Organisation organisation = TestUtils.genSampleSystemMeasures(null);
        // Construct and execute the method
        String result = new PanelModel(sampleDbConnectionIdMap, organisation).construct();
        // Test outputs
        assertEquals(genExpectedRowSyntax(sampleDbConnectionIdMap, organisation), result);
    }

    @Test
    void testConstruct_AssetsAndRoomsNoThresholds() {
        // Prepare test setup
        Organisation organisation = TestUtils.genSampleAssetMeasures(null);
        organisation = TestUtils.genSampleRoomMeasures(organisation, false);
        // Construct and execute the method
        String result = new PanelModel(sampleDbConnectionIdMap, organisation).construct();
        // Test outputs
        assertEquals(genExpectedRowSyntax(sampleDbConnectionIdMap, organisation), result);
    }

    @Test
    void testConstruct_AssetsAndRoomsWithThreshold() {
        // Prepare test setup
        Organisation organisation = TestUtils.genSampleAssetMeasures(null);
        organisation = TestUtils.genSampleRoomMeasures(organisation, true);
        // Construct and execute the method
        String result = new PanelModel(sampleDbConnectionIdMap, organisation).construct();
        // Test outputs
        assertEquals(genExpectedRowSyntax(sampleDbConnectionIdMap, organisation), result);
    }

    @Test
    void testConstruct_AssetsSystemsAndRoomsNoThresholds() {
        // Prepare test setup
        Organisation organisation = TestUtils.genSampleAssetMeasures(null);
        organisation = TestUtils.genSampleRoomMeasures(organisation, false);
        organisation = TestUtils.genSampleSystemMeasures(organisation);
        // Construct and execute the method
        String result = new PanelModel(sampleDbConnectionIdMap, organisation).construct();
        // Test outputs
        assertEquals(genExpectedRowSyntax(sampleDbConnectionIdMap, organisation), result);
    }

    public static String genExpectedRowSyntax(Map<String, String> databaseConnectionMap, Organisation organisation) {
        // Initialise the settings
        StringBuilder builder = new StringBuilder();
        rowNumber = 0;
        organisation.getAllItemGroups().forEach(group -> {
            if (builder.length() != 0) builder.append(",");
            // Use a clone of the int value so that the layout template test methods do not accidentally over increment the row number
            int currentRowClone = rowNumber + 1;
            switch (group) {
                case StringHelper.ROOM_KEY:
                    Queue<String> measureNames = new ArrayDeque<>(organisation.getAllMeasureNames(StringHelper.ROOM_KEY));
                    Queue<String> panelSets = LayoutTemplateTest.genExpectedRoomLayoutJson(currentRowClone, databaseConnectionMap, organisation);
                    while (!panelSets.isEmpty()) {
                        String measureName = measureNames.poll();
                        String panelSyntax = panelSets.poll();
                        String unit = organisation.getMeasure(StringHelper.ROOM_KEY, measureName).getUnit();
                        // Append a comma if this is not the first row
                        if (builder.length() != 0) builder.append(",");
                        // General row syntax for rooms
                        String title = StringHelper.addSpaceBetweenCapitalWords(measureName);
                        title = unit == null ? title : title + "[" + unit + "]";
                        builder.append("{\"id\":null, \"type\":\"row\", \"collapsed\":true,")
                                .append("\"title\": \"").append(title).append("\",")
                                .append(" \"gridPos\": {\"h\": 1,\"w\": ").append(TestUtils.ROW_WITH_TWO_CHART_WIDTH * 2)
                                // The row header should start at 0, and then increment the number after
                                .append(",\"x\": 0,\"y\": ").append(rowNumber).append("},")
                                // Row contents should start at +1 from the header
                                .append("\"panels\": [").append(panelSyntax)
                                .append("]}");
                        // Rooms will one row per measure, so it should be incremented this way
                        rowNumber++;
                    }
                    break;
                case StringHelper.SYSTEM_KEY:
                    String title = "Smart Meter";
                    panelSets = LayoutTemplateTest.genExpectedSystemLayoutJson(currentRowClone, databaseConnectionMap, organisation);
                    StringBuilder panelSyntax = new StringBuilder();
                    while (!panelSets.isEmpty()) {
                        if (panelSyntax.length() != 0) panelSyntax.append(",");
                        panelSyntax.append(panelSets.poll());
                    }
                    // General row syntax for system
                    builder.append("{\"id\":null, \"type\":\"row\", \"collapsed\":true,")
                            .append("\"title\": \"").append(title).append("\",")
                            .append(" \"gridPos\": {\"h\": 1,\"w\": ").append(TestUtils.ROW_WITH_TWO_CHART_WIDTH * 2)
                            // The row header should start on a separate position (-1) before the row contents
                            .append(",\"x\": 0,\"y\": ").append(rowNumber).append("},")
                            .append("\"panels\": [").append(panelSyntax)
                            .append("]}");
                    // All systems' measures are located in one row, and should only increment by 1
                    rowNumber++;
                    break;
                default:
                    title = StringHelper.addSpaceBetweenCapitalWords(group);
                    panelSets = LayoutTemplateTest.genExpectedAssetLayoutJson(currentRowClone, group, databaseConnectionMap, organisation);
                    panelSyntax = new StringBuilder();
                    while (!panelSets.isEmpty()) {
                        if (panelSyntax.length() != 0) panelSyntax.append(",");
                        panelSyntax.append(panelSets.poll());
                    }
                    // General row syntax for assets
                    builder.append("{\"id\":null, \"type\":\"row\", \"collapsed\":true,")
                            .append("\"title\": \"").append(title).append("\",")
                            .append(" \"gridPos\": {\"h\": 1,\"w\": ").append(TestUtils.ROW_WITH_TWO_CHART_WIDTH * 2)
                            // The row header should start on a separate position (-1) before the row contents
                            .append(",\"x\": 0,\"y\": ").append(rowNumber).append("},")
                            .append("\"panels\": [").append(panelSyntax)
                            .append("]}");
                    // All asset measures are located in one row per asset type, and should only increment by 1
                    rowNumber++;
            }
        });
        return builder.toString();
    }
}