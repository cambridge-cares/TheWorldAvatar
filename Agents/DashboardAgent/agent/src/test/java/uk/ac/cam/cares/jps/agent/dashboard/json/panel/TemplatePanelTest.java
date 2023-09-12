package uk.ac.cam.cares.jps.agent.dashboard.json.panel;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

class TemplatePanelTest {
    private static final List<String[]> SAMPLE_METADATA = new ArrayList<>();
    private static final String SAMPLE_TITLE = "Panel 1";
    private static final String SAMPLE_DESCRIPTION = "This is a panel";
    private static final String SAMPLE_MEASURE = "consumption";
    private static final String SAMPLE_ITEM_GROUP = "fridge";
    private static final String SAMPLE_DATABASE_ID = "basb8saf7as6";
    private static final String SAMPLE_FIRST_ASSET_NAME = "asset one";
    private static final String SAMPLE_FIRST_ASSET_COL_NAME = "column4";
    private static final String SAMPLE_ASSET_TABLE_NAME = "table1";
    private static final String SAMPLE_SEC_ASSET_NAME = "asset two";
    private static final String SAMPLE_SEC_ASSET_COL_NAME = "column7";
    private static final int SAMPLE_PANEL_HEIGHT = 8;
    private static final int SAMPLE_PANEL_WIDTH = 12;
    private static final int SAMPLE_PANEL_X_POSITION = 1;
    private static final int SAMPLE_PANEL_Y_POSITION = 0;


    @BeforeAll
    static void setup() {
        SAMPLE_METADATA.add(new String[]{SAMPLE_FIRST_ASSET_NAME, SAMPLE_FIRST_ASSET_COL_NAME, SAMPLE_ASSET_TABLE_NAME});
        SAMPLE_METADATA.add(new String[]{SAMPLE_SEC_ASSET_NAME, SAMPLE_SEC_ASSET_COL_NAME, SAMPLE_ASSET_TABLE_NAME});
    }

    @Test
    void testSetTitle() {
        // Construct the object
        TemplatePanel panel = new TemplatePanel(SAMPLE_MEASURE, SAMPLE_ITEM_GROUP, SAMPLE_DATABASE_ID, SAMPLE_METADATA);
        panel.setDescription(SAMPLE_DESCRIPTION);
        // Execute method
        panel.setTitle(SAMPLE_TITLE);
        // Verify if it is included
        StringBuilder result = panel.genCommonJson(1, 1, 1, 1);
        assertTrue(result.toString().contains(SAMPLE_TITLE));
    }

    @Test
    void testSetDescription() {
        // Construct the object
        TemplatePanel panel = new TemplatePanel(SAMPLE_MEASURE, SAMPLE_ITEM_GROUP, SAMPLE_DATABASE_ID, SAMPLE_METADATA);
        panel.setTitle(SAMPLE_TITLE);
        // Execute method
        panel.setDescription(SAMPLE_DESCRIPTION);
        // Verify if it is included
        StringBuilder result = panel.genCommonJson(1, 1, 1, 1);
        assertTrue(result.toString().contains(SAMPLE_DESCRIPTION));
    }

    @Test
    void testSetQuery() {
        StringBuilder sampleQuery = new StringBuilder().append("sample");
        // Construct the object
        TemplatePanel panel = new TemplatePanel(SAMPLE_MEASURE, SAMPLE_ITEM_GROUP, SAMPLE_DATABASE_ID, SAMPLE_METADATA);
        panel.setTitle(SAMPLE_TITLE);
        panel.setDescription(SAMPLE_DESCRIPTION);
        // Execute method
        panel.setQuery(sampleQuery);
        // Verify if it is included
        StringBuilder result = panel.genCommonJson(1, 1, 1, 1);
        assertTrue(result.toString().contains(sampleQuery.toString()));
    }

    @Test
    void testGenCommonJsonReplaceQuery() {
        // Generate expected inputs
        String[] expectedConfigItems = new String[]{SAMPLE_MEASURE, SAMPLE_ITEM_GROUP, SAMPLE_ASSET_TABLE_NAME, SAMPLE_DATABASE_ID};
        int[] expectedGeometryPosition = new int[]{SAMPLE_PANEL_HEIGHT, SAMPLE_PANEL_WIDTH, SAMPLE_PANEL_X_POSITION, SAMPLE_PANEL_Y_POSITION};
        String sampleQuery = "sample";
        StringBuilder sampleQueryBuilder = new StringBuilder().append(sampleQuery);
        // Construct the object
        TemplatePanel panel = new TemplatePanel(SAMPLE_MEASURE, SAMPLE_ITEM_GROUP, SAMPLE_DATABASE_ID, SAMPLE_METADATA);
        panel.setTitle(SAMPLE_TITLE);
        panel.setDescription(SAMPLE_DESCRIPTION);
        panel.setQuery(sampleQueryBuilder);
        // Execute method
        StringBuilder result = panel.genCommonJson(SAMPLE_PANEL_HEIGHT, SAMPLE_PANEL_WIDTH, SAMPLE_PANEL_X_POSITION, SAMPLE_PANEL_Y_POSITION);
        // Verify results
        assertEquals(genExpectedCommonJsonBase(SAMPLE_TITLE, SAMPLE_DESCRIPTION, expectedConfigItems, expectedGeometryPosition, SAMPLE_METADATA, sampleQuery), result.toString());
    }

    @Test
    void testGenCommonJson() {
        // Generate expected inputs
        String[] expectedConfigItems = new String[]{SAMPLE_MEASURE, SAMPLE_ITEM_GROUP, SAMPLE_ASSET_TABLE_NAME, SAMPLE_DATABASE_ID};
        int[] expectedGeometryPosition = new int[]{SAMPLE_PANEL_HEIGHT, SAMPLE_PANEL_WIDTH, SAMPLE_PANEL_X_POSITION, SAMPLE_PANEL_Y_POSITION};
        // Construct the object
        TemplatePanel panel = new TemplatePanel(SAMPLE_MEASURE, SAMPLE_ITEM_GROUP, SAMPLE_DATABASE_ID, SAMPLE_METADATA);
        panel.setTitle(SAMPLE_TITLE);
        panel.setDescription(SAMPLE_DESCRIPTION);
        // Execute method
        StringBuilder result = panel.genCommonJson(SAMPLE_PANEL_HEIGHT, SAMPLE_PANEL_WIDTH, SAMPLE_PANEL_X_POSITION, SAMPLE_PANEL_Y_POSITION);
        // Verify results
        assertEquals(genExpectedCommonJsonBase(SAMPLE_TITLE, SAMPLE_DESCRIPTION, expectedConfigItems, expectedGeometryPosition, SAMPLE_METADATA), result.toString());
    }

    @Test
    void testGenCommonJsonExceptionHandling() {
        // Construct the object
        TemplatePanel panel = new TemplatePanel(SAMPLE_MEASURE, SAMPLE_ITEM_GROUP, SAMPLE_DATABASE_ID, SAMPLE_METADATA);
        // Execute the method and ensure the right error is thrown for missing title
        NullPointerException thrownError = assertThrows(NullPointerException.class, () -> panel.genCommonJson(1, 1, 1, 1));
        // Test if error message thrown is accurate
        assertEquals("Title has not yet been set or is empty!", thrownError.getMessage());

        // Prepare the test for description
        panel.setTitle(SAMPLE_TITLE);
        // Execute the method and ensure the right error is thrown for missing description
        thrownError = assertThrows(NullPointerException.class, () -> panel.genCommonJson(1, 1, 1, 1));
        // Test if error message thrown is accurate
        assertEquals("Description has not yet been set or is empty!", thrownError.getMessage());
    }

    @Test
    void testConstructInvalid() {
        // Construct the object
        TemplatePanel panel = new TemplatePanel(SAMPLE_MEASURE, SAMPLE_ITEM_GROUP, SAMPLE_DATABASE_ID, SAMPLE_METADATA);
        // Execute the method and ensure the right error is thrown
        UnsupportedOperationException thrownError = assertThrows(UnsupportedOperationException.class, () -> panel.construct(1, 1, 1, 1));
        // Test if error message thrown is accurate
        assertEquals("Construct() method is not supported for TemplatePanel. Please use their implementation classes instead!", thrownError.getMessage());
    }

    @Test
    void testGetMeasure() {
        // Construct the object
        TemplatePanel panel = new TemplatePanel(SAMPLE_MEASURE, SAMPLE_ITEM_GROUP, SAMPLE_DATABASE_ID, SAMPLE_METADATA);
        // Execute the method and verify result
        assertEquals(SAMPLE_MEASURE, panel.getMeasure());
    }

    public static String genExpectedCommonJsonBase(String title, String description, String[] metadata, int[] geometryPositions, List<String[]> itemDetails) {
        return genExpectedCommonJsonBase(title, description, metadata, geometryPositions, itemDetails, "");
    }

    public static String genExpectedCommonJsonBase(String title, String description, String[] metadata, int[] geometryPositions, List<String[]> itemDetails, String query) {
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
                .append(genExpectedTransformationOption(itemDetails));
        return results.toString();
    }

    public static String genExpectedTransformationOption(List<String[]> itemDetails) {
        StringBuilder indexMapper = new StringBuilder();
        StringBuilder colNameMapper = new StringBuilder();
        int counter = 1;
        for (String[] metadata : itemDetails) {
            if (indexMapper.length() != 0) indexMapper.append(",");
            if (colNameMapper.length() != 0) colNameMapper.append(",");
            indexMapper.append("\"").append(metadata[1]).append("\":").append(counter++);
            colNameMapper.append("\"").append(metadata[1]).append("\":\"").append(metadata[0]).append("\"");
        }
        StringBuilder results = new StringBuilder();
        results.append("\"transformations\": [")
                .append("{ \"id\": \"organize\",\"options\": {\"excludeByName\": {},")
                .append("\"indexByName\": {\"time\": 0,").append(indexMapper).append("},")
                .append("\"renameByName\": {").append(colNameMapper).append("}}}")
                .append("]");
        return results.toString();
    }
}