package uk.ac.cam.cares.jps.agent.dashboard.json.panel.types;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

class TransformationOptionsTest {
    private static TransformationOptions options;
    private static final String SAMPLE_FIRST_ASSET_NAME = "asset one";
    private static final String SAMPLE_FIRST_ASSET_COL_NAME = "column7";
    private static final String SAMPLE_ASSET_TABLE_NAME = "table1";
    private static final String SAMPLE_SEC_ASSET_NAME = "asset two";
    private static final String SAMPLE_SEC_ASSET_COL_NAME = "column16";
    private static final List<String[]> SAMPLE_METADATA = new ArrayList<>();

    @BeforeAll
    static void setup() {
        SAMPLE_METADATA.add(new String[]{SAMPLE_FIRST_ASSET_NAME, SAMPLE_FIRST_ASSET_COL_NAME, SAMPLE_ASSET_TABLE_NAME});
        SAMPLE_METADATA.add(new String[]{SAMPLE_SEC_ASSET_NAME, SAMPLE_SEC_ASSET_COL_NAME, SAMPLE_ASSET_TABLE_NAME});
    }

    @BeforeEach
    void reset() {
        options = new TransformationOptions();
    }

    @Test
    void testAddOrganizeTransformation_WithoutNameSuffix() {
        // Execute method
        options.addOrganizeTransformation(SAMPLE_METADATA);
        // Verify result
        assertEquals("[" + genExpectedOrganizeTransformation(SAMPLE_METADATA, "") + "]", options.construct());
    }

    @Test
    void testAddOrganizeTransformation_WithNameSuffix() {
        // Execute method
        options.addOrganizeTransformation(" (range)", SAMPLE_METADATA);
        // Verify result
        assertEquals("[" + genExpectedOrganizeTransformation(SAMPLE_METADATA, " (range)") + "]", options.construct());
    }

    @Test
    void testAddGroupByTransformation() {
        // Execute method
        options.addGroupByTransformation("range", SAMPLE_METADATA);
        // Verify result
        assertEquals("[" + genExpectedGroupByTransformation("range", SAMPLE_METADATA) + "]", options.construct());
    }

    @Test
    void testConstruct_MultipleTransformations() {
        // Set up the test
        String expected = "[" + genExpectedGroupByTransformation("range", SAMPLE_METADATA) + "," + genExpectedOrganizeTransformation(SAMPLE_METADATA, " (range)") + "]";
        options.addGroupByTransformation("range", SAMPLE_METADATA);
        options.addOrganizeTransformation(" (range)", SAMPLE_METADATA);
        // Execute and verify result
        assertEquals(expected, options.construct());
    }

    public static String genExpectedGroupByTransformation(String aggregateType, List<String[]> itemDetails) {
        StringBuilder fieldAggregations = new StringBuilder();
        // Process metadata into the required format
        for (String[] metadata : itemDetails) {
            // Only append a comma at the start if it is not the first set
            if (fieldAggregations.length() != 0) fieldAggregations.append(",");
            // Append in format of "columnName":{"aggregations" : [], "operation":"aggregate" }
            fieldAggregations.append("\"").append(metadata[1])
                    .append("\":{")
                    .append("\"aggregations\":[\"").append(aggregateType)
                    .append("\"],\"operation\":\"aggregate\"")
                    .append("}");
        }
        // Generate the expected output
        StringBuilder results = new StringBuilder();
        results.append("{\"id\":\"groupBy\",\"options\":{\"fields\":{")
                .append("\"interval\":{\"aggregations\":[],\"operation\":\"groupby\"},").append(fieldAggregations)
                .append("}}}");
        return results.toString();
    }

    public static String genExpectedOrganizeTransformation(List<String[]> itemDetails, String colNameSuffix) {
        StringBuilder indexMapper = new StringBuilder();
        StringBuilder colNameMapper = new StringBuilder();
        int counter = 1;
        for (String[] metadata : itemDetails) {
            if (indexMapper.length() != 0) indexMapper.append(",");
            if (colNameMapper.length() != 0) colNameMapper.append(",");
            indexMapper.append("\"").append(metadata[1]).append("\":").append(counter++);
            colNameMapper.append("\"").append(metadata[1]).append(colNameSuffix).append("\":\"").append(metadata[0]).append("\"");
        }
        // Generate the expected output
        return "{\"id\":\"organize\",\"options\":{\"excludeByName\":{}," +
                "\"indexByName\":{\"time\": 0," + indexMapper + "}," +
                "\"renameByName\":{" + colNameMapper + "}}}";
    }
}