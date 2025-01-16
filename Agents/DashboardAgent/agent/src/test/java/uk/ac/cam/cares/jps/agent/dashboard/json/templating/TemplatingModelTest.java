package uk.ac.cam.cares.jps.agent.dashboard.json.templating;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import uk.ac.cam.cares.jps.agent.dashboard.TestUtils;
import uk.ac.cam.cares.jps.agent.dashboard.datamodel.Measure;
import uk.ac.cam.cares.jps.agent.dashboard.datamodel.Organisation;
import uk.ac.cam.cares.jps.agent.dashboard.json.panel.layout.TemporalInterval;
import uk.ac.cam.cares.jps.agent.dashboard.utils.StringHelper;

import java.util.*;

import static org.junit.jupiter.api.Assertions.*;

public class TemplatingModelTest {
    private static Map<String, String> sampleDbConnectionIdMap;
    private static final String TIME_INTERVAL_FILTER_DESCRIPTION = "A filter to display the time interval requested by the user in the trend related charts.";
    private static final String FACILITY_FILTER_DESCRIPTION = "A filter at the facility level to view the specified facilities and their associated measures.";

    @BeforeAll
    static void genSampleData() {
        sampleDbConnectionIdMap = TestUtils.genSampleDatabaseConnectionMap();
    }

    @Test
    void testConstruct_AssetsOnly() {
        Organisation sample = TestUtils.genSampleAssetMeasures(null);
        // Construct and execute the method
        String result = new TemplatingModel(sampleDbConnectionIdMap, sample).construct();
        // Test outputs
        assertEquals(genExpectedJsonSyntax(sampleDbConnectionIdMap, sample), result);
    }

    @Test
    void testConstruct_RoomsOnly() {
        Organisation sample = TestUtils.genSampleRoomMeasures(null, true);
        // Construct and execute the method
        String result = new TemplatingModel(sampleDbConnectionIdMap, sample).construct();
        // Test outputs
        assertEquals(genExpectedJsonSyntax(sampleDbConnectionIdMap, sample), result);
    }

    @Test
    void testConstruct_SystemsOnly() {
        Organisation sample = TestUtils.genSampleSystemMeasures(null);
        // Construct and execute the method
        String result = new TemplatingModel(sampleDbConnectionIdMap, sample).construct();
        // Test outputs
        assertEquals(genExpectedJsonSyntax(sampleDbConnectionIdMap, sample), result);
    }

    public static String genExpectedJsonSyntax(Map<String, String> databaseConnectionMap, Organisation organisation) {
        StringBuilder builder = new StringBuilder();
        StringBuilder tempBuilder = new StringBuilder();
        builder.append("{\"enable\": true,\"list\": [");
        // Only generate these variables if there are values
        String connectionID = databaseConnectionMap.values().iterator().next();
        genTemporalSelectors(tempBuilder, connectionID);
        genFacilityItemTypeVariables(tempBuilder, organisation, connectionID);

        // Retrieve all associated items and their measures for each group
        organisation.getAllItemGroups().forEach(group -> {
            organisation.getAllMeasureNames(group).forEach(measure -> {
                // Stores the metadata of item name and column name
                Measure currentMeasure = organisation.getMeasure(group, measure);
                Queue<String[]> availableMeasureMetadata = currentMeasure.getTimeSeriesData();
                // Only create a new variable for each measure of an item group if any metadata is available
                if (!availableMeasureMetadata.isEmpty()) {
                    tempBuilder.append(",")
                            .append(PostgresVariableTest.genExpectedPostgresVarSyntaxForMeasureFilter(measure, group, availableMeasureMetadata, connectionID));
                }
            });
        });
        builder.append(tempBuilder)
                .append("]}");
        return builder.toString();
    }

    private static void genTemporalSelectors(StringBuilder tempBuilder, String connectionID) {
        List<String> temporalIntervals = new ArrayList<>();
        temporalIntervals.add(TemporalInterval.DAILY_OVER_WEEK);
        temporalIntervals.add(TemporalInterval.DAILY_OVER_MONTH);
        temporalIntervals.add(TemporalInterval.WEEKLY_OVER_MONTH);
        temporalIntervals.add(TemporalInterval.MONTHLY);
        tempBuilder.append(CustomVariableTest.genExpectedCustomVariableSyntax(StringHelper.INTERVAL_VARIABLE_NAME, TIME_INTERVAL_FILTER_DESCRIPTION, temporalIntervals, 2, false, false))
                .append(",");
        tempBuilder.append(PostgresVariableTest.genExpectedPostgresVarSyntaxForGenericFilter(StringHelper.REF_MONTH_VARIABLE_NAME, connectionID, TemporalInterval.getMonthMap()))
                .append(",");
    }

    private static void genFacilityItemTypeVariables(StringBuilder tempBuilder, Organisation organisation, String connectionId) {
        // Generate a custom variable for all facilities
        tempBuilder.append(CustomVariableTest.genExpectedCustomVariableSyntax("Facilities", FACILITY_FILTER_DESCRIPTION,
                new ArrayList<>(organisation.getFacilities()),
                0, true, true));
        // Generate postgres variables for each item group
        organisation.getAllItemGroups().forEach(itemGroup -> {
            Queue<String[]> items = organisation.getFacilityItemInventory(itemGroup);
            tempBuilder.append(",").append(PostgresVariableTest.genExpectedPostgresVarSyntaxForItemFilter(itemGroup, connectionId, items));
        });
    }
}