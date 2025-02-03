package uk.ac.cam.cares.jps.agent.dashboard.json.panel.options;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

public class DataSourceTest {
    private static final String SAMPLE_DB_CONNECTION_UID = "315nfs7";

    @Test
    void testConstruct_Postgres() {
        DataSource sample = new DataSource(SAMPLE_DB_CONNECTION_UID);
        assertEquals(genExpectedDataSource(SAMPLE_DB_CONNECTION_UID), sample.construct());
    }

    @Test
    void testConstruct_Grafana() {
        DataSource sample = new DataSource();
        assertEquals(genExpectedDataSource(), sample.construct());
    }

    public static String genExpectedDataSource() {
        String sourceType = "datasource";
        String uid = "grafana";
        return "\"datasource\": {\"type\": \"" + sourceType + "\", \"uid\": \"" + uid + "\"},";
    }

    public static String genExpectedDataSource(String databaseConnectionId) {
        String sourceType = "postgres";
        return "\"datasource\": {\"type\": \"" + sourceType + "\", \"uid\": \"" + databaseConnectionId + "\"},";
    }
}