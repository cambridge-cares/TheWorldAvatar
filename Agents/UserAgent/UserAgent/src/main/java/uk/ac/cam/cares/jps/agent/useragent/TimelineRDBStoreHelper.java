package uk.ac.cam.cares.jps.agent.useragent;

import org.apache.log4j.Logger;
import org.json.JSONArray;
import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;

public class TimelineRDBStoreHelper {
    private RemoteRDBStoreClient postgresRdbClient;
    private Logger LOGGER = Logger.getLogger(TimelineRDBStoreHelper.class);

    public TimelineRDBStoreHelper(RemoteRDBStoreClient postgresRdbClient) {
        this.postgresRdbClient = postgresRdbClient;
        initRDBStore();
    }

    private void initRDBStore() {
        initTimelineSchema();
        initPhoneTable();
    }

    private void initTimelineSchema() {
        JSONArray result = postgresRdbClient.executeQuery("SELECT schema_name \n" +
                "FROM information_schema.schemata\n" +
                "WHERE schema_name = 'timeline';");
        LOGGER.debug(result);
        if (!result.isEmpty()) {
            return;
        }

        LOGGER.debug("creating schema");
        postgresRdbClient.executeUpdate("CREATE SCHEMA timeline;");
    }

    private void initPhoneTable() {
        JSONArray result = postgresRdbClient.executeQuery("SELECT table_name\n" +
                "FROM information_schema.tables\n" +
                "WHERE table_schema = 'timeline'\n" +
                "AND table_name = 'smartPhone';");

        if (!result.isEmpty()) {
            return;
        }

        postgresRdbClient.executeUpdate("CREATE TABLE IF NOT EXISTS \"postgres\".\"timeline\".\"smartPhone\" (" +
                "phone_id CHAR(36) PRIMARY KEY," +
                "user_id CHAR(36));");
    }

    public void registerPhone(String phoneId, String userId) {
        postgresRdbClient.executeUpdate("INSERT INTO \"timeline\".\"smartPhone\" (\"phone_id\", \"user_id\")\n" +
                String.format("VALUES ('%s', '%s');", phoneId, userId));
    }
}
