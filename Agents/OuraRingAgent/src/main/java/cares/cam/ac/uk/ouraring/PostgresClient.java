package cares.cam.ac.uk.ouraring;

import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.HashMap;
import java.util.Map;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import com.cmclinnovations.stack.clients.postgis.PostGISClient;

import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;

public class PostgresClient {
    static RemoteRDBStoreClient remoteRDBStoreClient = PostGISClient.getInstance().getRemoteStoreClient();
    private static final Logger LOGGER = LogManager.getLogger(PostgresClient.class);

    Map<String, String> getUserToOuraApiMap() {
        // column, table, schema names defined in UserAgent
        String query = "SELECT oura_ring_api_key, user_id FROM timeline.\"ouraRing\"";

        Map<String, String> userToOuraApiMap = new HashMap<>();
        try (Connection conn = remoteRDBStoreClient.getConnection(); Statement statement = conn.createStatement();) {
            ResultSet results = statement.executeQuery(query);
            while (results.next()) {
                String userId = results.getString("user_id");
                if (userToOuraApiMap.containsKey(userId)) {
                    LOGGER.warn("Duplicate Oura API key found for {}, skipping", userId);
                    continue;
                }
                userToOuraApiMap.put(results.getString("user_id"), results.getString("oura_ring_api_key"));
            }
        } catch (SQLException e) {
            LOGGER.error(e.getMessage());
            throw new RuntimeException(e);
        }

        return userToOuraApiMap;
    }
}
