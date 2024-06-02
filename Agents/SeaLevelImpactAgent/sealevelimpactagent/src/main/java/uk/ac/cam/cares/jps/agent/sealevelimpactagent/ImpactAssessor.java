package uk.ac.cam.cares.jps.agent.sealevelimpactagent;
import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;

import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;


public class ImpactAssessor {

    /**
     * @param remoteRDBStoreClient
     * @param sspScenario
     * @param confidence
     * @param quantile
     * @return
     * @throws SQLException
     */
    public String getSeaLevelChangeUUID(RemoteRDBStoreClient remoteRDBStoreClient, String sspScenario, String confidence, Double quantile) throws SQLException {

        String findNearestNode_sql = "";

        try (Connection connection = remoteRDBStoreClient.getConnection()) {

            try (Statement statement = connection.createStatement()) {
                try (ResultSet resultSet = statement.executeQuery(findNearestNode_sql)) {
                    if (resultSet.next()) {
                        // Assuming 'uuid' and 'distance' are columns in your query result
                        String uuid = resultSet.getString("uuid");
                        return uuid;
                    } else {
                        // No results found
                        return null;
                    }
                }
            }
        }
    }


    /**
     * Create connection to remoteStoreClient and execute SQL statement
     * @param connection PostgreSQL connection object
     * @param sql SQl statement to execute
     */
    private void executeSql(Connection connection, String sql) throws SQLException {
        try (Statement statement = connection.createStatement()) {
            statement.execute(sql);
        }
    }
}


