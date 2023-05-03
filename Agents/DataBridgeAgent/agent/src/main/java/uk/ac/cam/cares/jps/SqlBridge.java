package uk.ac.cam.cares.jps;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONObject;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.sql.PostGISClient;
import uk.ac.cam.cares.jps.sql.SqlConnectionPool;

import java.sql.*;
import java.util.ArrayDeque;
import java.util.Queue;

/**
 * A bridge that connects and transfer data between 2 SQL databases.
 *
 * @author qhouyee
 */
public class SqlBridge {
    private static final Logger LOGGER = LogManager.getLogger(DataBridgeAgent.class);
    private static final String INVALID_CONNECTION_MESSAGE = "Connection is invalid...";
    private final SqlConnectionPool pool;

    /**
     * Standard Constructor verifying the SQL database connections.
     *
     * @param config An array containing the SQL credentials.
     */
    public SqlBridge(String[] config) {
        this.pool = new SqlConnectionPool(config);
        LOGGER.info("Pinging source database for availability...");
        try (Connection srcConn = this.pool.getSourceConnection()) {
            if (!srcConn.isValid(60)) {
                LOGGER.fatal(INVALID_CONNECTION_MESSAGE);
                throw new JPSRuntimeException(INVALID_CONNECTION_MESSAGE);
            }
        } catch (SQLException e) {
            LOGGER.fatal("Error connecting to source database: " + e);
            throw new JPSRuntimeException("Error connecting to source database: " + e);
        }
        LOGGER.info("Pinging target database for availability...");
        try (Connection targetConn = this.pool.getTargetConnection()) {
            if (!targetConn.isValid(60)) {
                LOGGER.fatal(INVALID_CONNECTION_MESSAGE);
                throw new JPSRuntimeException(INVALID_CONNECTION_MESSAGE);
            }
        } catch (SQLException e) {
            LOGGER.fatal("Error connecting to target database: " + e);
            throw new JPSRuntimeException("Error connecting to target database: " + e);
        }
        LOGGER.info("Both databases are available for interaction...");
    }

    /**
     * Transfer all data from the source database to target database.
     */
    public JSONObject transfer(boolean isStack) {
        JSONObject response = new JSONObject();
        Queue<String> tables;
        try (Connection sourceConn = this.pool.getSourceConnection()) {
            LOGGER.info("Retrieving all table names from source database...");
            tables = retrieveTableNames(sourceConn);
        } catch (SQLException e) {
            LOGGER.fatal("There was an error interacting with the source database: " + e);
            throw new JPSRuntimeException("There was an error interacting with the source database: " + e);
        }
        PostGISClient dockerClient = new PostGISClient(isStack);
        // Create a new client to execute sql commands on docker
        String[] configs = this.pool.getConfigs();
        // For each table in the queue
        while (!tables.isEmpty()) {
            // Retrieve the current table
            String tableName = tables.poll();
            LOGGER.info("Transferring table data into target database...");
            String cmd = dockerClient.transferTable(tableName, configs[0], configs[1], configs[2], configs[3], configs[4], configs[5]);
            if (!cmd.isEmpty()){
                if(response.isEmpty()){
                    response.put("Result", "Agent is not running on a stack, please run the following command(s) for transfer. Please exclude the two additional backslash for the table name ie '-t \"'");
                }
                response.accumulate("Result", cmd);
            }
        }
        return response;
    }

    /**
     * Retrieve all available tables and their names from the source database.
     *
     * @param srcConn The source database connection object.
     * @return A queue containing all table names.
     */
    private Queue<String> retrieveTableNames(Connection srcConn) throws SQLException {
        Queue<String> tables = new ArrayDeque<>();
        DatabaseMetaData dbmd = srcConn.getMetaData();
        ResultSet dbTables = dbmd.getTables(null, null, "%", new String[]{"TABLE"});
        while (dbTables.next()) {
            tables.offer(dbTables.getString("TABLE_NAME"));
        }
        return tables;
    }
}
