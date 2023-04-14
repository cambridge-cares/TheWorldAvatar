package uk.ac.cam.cares.jps;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

import java.sql.*;
import java.util.ArrayDeque;
import java.util.HashMap;
import java.util.Map;
import java.util.Queue;

/**
 * A bridge that connects and transfer data between 2 SQL databases.
 *
 * @author qhouyee
 */
public class SqlBridge {
    private static final Logger LOGGER = LogManager.getLogger(DataBridgeAgent.class);
    private static final String INVALID_CONNECTION_MESSAGE = "Connection is invalid...";
    private static final int BATCH_LIMIT = 10000;
    private final SqlConnectionPool pool;

    /**
     * Standard Constructor verifying the SQL database connections.
     *
     * @param config An array containing the SQL credentials.
     */
    public SqlBridge(String[] config) {
        this.pool = new SqlConnectionPool(config);
        LOGGER.info("Pinging source database for availability...");
        try (Connection srcConn = SqlConnectionPool.getSourceConnection()) {
            if (!srcConn.isValid(60)) {
                LOGGER.fatal(INVALID_CONNECTION_MESSAGE);
                throw new JPSRuntimeException(INVALID_CONNECTION_MESSAGE);
            }
        } catch (SQLException e) {
            LOGGER.fatal("Error connecting to source database: " + e);
            throw new JPSRuntimeException("Error connecting to source database: " + e);
        }
        LOGGER.info("Pinging target database for availability...");
        try (Connection targetConn = SqlConnectionPool.getTargetConnection()) {
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
    public void transfer() {
        try (Connection sourceConn = SqlConnectionPool.getSourceConnection()) {
            LOGGER.info("Retrieving all table names from source database...");
            Queue<String> tables = retrieveTableNames(sourceConn);
            // For each table in the queue
            while (!tables.isEmpty()) {
                // Retrieve the current table
                String tableName = tables.poll();
                LOGGER.info("Retrieving table schema for " + tableName + "...");
                String tableSchema = retrieveTableSchema(sourceConn, tableName);
                LOGGER.info("Initialising table in target database...");
                createTargetTable(tableName, tableSchema);
                LOGGER.info("Creating prepared statement template...");
                String templateStatement = genPreparedStatementTemplate(sourceConn, tableName);
                // Set up the row numbers in an array to allow the values to change in-place even in nested methods
                int[] rows = new int[2];
                rows[0] = 1; // Current row of each batch
                rows[1] = 0; // Offset the results by this amount for each batch
                // Continue the batch retrieval when each batch has at least 1 row available
                while (rows[0] > 0) {
                    LOGGER.info("Retrieving data in batches starting from the " + rows[1] + " rows...");
                    Map<Integer, Queue<Object>> values = retrieveBatchValues(sourceConn, tableName, rows);
                    LOGGER.info("Transferring batch to target database...");
                    uploadData(templateStatement, rows[0], values);
                    rows[1] += BATCH_LIMIT;
                }
            }
        } catch (SQLException e) {
            LOGGER.fatal("There was an error interacting with the source database: " + e);
            throw new JPSRuntimeException("There was an error interacting with the source database: " + e);
        }
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

    /**
     * Retrieve the table schema.
     *
     * @param srcConn   The source database connection object.
     * @param tableName The source table to be retrieved.
     * @return The table's schema as a formatted SQL string.
     */
    private String retrieveTableSchema(Connection srcConn, String tableName) {
        StringBuilder schema = new StringBuilder();
        // Retrieve the column name and data type as this format `columnName dataType, `
        String schemaQuery = "SELECT CONCAT(column_name, ' ', data_type, ', ') FROM information_schema.columns WHERE table_name = '" + tableName + "'";
        try (ResultSet columnsResultSet = queryTable(srcConn, schemaQuery)) {
            // When there is a next row available,
            while (columnsResultSet.next()) {
                // Append each row's value in the format `columnName dataType, ` to the schema string
                schema.append(columnsResultSet.getString(1));
            }
            // Remove the last two characters `, ` and return the schema
            return schema.substring(0, schema.length() - 2);
        } catch (SQLException e) {
            LOGGER.fatal("Failed to retrieve table's schema for " + tableName + " . " + e);
            throw new JPSRuntimeException("Failed to retrieve table's schema for " + tableName + " . " + e);
        }
    }

    /**
     * Create the target table in the target database if it doesn't exist.
     *
     * @param targetTable The target table to store data, usually the same name as source table.
     * @param tableSchema The table schema to be created.
     */
    private void createTargetTable(String targetTable, String tableSchema) {
        // Create the target table creation query
        String initTable = "CREATE TABLE IF NOT EXISTS \"" + targetTable + "\" (" + tableSchema + ");";
        // Execute the query at the target database
        try (Connection targetConn = SqlConnectionPool.getTargetConnection();
             Statement stmt = targetConn.createStatement()) {
            stmt.executeUpdate(initTable);
        } catch (SQLException e) {
            LOGGER.fatal("Failed to create table at target database. " + e);
            throw new JPSRuntimeException("Failed to create table at target database. " + e);
        }
    }

    /**
     * Generate the prepared statement template to load the values into the target table.
     * Note that prepared statements are for each row, and number of parameters ie ?
     * should not exceed the column count.
     *
     * @param srcConn  The source database connection object.
     * @param srcTable The table to be queried from.
     * @return The template statement.
     */
    private String genPreparedStatementTemplate(Connection srcConn, String srcTable) throws SQLException {
        // Create the query
        String srcQuery = "SELECT * FROM \"" + srcTable + "\" LIMIT 1;";
        try (ResultSet results = queryTable(srcConn, srcQuery)) {
            ResultSetMetaData metaData = results.getMetaData();
            // Retrieve the column and row count
            int colCount = metaData.getColumnCount();
            // Create insert and values query
            StringBuilder templateQuery = new StringBuilder();
            StringBuilder valuesQuery = new StringBuilder();
            // Append starting statements
            // Note that since we are duplicating the source table, the target table name is the same
            templateQuery.append("INSERT INTO \"" + srcTable + "\" (");
            valuesQuery.append(") VALUES (");
            // Iterate over all the columns
            for (int i = 1; i <= colCount; i++) {
                // Retrieve column name and append to the first query
                templateQuery.append(metaData.getColumnName(i));
                templateQuery.append(",");
                // Append the parameter placeholder of each column for prepared statement
                valuesQuery.append("?,");
            }
            // Remove the last comma for both queries
            templateQuery.deleteCharAt(templateQuery.length() - 1);
            valuesQuery.deleteCharAt(valuesQuery.length() - 1);
            // Combine the queries and close it
            templateQuery.append(valuesQuery).append(")");
            return templateQuery.toString();
        }
    }

    /**
     * Retrieve the values by batch, and store the values as a map connecting each column to their values.
     * As each row is iterated in order, the values are stored in order as well.
     * The map is intended to append values to the prepared statement by row.
     *
     * @param srcConn   The source database connection object.
     * @param tableName The table to be queried from.
     * @param rows      An array to update the current row number, and indicate the offset row number.
     * @return A map linking all values to their columns and rows.
     */
    private Map<Integer, Queue<Object>> retrieveBatchValues(Connection srcConn, String tableName, int[] rows) throws SQLException {
        Map<Integer, Queue<Object>> values = new HashMap<>();
        // Create the query statement
        String query = "SELECT * FROM \"" + tableName + "\" ORDER BY 1 LIMIT " + BATCH_LIMIT + " OFFSET " + rows[1] + ";";
        try (ResultSet results = queryTable(srcConn, query)) {
            // Set the result set to the last row of this batch and get current row number
            rows[0] = results.last() ? results.getRow() : 0;
            int colCount = results.getMetaData().getColumnCount();
            // Reset cursor back to first row
            results.beforeFirst();
            // There is still another row, iterate over all the columns and add their values accordingly
            while (results.next()) {
                for (int i = 1; i <= colCount; i++) {
                    // If this integer has been added as a key
                    if (values.containsKey(i)) {
                        // Append the cell value into the relevant queue in the mappings
                        values.get(i).offer(results.getObject(i));
                    } else {
                        // Create a new queue and add value inside
                        Queue<Object> newQueue = new ArrayDeque<>();
                        newQueue.offer(results.getObject(i));
                        // Store them in the mappings
                        values.put(i, newQueue);
                    }
                }
            }
        } catch (SQLException ex) {
            LOGGER.fatal("Unable to retrieve values from the source database: " + ex);
            throw new JPSRuntimeException("Unable to retrieve values from the source database: " + ex);
        }
        return values;
    }

    /**
     * Query SQL statements from the database.
     *
     * @param srcConn The source database connection object.
     * @param query   The SQL query to execute on the connection.
     * @return The query results for further processing.
     */
    private ResultSet queryTable(Connection srcConn, String query) {
        try {
            // Create a new connection that must be closed later
            // This result set type is necessary to reverse the row pointer. Otherwise, it is only forward-only, which we dont want
            Statement stmt = srcConn.createStatement(ResultSet.TYPE_SCROLL_INSENSITIVE, ResultSet.CONCUR_READ_ONLY);
            return stmt.executeQuery(query);
        } catch (SQLException e) {
            LOGGER.fatal("Failed to query for source data. " + e);
            throw new JPSRuntimeException("Failed to query for source data. " + e);
        }
    }

    /**
     * Uploads the query data to the target database.
     *
     * @param template The statement template.
     * @param rowCount The total number of rows.
     * @param values   The map containing the values linked to their row and columns.
     */
    private void uploadData(String template, int rowCount, Map<Integer, Queue<Object>> values) {
        // Establish a connection to target table
        try (Connection targetConn = SqlConnectionPool.getTargetConnection();
             PreparedStatement insertStmt = targetConn.prepareStatement(template)) {
            // Iterate through all rows
            for (int i = 1; i <= rowCount; i++) {
                // For all the columns
                for (Integer key : values.keySet()) {
                    // Retrieve their values stored as a queue
                    Queue<Object> queueValues = values.get(key);
                    // Set prepared statement column values for that row
                    insertStmt.setObject(key, queueValues.poll());
                }
                // Upload data for each row individually, as prepared statement parameters must change
                insertStmt.executeUpdate();
            }
        } catch (SQLException e) {
            LOGGER.fatal("Failed to upload data for target database: " + e);
            throw new JPSRuntimeException("Failed to upload data for target database: " + e);
        }
    }
}
