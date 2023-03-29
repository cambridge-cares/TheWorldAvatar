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
    private final String source;
    private final String sourceUser;
    private final String sourcePass;
    private final String target;
    private final String targetUser;
    private final String targetPass;

    /**
     * Standard Constructor setting the SQL database credentials.
     *
     * @param srcDB   Sets the source database.
     * @param srcUser Sets the user's credential for the source database.
     * @param srcPass Sets the user's password credential for the source database.
     * @param tgtDB   Sets the target database.
     * @param tgtUser Sets the user's credential for the target database.
     * @param tgtPass Sets the user's password credential for the target database.
     */
    public SqlBridge(String srcDB, String srcUser, String srcPass, String tgtDB, String tgtUser, String tgtPass) {
        LOGGER.info("Pinging origin database for availability...");
        try (Connection conn = connect(srcDB, srcUser, srcPass)) {
            if (conn.isValid(60)) {
                this.source = srcDB;
                this.sourceUser = srcUser;
                this.sourcePass = srcPass;
            } else {
                LOGGER.fatal(INVALID_CONNECTION_MESSAGE);
                throw new JPSRuntimeException(INVALID_CONNECTION_MESSAGE);
            }
        } catch (SQLException e) {
            LOGGER.fatal("Error connecting to origin database: " + e);
            throw new JPSRuntimeException("Error connecting to origin database: " + e);
        }
        LOGGER.info("Pinging destination database for availability...");
        try (Connection conn = connect(tgtDB, tgtUser, tgtPass)) {
            if (conn.isValid(60)) {
                this.target = tgtDB;
                this.targetUser = tgtUser;
                this.targetPass = tgtPass;
            } else {
                LOGGER.fatal(INVALID_CONNECTION_MESSAGE);
                throw new JPSRuntimeException(INVALID_CONNECTION_MESSAGE);
            }
        } catch (SQLException e) {
            LOGGER.fatal("Error connecting to destination database: " + e);
            throw new JPSRuntimeException("Error connecting to destination database: " + e);
        }
    }

    /**
     * Transfer all data from the source database to target database.
     */
    public void transfer() {
        try (Connection sourceConn = connect(this.source, this.sourceUser, this.sourcePass)) {
            LOGGER.info("Retrieving all table names from source database...");
            Queue<String> tables = retrieveTableNames(sourceConn);
            // For each table in the queue
            while (!tables.isEmpty()) {
                // Retrieve the current table
                String tableName = tables.poll();
                LOGGER.info("Retrieving table schema of " + tableName + "...");
                String tableSchema = retrieveTableSchema(sourceConn, tableName);
                LOGGER.info("Retrieving all data in" + tableName + "...");
                int rowCount;
                String templateStatement;
                Map<Integer, Queue<Object>> values;
                try (ResultSet results = queryTable(sourceConn, tableName)) {
                    // Set the result set to the last row and get current row number
                    rowCount = results.last() ? results.getRow() : 0;
                    LOGGER.info("Storing source data into memory...");
                    templateStatement = createPreparedStatement(results, tableName);
                    values = createValuesMap(results);
                } catch (SQLException ex) {
                    LOGGER.fatal("Unable to parse query results: " + ex);
                    throw new JPSRuntimeException("Unable to parse query results: " + ex);
                }
                // Set up connection to target database
                try (Connection targetConn = connect(this.target, this.targetUser, this.targetPass)) {
                    LOGGER.info("Initialising table " + tableName + " in target database...");
                    createTargetTable(targetConn, tableName, tableSchema);
                    LOGGER.info("Transferring data to target database...");
                    uploadData(targetConn, templateStatement, rowCount, values);
                } catch (SQLException e) {
                    LOGGER.fatal("Error transferring data to target: " + e);
                    throw new JPSRuntimeException("Error transferring data to target: " + e);
                }
            }
        } catch (SQLException e) {
            LOGGER.fatal("Error connecting to source database: " + e);
            throw new JPSRuntimeException("Error connecting to source database: " + e);
        }
    }

    /**
     * Connect to the specified PostgreSQL database.
     *
     * @param url      The database url.
     * @param user     The database's username.
     * @param password The database's password.
     * @return the database Connection object.
     */
    private Connection connect(String url, String user, String password) throws SQLException {
        try {
            Class.forName("org.postgresql.Driver");
        } catch (ClassNotFoundException e) {
            LOGGER.fatal(e);
            throw new JPSRuntimeException(e);
        }
        return DriverManager.getConnection(url, user, password);
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
        try (Statement stmt = srcConn.createStatement()) {
            // Retrieve the column name and data type as this format `columnName dataType, `
            String schemaQuery = "SELECT CONCAT(column_name, ' ', data_type, ', ') FROM information_schema.columns WHERE table_name = '" + tableName + "'";
            ResultSet columnsResultSet = stmt.executeQuery(schemaQuery);
            // When there is a next row available,
            while (columnsResultSet.next()) {
                // Append each row's value in the format `columnName dataType, ` to the schema string
                schema.append(columnsResultSet.getString(1));
            }
            // Remove the last two characters `, `
            return schema.substring(0, schema.length() - 2);
        } catch (SQLException e) {
            LOGGER.fatal("Failed to retrieve table's schema for " + tableName + " . " + e);
            throw new JPSRuntimeException("Failed to retrieve table's schema for " + tableName + " . " + e);
        }
    }

    /**
     * Query all SQL statements from the source database.
     *
     * @param srcConn   The source database connection object.
     * @param tableName The source table to be retrieved.
     * @return The query results for further processing.
     */
    private ResultSet queryTable(Connection srcConn, String tableName) {
        // Create the query
        StringBuilder query = new StringBuilder();
        query.append("SELECT * FROM \"" + tableName + "\" LIMIT 1000;");
        // Execute the query at the source database
        try {
            // Create a new connection that must be closed later
            // This result set type is necessary to reverse the row pointer. Otherwise, it is only forward-only, which we dont want
            Statement stmt = srcConn.createStatement(ResultSet.TYPE_SCROLL_INSENSITIVE, ResultSet.CONCUR_READ_ONLY);
            return stmt.executeQuery(query.toString());
        } catch (SQLException e) {
            LOGGER.fatal("Failed to query for source data. " + e);
            throw new JPSRuntimeException("Failed to query for source data. " + e);
        }
    }

    /**
     * Create the prepared statement template to load the values into the target table.
     * Note that prepared statements are for each row, and number of parameters ie ?
     * should not exceed the column count.
     *
     * @param results     The query results from the source table.
     * @param targetTable The table to upload the data to.
     * @return The template statement.
     */
    private String createPreparedStatement(ResultSet results, String targetTable) throws SQLException {
        ResultSetMetaData metaData = results.getMetaData();
        // Retrieve the column and row count
        int colCount = metaData.getColumnCount();
        // Create insert and values query
        StringBuilder templateQuery = new StringBuilder();
        StringBuilder valuesQuery = new StringBuilder();
        // Append starting statements
        templateQuery.append("INSERT INTO \"" + targetTable + "\" (");
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

    /**
     * Create the values map connecting each column to their values.
     * As each row is iterated in order, the values are stored in order as well.
     * The map is intended to append values to prepared statement by row.
     *
     * @param results The query results from the source table.
     * @return A map linking all values to their columns and rows.
     */
    private Map<Integer, Queue<Object>> createValuesMap(ResultSet results) throws SQLException {
        Map<Integer, Queue<Object>> values = new HashMap<>();
        // Reset cursor back to first row
        results.beforeFirst();
        while (results.next()) {
            // Iterate over all the columns
            for (int i = 1; i <= results.getMetaData().getColumnCount(); i++) {
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
        return values;
    }

    /**
     * Create the target table in the target database if it doesn't exist.
     *
     * @param targetConn  The target database connection object.
     * @param targetTable The target table to store data, usually the same name as source table.
     * @param tableSchema The table schema to be created.
     */
    private void createTargetTable(Connection targetConn, String targetTable, String tableSchema) {
        // Create the target table creation query
        StringBuilder initTable = new StringBuilder();
        initTable.append("CREATE TABLE IF NOT EXISTS \"" + targetTable + "\" (" + tableSchema + ");");
        // Execute the query at the target database
        try (Statement stmt = targetConn.createStatement()) {
            stmt.executeUpdate(initTable.toString());
        } catch (SQLException e) {
            LOGGER.fatal("Failed to create table at target database. " + e);
            throw new JPSRuntimeException("Failed to create table at target database. " + e);
        }
    }

    /**
     * Uploads the query data to the target database.
     *
     * @param targetConn The Connection object to the target database.
     * @param template   The statement template.
     * @param rowCount   The taotal number of rows.
     * @param values     The map containing the values linked to their row and columns.
     */
    private void uploadData(Connection targetConn, String template, int rowCount, Map<Integer, Queue<Object>> values) throws SQLException {
        // Establish a connection to target table
        try (PreparedStatement insertStmt = targetConn.prepareStatement(template)) {
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
        }
    }
}
