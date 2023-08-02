package uk.ac.cam.cares.jps.agent.dashboard.stack;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import uk.ac.cam.cares.jps.agent.dashboard.DashboardAgent;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

import java.sql.*;
import java.util.*;

/**
 * A client that provides method to interact and store information from the PostGIS container within a stack.
 *
 * @author qhouyee
 */
public class PostGisClient {
    private final String STACK_JDBC_URL;
    private final String STACK_POSTGIS_USER;
    private final String STACK_POSTGIS_PASSWORD;
    private final List<String> DATABASE_LIST = new ArrayList<>();
    private static final String ASSET_KEY = "assets";
    private static final Logger LOGGER = LogManager.getLogger(DashboardAgent.class);

    /**
     * Standard Constructor to initialise this client.
     *
     * @param jdbcUrl The JDBC url of the stack's PostGIS container, excluding the database name.
     * @param user    The username to access the stack's PostGIS container.
     * @param pass    The password to access the stack's PostGIS container.
     */
    protected PostGisClient(String jdbcUrl, String user, String pass) {
        // Set up the required information for further interactions
        this.STACK_JDBC_URL = jdbcUrl;
        this.STACK_POSTGIS_USER = user;
        this.STACK_POSTGIS_PASSWORD = pass;
        // Retrieve all available custom database so that methods can perform federated query on each
        // Note that to retrieve all databases, this method must connect to any default database, which is postgres in this case
        try (Connection conn = connect(this.getJdbc("postgres"), user, pass)) {
            this.retrieveAllDatabaseNames(conn);
        } catch (SQLException e) {
            LOGGER.fatal("Error connecting to database: " + e);
            throw new JPSRuntimeException("Error connecting to database: " + e);
        }
    }

    /**
     * Get the list of database names that is available in this stack. This method is accessible for the stack client's usage.
     */
    protected List<String> getDatabaseNames() {
        return this.DATABASE_LIST;
    }

    /**
     * Get the PostGIS username.
     */
    protected String getUsername() {
        return this.STACK_POSTGIS_USER;
    }

    /**
     * Get the PostGIS password.
     */
    protected String getPassword() {
        return this.STACK_POSTGIS_PASSWORD;
    }

    /**
     * Get the JDBC URL of a specified database within this stack.
     *
     * @param database The database of interest in the stack's RDB.
     * @return The JDBC URL of the database specified.
     */
    protected String getJdbc(String database) {
        return this.STACK_JDBC_URL + database;
    }

    /**
     * Get the column and table name corresponding with the asset type and measure from the PostGIS database.
     * The returned map will have the following structure:
     * { assetType1: {
     * assets: [AssetName1, AssetName2, AssetName3],
     * measure1: [[AssetName1, ColName1, TableName1],[AssetName2, ColName2, TableName1],[AssetName3, ColName3, TableName1]],
     * measure2: [[AssetName1, ColName5, TableName1],[AssetName2, ColName6, TableName1],[AssetName3, ColName7, TableName1]],
     * },
     * assetType2: {
     * assets: [AssetName5, AssetName6, AssetName7],
     * measure1: [[AssetName5, ColName1, TableName1],[AssetName6, ColName2, TableName1],[AssetName7, ColName3, TableName1]],
     * measure2: [[AssetName5, ColName5, TableName1],[AssetName6, ColName6, TableName1],[AssetName7, ColName7, TableName1]],
     * }
     * }
     *
     * @param timeSeries A time series map containing the asset name and measure IRIs required.
     * @return A map: {assetType: {assets:[asset name list], measure[[measureDetails],[measureDetails]]}}.
     */
    protected Map<String, Map<String, List<String[]>>> getMeasureColAndTableName(Map<String, Queue<String[]>> timeSeries) {
        // Initialise a queue to store all results across database
        Queue<String[]> postGisResults = new ArrayDeque<>();
        // Parse time series here so that it is only executed once rather than every loop
        String[] measureIris = parseTimeSeriesMapForQuery(timeSeries);
        // Connect to all existing database and attempt to retrieve the required metadata
        for (String database : this.DATABASE_LIST) {
            try (Connection conn = connect(this.getJdbc(database), this.STACK_POSTGIS_USER, this.STACK_POSTGIS_PASSWORD)) {
                Queue<String[]> postGisData = this.retrieveAllColAndTableNames(conn, measureIris);
                // If there are results, store them into the overall queue for further processing
                // This step is required as data might be stored on different databases for different assets
                // If we ignore this step, only results from one database is stored
                postGisResults.addAll(postGisData);
            } catch (SQLException e) {
                LOGGER.fatal("Error connecting to database at: " + database + " :" + e);
                throw new JPSRuntimeException("Error connecting to database at: " + database + " :" + e);
            }
        }
        return processQueryResultsAsNestedMap(postGisResults);
    }

    /**
     * Connect to the specified PostgreSQL database.
     *
     * @param jdbcUrl  The database JDBC url.
     * @param user     The database's username.
     * @param password The database's password.
     * @return the database Connection object.
     */
    private Connection connect(String jdbcUrl, String user, String password) throws SQLException {
        try {
            Class.forName("org.postgresql.Driver");
        } catch (ClassNotFoundException e) {
            LOGGER.fatal(e);
            throw new JPSRuntimeException(e);
        }
        return DriverManager.getConnection(jdbcUrl, user, password);
    }

    /**
     * Retrieves all the database names from the default 'postgres' database, excluding the defaults.
     * The results will be stored in this client and can be retrieved using getDatabaseNames().
     */
    private void retrieveAllDatabaseNames(Connection conn) {
        try (Statement stmt = conn.createStatement()) {
            // Retrieve all available database names
            String listDatabaseQuery = "SELECT datname FROM pg_database\n" +
                    // Excluding default databases that does not store data in our workflow and are irrelevant
                    "WHERE datname NOT IN ('postgres', 'template_postgis', 'template1', 'template0');";
            ResultSet columnsResultSet = stmt.executeQuery(listDatabaseQuery);
            // When there is a next row available,
            while (columnsResultSet.next()) {
                // Append each row's value for their database name
                this.DATABASE_LIST.add(columnsResultSet.getString(1));
            }
        } catch (SQLException e) {
            LOGGER.fatal("Failed to retrieve all database names available! " + e);
            throw new JPSRuntimeException("Failed to retrieve all database names available! " + e);
        }
    }

    /**
     * Parses the time series into the query format required for two CASE WHEN variables and one matching table query.
     *
     * @param timeSeries The time series mapping each asset to their list of measure and time series. The String[] = measureName, dataIRI, timeseriesIRI.
     * @return An array of query syntax as a String. Within the array, first position is the assetVariable; Second position is the measureVariable; Third position is assetTypeVariable; Forth position is matchingTable.
     */
    private String[] parseTimeSeriesMapForQuery(Map<String, Queue<String[]>> timeSeries) {
        // Initialise require objects to put values in
        String[] querySyntax = new String[4];
        StringBuilder assetCaseWhenValues = new StringBuilder();
        StringBuilder measureCaseWhenValues = new StringBuilder();
        StringBuilder assetTypeCaseWhenValues = new StringBuilder();
        StringBuilder matchingTableValues = new StringBuilder();
        // For each asset
        for (String asset : timeSeries.keySet()) {
            // Retrieve all their measureIRIs
            Queue<String[]> measureIRIs = timeSeries.get(asset);
            // While the queue isn't empty, retrieve and remove the first time series IRI set
            // Parse them according to the desired format
            while (!measureIRIs.isEmpty()) {
                String[] timeSeriesIRIs = measureIRIs.poll();
                // For the asset case when values, attach the right asset name to the right combination
                assetCaseWhenValues.append("WHEN \"dataIRI\" = '").append(timeSeriesIRIs[1])
                        .append("' AND \"timeseriesIRI\"= '").append(timeSeriesIRIs[2])
                        .append("' THEN '").append(asset).append("'");
                // For the measure case when values, attach the right measure name to the right combination
                measureCaseWhenValues.append("WHEN \"dataIRI\" = '").append(timeSeriesIRIs[1])
                        .append("' AND \"timeseriesIRI\"= '").append(timeSeriesIRIs[2])
                        .append("' THEN '").append(timeSeriesIRIs[0]).append("'");
                // For the asset type case when values, attach the right asset type to the right combination
                assetTypeCaseWhenValues.append("WHEN \"dataIRI\" = '").append(timeSeriesIRIs[1])
                        .append("' AND \"timeseriesIRI\"= '").append(timeSeriesIRIs[2])
                        .append("' THEN '").append(timeSeriesIRIs[3]).append("'");
                // For the matching table values,
                // Only append comma if there are already existing values
                if (matchingTableValues.length() != 0) matchingTableValues.append(", ");
                // Enclose dataIRI and time series IRI together in brackets
                matchingTableValues.append("('").append(timeSeriesIRIs[1]).append("', '")
                        .append(timeSeriesIRIs[2]).append("') ");
            }
        }
        // Once added, attach the right syntax to the final array returned
        querySyntax[0] = assetCaseWhenValues.toString();
        querySyntax[1] = measureCaseWhenValues.toString();
        querySyntax[2] = assetTypeCaseWhenValues.toString();
        querySyntax[3] = matchingTableValues.toString();
        return querySyntax;
    }

    /**
     * Retrieves all the column and table names from the dbTable in the selected database.
     *
     * @param conn               A connection object to the required database.
     * @param measureQuerySyntax An array containing three query syntax to be appended to the query.
     * @return A list of the required column and table names mapped to their asset and measures. Position 0 - measure name; Position 1 - asset name; Position 2 - asset type; Position 3 - column name; Position 4 - table name.
     */
    private Queue<String[]> retrieveAllColAndTableNames(Connection conn, String[] measureQuerySyntax) {
        Queue<String[]> results = new ArrayDeque<>();
        try (Statement stmt = conn.createStatement()) {
            // Retrieve only from dbTable of each database and try to find certain info if available
            String retrieveColAndTableNameQuery = "SELECT \"columnName\", \"tableName\", " +
                    // Add a third variable to map asset name
                    "CASE " + measureQuerySyntax[0] + "ELSE 'Unknown asset' END AS asset, " +
                    // Add a forth variable to map measure
                    "CASE " + measureQuerySyntax[1] + "ELSE 'Unknown asset' END AS measure, " +
                    // Add a fifth variable to map asset type
                    "CASE " + measureQuerySyntax[2] + "ELSE 'Unknown asset' END AS assettype " +
                    // Table should be fixed to dbTable
                    "FROM \"dbTable\" " +
                    // Create a new table containing the values that we wish to filter for
                    "JOIN (VALUES " + measureQuerySyntax[3] +
                    ") AS matches (measure, timeseries) " +
                    "ON \"dbTable\".\"dataIRI\"= matches.measure AND \"dbTable\".\"timeseriesIRI\"= matches.timeseries";
            ResultSet rowResultSet = stmt.executeQuery(retrieveColAndTableNameQuery);
            // When there is a next row available in the result,
            while (rowResultSet.next()) {
                // Retrieve the necessary values and append it into the queue
                String[] rowValues = new String[5];
                rowValues[0] = rowResultSet.getString("measure");
                rowValues[1] = rowResultSet.getString("asset");
                rowValues[2] = rowResultSet.getString("assettype");
                rowValues[3] = rowResultSet.getString("columnName");
                rowValues[4] = rowResultSet.getString("tableName");
                results.offer(rowValues);
            }
        } catch (SQLException e) {
            // In the code, we should ignore any error message arising from missing dbTable
            // Without a dbTable, the database is probably for other non-time series uses and should be ignored without breaking the code
            // But otherwise, every other error should stop the code
            if (!e.getMessage().contains("ERROR: relation \"dbTable\" does not exist")) {
                LOGGER.fatal("Failed to retrieve column and table names available! " + e);
                throw new JPSRuntimeException("Failed to retrieve column and table names available! " + e);
            }
        }
        return results;
    }

    /**
     * Processes the query results into the required nested map structure so that it is easier for the Dashboard client to parse:
     * { assetType1: {
     * assets: [AssetName1, AssetName2, AssetName3],
     * measure1: [[AssetName1, ColName1, TableName1],[AssetName2, ColName2, TableName1],[AssetName3, ColName3, TableName1]],
     * measure2: [[AssetName1, ColName5, TableName1],[AssetName2, ColName6, TableName1],[AssetName3, ColName7, TableName1]],
     * },
     * assetType2: {
     * assets: [AssetName5, AssetName6, AssetName7],
     * measure1: [[AssetName5, ColName1, TableName1],[AssetName6, ColName2, TableName1],[AssetName7, ColName3, TableName1]],
     * measure2: [[AssetName5, ColName5, TableName1],[AssetName6, ColName6, TableName1],[AssetName7, ColName7, TableName1]],
     * }
     * }
     *
     * @param postGisResults The postGIS query results that has been stored as a queue containing all row values.
     * @return The required map structure {assetType: {assets:[asset name list], measure[[measureDetails],[measureDetails]]}}.
     */
    private Map<String, Map<String, List<String[]>>> processQueryResultsAsNestedMap(Queue<String[]> postGisResults) {
        Map<String, Map<String, List<String[]>>> results = new HashMap<>();
        // While there are still results
        while (!postGisResults.isEmpty()) {
            String[] assetMetadata = postGisResults.poll();
            String measureKey = assetMetadata[0];
            String assetName = assetMetadata[1];
            String assetTypeKey = assetMetadata[2];
            String columnName = assetMetadata[3];
            String tableName = assetMetadata[4];
            // If the asset type does not exist in the map,
            if (!results.containsKey(assetTypeKey)) {
                // Initialise a new hashmap containing only one key-value pair to link the asset names to their type
                Map<String, List<String[]>> measureMap = new HashMap<>();
                // Asset key will be consistently available to make it easier to link asset names to their type
                measureMap.put(ASSET_KEY, new ArrayList<>());
                results.put(assetTypeKey, measureMap);
            }
            // Retrieve either an existing or newly created map with the asset type key
            Map<String, List<String[]>> measureMap = results.get(assetTypeKey);
            // Add the asset name directly to the asset list
            measureMap.get(ASSET_KEY).add(new String[]{assetName});
            // If the measure specified does not exist in the map, initialise a new empty list
            if (!measureMap.containsKey(measureKey)) measureMap.put(measureKey, new ArrayList<>());
            // Add the required measure metadata into the list
            measureMap.get(measureKey).add(new String[]{assetName, columnName, tableName});
        }
        return results;
    }
}
