package uk.ac.cam.cares.jps.agent.dashboard.stack;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import uk.ac.cam.cares.jps.agent.dashboard.utils.StringHelper;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;

import java.sql.*;
import java.text.MessageFormat;
import java.util.*;

/**
 * A client that provides method to interact and store information from the PostGIS container within a stack.
 *
 * @author qhouyee
 */
public class PostGisClient {
    private final String stackJdbcUrl;
    private final RemoteRDBStoreClient stackRdbClient;
    private final List<String> databaseList = new ArrayList<>();
    private static final String WHEN_DATA_IRI = "WHEN \"dataIRI\" = '";
    private static final String AND_TIMESERIES_IRI = "' AND \"timeseriesIRI\"= '";
    private static final String THEN_STATEMENT = "' THEN '";
    private static final Logger LOGGER = LogManager.getLogger(PostGisClient.class);

    /**
     * Standard Constructor to initialise this client.
     *
     * @param jdbcUrl The JDBC url of the stack's PostGIS container, excluding the database name.
     * @param user    The username to access the stack's PostGIS container.
     * @param pass    The password to access the stack's PostGIS container.
     */
    protected PostGisClient(String jdbcUrl, String user, String pass) {
        // Set up the required information for further interactions
        this.stackJdbcUrl = jdbcUrl;
        this.stackRdbClient = new RemoteRDBStoreClient(jdbcUrl, user, pass);
        // Retrieve all available custom database so that methods can perform federated query on each
        // Note that to retrieve all databases, this method must connect to any default database, which is postgres in this case
        try (Connection conn = connect(this.getJdbc("postgres"))) {
            this.retrieveAllDatabaseNames(conn);
        } catch (SQLException e) {
            LOGGER.fatal("Error connecting to database: ", e);
            throw new JPSRuntimeException("Error connecting to database: ", e);
        }
    }

    /**
     * Get the list of database names that is available in this stack. This method is accessible for the stack client's usage.
     */
    protected List<String> getDatabaseNames() {
        return this.databaseList;
    }

    /**
     * Get the PostGIS username.
     */
    protected String getUsername() {
        return this.stackRdbClient.getUser();
    }

    /**
     * Get the PostGIS password.
     */
    protected String getPassword() {
        return this.stackRdbClient.getPassword();
    }

    /**
     * Get the JDBC URL of a specified database within this stack.
     *
     * @param database The database of interest in the stack's RDB.
     * @return The JDBC URL of the database specified.
     */
    protected String getJdbc(String database) {
        return this.stackJdbcUrl + database;
    }

    /**
     * Get the column and table name corresponding with the item type and measure from the PostGIS database.
     * The returned map will have the following structure:
     * { facilities:{
     * facility1: [[RoomName1], [AssetName1], [AssetName2], [AssetName6]],
     * facility2: [[RoomName2], [AssetName3], [AssetName4], [AssetName5], [AssetName7]],
     * },
     * assetType1: {
     * assets: [AssetName1, AssetName2, AssetName3],
     * measure1: [[AssetName1, ColName1, TableName1, Database, unit],[AssetName2, ColName2, TableName1, Database, unit],[AssetName3, ColName3, TableName1, Database, unit]],
     * measure2: [[AssetName1, ColName5, TableName1, Database, unit],[AssetName2, ColName6, TableName1, Database, unit],[AssetName3, ColName7, TableName1, Database, unit]],
     * },
     * assetType2: {
     * assets: [AssetName5, AssetName6, AssetName7],
     * measure1: [[AssetName5, ColName1, TableName1, Database, unit],[AssetName6, ColName2, TableName1, Database, unit],[AssetName7, ColName3, TableName1, Database, unit]],
     * measure2: [[AssetName5, ColName5, TableName1, Database, unit],[AssetName6, ColName6, TableName1, Database, unit],[AssetName7, ColName7, TableName1, Database, unit]],
     * },
     * rooms:{
     * rooms: [[RoomName1], [RoomName2]],
     * thresholds: [[measure1, minThrehold, maxThreshold]],
     * measure1: [[RoomName1, ColName1, TableName2, Database, unit],[RoomName2, ColName3, TableName2, Database, unit]],
     * measure2: [[RoomName1, ColName2, TableName2, Database, unit],[RoomName2, ColName4, TableName2, Database, unit]]
     * }
     * }
     *
     * @param timeSeries A time series map containing the item name and measure IRIs required.
     * @return A map: {facilities: {facility1: [items list]}, assetType: {assets:[asset name list], measure:[[measureDetails],[measureDetails]]}, rooms:{rooms:[room name list], thresholds[thresholdList], measure: [measureDetails}}.
     */
    protected Map<String, Map<String, List<String[]>>> getMeasureColAndTableName(Map<String, Queue<String[]>> timeSeries) {
        // Initialise a queue to store all results across database
        Queue<String[]> postGisResults = new ArrayDeque<>();
        Queue<String[]> thresholds = new ArrayDeque<>(); // for the thresholds
        // If there are thresholds available, replace the empty queue
        if (timeSeries.containsKey(StringHelper.THRESHOLD_KEY)) {
            // Retrieve the thresholds and remove it immediately to prevent it from corrupting the workflow
            thresholds = timeSeries.get(StringHelper.THRESHOLD_KEY);
            timeSeries.remove(StringHelper.THRESHOLD_KEY);
        }
        // Retrieve the facility and their associated items and remove it immediately to prevent it from corrupting the workflow
        Queue<String[]> facilities = timeSeries.get(StringHelper.FACILITY_KEY);
        timeSeries.remove(StringHelper.FACILITY_KEY);
        // Parse time series here so that it is only executed once rather than every loop
        String[] measureIris = parseTimeSeriesMapForQuery(timeSeries);
        // Connect to all existing database and attempt to retrieve the required metadata
        for (String database : this.databaseList) {
            try (Connection conn = connect(this.getJdbc(database))) {
                Queue<String[]> postGisData = this.retrieveAllColAndTableNames(conn, database, measureIris);
                // If there are results, store them into the overall queue for further processing
                // This step is required as data might be stored on different databases for different assets
                // If we ignore this step, only results from one database is stored
                postGisResults.addAll(postGisData);
            } catch (SQLException e) {
                LOGGER.fatal("Error connecting to database at: {} :", database, e);
                throw new JPSRuntimeException(MessageFormat.format("Error connecting to database at: {0} :", database), e);
            }
        }
        return processQueryResultsAsNestedMap(postGisResults, facilities, thresholds);
    }

    /**
     * Connect to the specified PostgreSQL database.
     *
     * @param jdbcUrl  The database JDBC url.
     * @return the database Connection object.
     */
    private Connection connect(String jdbcUrl) throws SQLException {
        // The remote client can only get connection based on the jdbc url set, so we set this parameter each time
        this.stackRdbClient.setRdbURL(jdbcUrl);
        return this.stackRdbClient.getConnection();
    }

    /**
     * Retrieves all the database names from the default 'postgres' database, excluding the defaults.
     * The results will be stored in this client and can be retrieved using getDatabaseNames().
     *
     * @param conn A connection object to the required database.
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
                this.databaseList.add(columnsResultSet.getString(1));
            }
        } catch (SQLException e) {
            LOGGER.fatal("Failed to retrieve all database names available! ", e);
            throw new JPSRuntimeException("Failed to retrieve all database names available! ", e);
        }
    }

    /**
     * Parses the time series into the query format required for four CASE WHEN variables and one matching table query.
     *
     * @param timeSeries The time series mapping each asset and room to their list of measure and time series. The String[] = measureName, dataIRI, timeseriesIRI.
     * @return An array of query syntax as a String. Within the array, first position is the roomOrAssetVariable; Second position is the measureVariable; Third position is assetTypeVariable; Forth position is unit; Fifth position is matchingTable.
     */
    private String[] parseTimeSeriesMapForQuery(Map<String, Queue<String[]>> timeSeries) {
        // Initialise require objects to put values in
        String[] querySyntax = new String[5];
        StringBuilder roomOrAssetCaseWhenValues = new StringBuilder();
        StringBuilder measureCaseWhenValues = new StringBuilder();
        StringBuilder assetTypeCaseWhenValues = new StringBuilder();
        StringBuilder unitCaseWhenValues = new StringBuilder();
        StringBuilder matchingTableValues = new StringBuilder();
        // For each asset or room
        for (Map.Entry<String, Queue<String[]>> entry : timeSeries.entrySet()) {
            String roomOrAsset = entry.getKey();
            Queue<String[]> measureIRIs = entry.getValue();
            // While the queue isn't empty, retrieve and remove the first time series IRI set
            // Parse them according to the desired format
            while (!measureIRIs.isEmpty()) {
                String[] timeSeriesIRIs = measureIRIs.poll();
                // For the matching table values,
                if (matchingTableValues.length() != 0)
                    matchingTableValues.append(", "); // Only append comma if there are already existing values
                // Enclose dataIRI and time series IRI together in brackets
                matchingTableValues.append("('").append(timeSeriesIRIs[1]).append("', '")
                        .append(timeSeriesIRIs[2]).append("') ");
                // For the room and asset case when values, attach the right asset or room name to the right combination
                roomOrAssetCaseWhenValues.append(WHEN_DATA_IRI).append(timeSeriesIRIs[1])
                        .append(AND_TIMESERIES_IRI).append(timeSeriesIRIs[2])
                        // Ensure that there is a backslash behind quotes to escape the name
                        .append(THEN_STATEMENT).append(StringHelper.addCharacterEscapingForSingleQuotes(roomOrAsset)).append("'");
                // For the measure case when values, attach the right measure name to the right combination
                measureCaseWhenValues.append(WHEN_DATA_IRI).append(timeSeriesIRIs[1])
                        .append(AND_TIMESERIES_IRI).append(timeSeriesIRIs[2])
                        .append(THEN_STATEMENT).append(timeSeriesIRIs[0]).append("'");
                // For the unit case when values, attach the unit if it exists. Otherwise, attach null. Not all measures have units
                unitCaseWhenValues.append(WHEN_DATA_IRI).append(timeSeriesIRIs[1])
                        .append(AND_TIMESERIES_IRI).append(timeSeriesIRIs[2])
                        .append(THEN_STATEMENT).append(timeSeriesIRIs[3]).append("'");
                // For the asset type case when values, only attach the right asset type to the right combination
                // Rooms will have an asset type of rooms
                assetTypeCaseWhenValues.append(WHEN_DATA_IRI).append(timeSeriesIRIs[1])
                        .append(AND_TIMESERIES_IRI).append(timeSeriesIRIs[2])
                        .append(THEN_STATEMENT).append(timeSeriesIRIs[4]).append("'");
            }
        }
        // Once added, attach the right syntax to the final array returned
        querySyntax[0] = roomOrAssetCaseWhenValues.toString();
        querySyntax[1] = measureCaseWhenValues.toString();
        querySyntax[2] = assetTypeCaseWhenValues.toString();
        querySyntax[3] = unitCaseWhenValues.toString();
        querySyntax[4] = matchingTableValues.toString();
        return querySyntax;
    }

    /**
     * Retrieves all the column and table names from the dbTable in the selected database.
     *
     * @param conn               A connection object to the required database.
     * @param database           The database name.
     * @param measureQuerySyntax An array containing five query syntax to be appended to the query.
     * @return A list of the required column and table names mapped to their asset and rooms alongside their measures. Position 0 - measure name; Position 1 - asset or room name; Position 2 - asset type if available; Position 3 - measure unit; Position 4 - column name; Position 5 - table name; Position 6 - database name.
     */
    private Queue<String[]> retrieveAllColAndTableNames(Connection conn, String database, String[] measureQuerySyntax) {
        Queue<String[]> results = new ArrayDeque<>();
        // Exit the method if no measures are available
        if (measureQuerySyntax[0].isEmpty()) return results;
        // Continue the method if there are measures to process
        try (Statement stmt = conn.createStatement()) {
            // Retrieve only from dbTable of each database and try to find certain info if available
            String retrieveColAndTableNameQuery = "SELECT \"columnName\", \"tableName\", " +
                    // Add a third variable to map asset or room name
                    "CASE " + measureQuerySyntax[0] + "ELSE 'Unknown item' END AS item, " +
                    // Add a forth variable to map measure
                    "CASE " + measureQuerySyntax[1] + "ELSE 'Unknown measure' END AS measure, ";
            // Add a fifth variable to map asset type
            retrieveColAndTableNameQuery += "CASE " + measureQuerySyntax[2] + "ELSE 'Unknown asset type' END AS assettype, ";
            // Add a sixth variable to map units
            retrieveColAndTableNameQuery += "CASE " + measureQuerySyntax[3] + "ELSE '' END AS unit " +
                    // Table should be fixed to dbTable
                    "FROM \"dbTable\" " +
                    // Create a new table containing the values that we wish to filter for
                    "JOIN (VALUES " + measureQuerySyntax[4] +
                    ") AS matches (measure, timeseries) " +
                    "ON \"dbTable\".\"dataIRI\"= matches.measure AND \"dbTable\".\"timeseriesIRI\"= matches.timeseries";
            ResultSet rowResultSet = stmt.executeQuery(retrieveColAndTableNameQuery);
            // When there is a next row available in the result,
            while (rowResultSet.next()) {
                // Retrieve the necessary values and append it into the queue
                String[] rowValues = new String[7];
                rowValues[0] = rowResultSet.getString("measure");
                rowValues[1] = rowResultSet.getString("item");
                // Only retrieve asset type if it has been queried, as this spatial zone may not have any assets
                if (measureQuerySyntax[2] != null) rowValues[2] = rowResultSet.getString("assettype");
                rowValues[3] = rowResultSet.getString("unit");
                rowValues[4] = rowResultSet.getString("columnName");
                rowValues[5] = rowResultSet.getString("tableName");
                // If there are results returned from this database, attach this metadata on it
                rowValues[6] = database;
                results.offer(rowValues);
            }
        } catch (SQLException e) {
            // In the code, we should ignore any error message arising from missing dbTable
            // Without a dbTable, the database is probably for other non-time series uses and should be ignored without breaking the code
            // But otherwise, every other error should stop the code
            if (!e.getMessage().contains("ERROR: relation \"dbTable\" does not exist")) {
                LOGGER.fatal("Failed to retrieve column and table names available! ", e);
                throw new JPSRuntimeException("Failed to retrieve column and table names available! ", e);
            }
        }
        return results;
    }

    /**
     * Processes the query results into the required nested map structure so that it is easier for the Dashboard client to parse:
     * { facilities:{
     * facility1: [[RoomName1], [AssetName1], [AssetName2], [AssetName6]],
     * facility2: [[RoomName2], [AssetName3], [AssetName4], [AssetName5], [AssetName7]],
     * }
     * assetType1: {
     * assets: [AssetName1, AssetName2, AssetName3],
     * measure1: [[AssetName1, ColName1, TableName1, Database, unit],[AssetName2, ColName2, TableName1, Database, unit],[AssetName3, ColName3, TableName1, Database, unit]],
     * measure2: [[AssetName1, ColName5, TableName1, Database, unit],[AssetName2, ColName6, TableName1, Database, unit],[AssetName3, ColName7, TableName1, Database, unit]],
     * },
     * assetType2: {
     * assets: [AssetName5, AssetName6, AssetName7],
     * measure1: [[AssetName5, ColName1, TableName1, Database, unit],[AssetName6, ColName2, TableName1, Database, unit],[AssetName7, ColName3, TableName1, Database, unit]],
     * measure2: [[AssetName5, ColName5, TableName1, Database, unit],[AssetName6, ColName6, TableName1, Database, unit],[AssetName7, ColName7, TableName1, Database, unit]],
     * },
     * Rooms:{
     * thresholds: [[measure1, minThreshold, maxThreshold],[measure2, minThreshold, maxThreshold]],
     * Rooms: [RoomName1, RoomName2],
     * measure1: [[RoomName1, ColName1, TableName2, Database, unit],[RoomName2, ColName3, TableName2, Database, unit]],
     * measure2: [[RoomName1, ColName2, TableName2, Database, unit],[RoomName2, ColName4, TableName2, Database, unit]]
     * },
     * systems:{
     * systems: [System1, Subsystem2],
     * measure1: [[System1, ColName1, TableName2, Database, unit],[Subsystem2, ColName3, TableName2, Database, unit]],
     * measure2: [[System1, ColName2, TableName2, Database, unit],[Subsystem2, ColName4, TableName2, Database, unit]]
     * }
     * }
     *
     * @param postGisResults The postGIS query results that has been stored as a queue containing all row values.
     * @param facilities     A queue containing an array of the facility and their associated rooms or assets.
     * @param thresholds     A queue containing an array of the threshold metadata.
     * @return The required map structure {facilities: {facility1: [items list]}, assetType: {assets:[asset name list], measure: [[measureDetails],[measureDetails]]}, room : {thresholds[thresholdList], measure: [[measureDetails],[measureDetails]]}}.
     */
    private Map<String, Map<String, List<String[]>>> processQueryResultsAsNestedMap(Queue<String[]> postGisResults, Queue<String[]> facilities, Queue<String[]> thresholds) {
        Map<String, Map<String, List<String[]>>> results = new HashMap<>();
        // While there are still results
        while (!postGisResults.isEmpty()) {
            String[] metadata = postGisResults.poll();
            String measureKey = metadata[0];
            String assetOrRoomOrSystemName = metadata[1];
            String itemType = metadata[2];
            String unit = metadata[3];
            String columnName = metadata[4];
            String tableName = metadata[5];
            String database = metadata[6];
            Map<String, List<String[]>> measureMap;
            // Depending on whether it is a room, system, or not, set the keys accordingly
            String itemTypeKey = itemType;
            String nestedItemKey = StringHelper.ASSET_KEY;
            if (itemType.equals(StringHelper.ROOM_KEY)) {
                itemTypeKey = StringHelper.ROOM_KEY;
                nestedItemKey = StringHelper.ROOM_KEY;
            } else if (itemType.equals(StringHelper.SYSTEM_KEY)) {
                itemTypeKey = StringHelper.SYSTEM_KEY;
                nestedItemKey = StringHelper.SYSTEM_KEY;
            }
            // If the asset type, system, or room key does not exist in the map, initialise a new hashmap containing only one key-value pair
            String finalNestedItemKey = nestedItemKey;
            results.computeIfAbsent(itemTypeKey, measureMapValue -> {
                Map<String, List<String[]>> newMap = new HashMap<>();
                // Corresponding key will be consistently available to make it easier to link asset names to their type and find all rooms and systems
                newMap.put(finalNestedItemKey, new ArrayList<>());
                return newMap;
            });
            // Retrieve either an existing or newly created map with the corresponding asset type, system, or room key
            measureMap = results.get(itemTypeKey);
            // Add the room, system, or asset name directly to the corresponding list
            measureMap.get(nestedItemKey).add(new String[]{assetOrRoomOrSystemName});
            // If the measure specified does not exist in the map, initialise a new empty list
            measureMap.computeIfAbsent(measureKey, measureList -> new ArrayList<>());
            // Add the required measure metadata into the list
            measureMap.get(measureKey).add(new String[]{assetOrRoomOrSystemName, columnName, tableName, database, unit});
        }
        // Only add the facility mapping if there are items to map
        if (results.size() > 0) this.addFacilityItemMapping(results, facilities);
        this.addThresholds(results, thresholds);
        return results;
    }

    /**
     * Add the mapping between a facility and the asset or room it contains into the input map.
     *
     * @param metaMap    A map containing the item and their measure metadata for the dashboard.
     * @param facilities A queue containing an array of the facility and its associated assets and rooms.
     */
    private void addFacilityItemMapping(Map<String, Map<String, List<String[]>>> metaMap, Queue<String[]> facilities) {
        // Initialise a new map and list to hold information
        Map<String, List<String[]>> facilityMapping = new HashMap<>();
        // While there are still mappings to parse
        while (!facilities.isEmpty()) {
            List<String[]> currentFacilityItems = new ArrayList<>();
            // Each array contains a facility and its associated items
            String[] facility = facilities.poll();
            // Retrieve the first item, which will always be a name for the facility
            String facilityName = facility[0];
            // Stream a new array without the first item, so that it only contains the facility items and not the facility name
            String[] remainingItems = Arrays.stream(facility, 1, facility.length).toArray(String[]::new);
            // Append the values into the mapping according to the format
            currentFacilityItems.add(remainingItems);
            facilityMapping.put(facilityName, currentFacilityItems);
        }
        // Store the mappings
        metaMap.put(StringHelper.FACILITY_KEY, facilityMapping);
    }

    /**
     * Add the thresholds, typically for rooms, into the input map.
     *
     * @param metaMap    A map containing the item and their measure metadata for the dashboard.
     * @param thresholds A queue containing an array of the threshold metadata.
     */
    private void addThresholds(Map<String, Map<String, List<String[]>>> metaMap, Queue<String[]> thresholds) {
        // Only attempt this if there is more than one threshold and there are rooms
        if (!thresholds.isEmpty() && metaMap.containsKey(StringHelper.ROOM_KEY)) {
            // Retrieve the nested map for rooms
            Map<String, List<String[]>> roomMetadata = metaMap.get(StringHelper.ROOM_KEY);
            // Initialise a new list to hold information
            List<String[]> roomThresholds = new ArrayList<>();
            // Append all thresholds from the Queue into the list
            while (!thresholds.isEmpty()) {
                String[] threshold = thresholds.poll();
                roomThresholds.add(threshold);
            }
            // Store the thresholds
            roomMetadata.put(StringHelper.THRESHOLD_KEY, roomThresholds);
        }
    }
}
