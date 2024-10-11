package uk.ac.cam.cares.jps.agent.dashboard.stack;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import uk.ac.cam.cares.jps.agent.dashboard.datamodel.Measure;
import uk.ac.cam.cares.jps.agent.dashboard.datamodel.Organisation;
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
     * Retrieve the column and table name corresponding with the item type and measure from the PostGIS database.
     * Results will be stored into the organisation data model.
     *
     * @param organisation A time series map containing the item name and measure IRIs required.
     */
    protected void retrieveMeasureRDBLocation(Organisation organisation) {
        // Parse time series here so that it is only executed once rather than every loop
        String[] sqlQueries = this.parseFacilityTimeSeriesForQuery(organisation);
        // Store the sql query generated into a list, which will be in sequence of facility results
        // Connect to all existing database and attempt to retrieve the required metadata
        for (String database : this.databaseList) {
            try (Connection conn = connect(this.getJdbc(database))) {
                this.retrieveAllColAndTableNames(conn, database, sqlQueries, organisation);
            } catch (SQLException e) {
                LOGGER.fatal("Error connecting to database at: {} :", database, e);
                throw new JPSRuntimeException(MessageFormat.format("Error connecting to database at: {0} :", database), e);
            }
        }
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
     * Parses all time series within the organisation into the required query format of CASE WHEN variables and a matching table query.
     *
     * @param organisation          The organisation's time series data model.
     * @return An array of query syntax. Within the array, first position is the facilityVariable; Second position is the itemVariable; Third position is the measureVariable; Forth position is itemGroupVariable; Fifth position is matchingTable.
     */
    private String[] parseFacilityTimeSeriesForQuery(Organisation organisation) {
        // Initialise require objects to put values in
        String[] querySyntax = new String[5];
        StringBuilder facilityCaseWhenValues = new StringBuilder();
        StringBuilder itemCaseWhenValues = new StringBuilder();
        StringBuilder measureCaseWhenValues = new StringBuilder();
        StringBuilder itemGroupCaseWhenValues = new StringBuilder();
        StringBuilder matchingTableValues = new StringBuilder();
        String caseWhenTemplate = "WHEN \"dataIRI\"='%s' AND \"timeseriesIRI\"='%s' THEN '%s' ";
        List<String> itemGroups = organisation.getAllItemGroups();
        // Parse all item groups and measures within the facility into the require SQL query format
        itemGroups.forEach(itemGroup ->{
            Set<String> measures = organisation.getAllMeasureNames(itemGroup);
            measures.forEach(measure -> {
                Measure currentMeasureData = organisation.getMeasure(itemGroup, measure);
                Queue<String[]> timeSeriesIris = currentMeasureData.getAllTimeSeriesIris();
                while (!timeSeriesIris.isEmpty()){
                    String[] timeSeriesMetadata = timeSeriesIris.poll();
                    // For the matching table values, enclose dataIRI and time series IRI together in brackets
                    if (matchingTableValues.length() != 0) matchingTableValues.append(", ");
                    matchingTableValues.append("('").append(timeSeriesMetadata[1]).append("', '")
                            .append(timeSeriesMetadata[2]).append("') ");
                    itemCaseWhenValues.append(String.format(caseWhenTemplate, timeSeriesMetadata[1], timeSeriesMetadata[2], timeSeriesMetadata[0]));
                    measureCaseWhenValues.append(String.format(caseWhenTemplate, timeSeriesMetadata[1], timeSeriesMetadata[2], currentMeasureData.getName()));
                    itemGroupCaseWhenValues.append(String.format(caseWhenTemplate, timeSeriesMetadata[1], timeSeriesMetadata[2], itemGroup));
                }
            });
        });
        querySyntax[0] = itemCaseWhenValues.toString();
        querySyntax[1] = measureCaseWhenValues.toString();
        querySyntax[2] = itemGroupCaseWhenValues.toString();
        querySyntax[3] = matchingTableValues.toString();
        return querySyntax;
    }

    /**
     * Retrieves all the column and table names from the dbTable in the selected database, and populate the data model accordingly.
     *
     * @param conn               A connection object to the required database.
     * @param database           The database name.
     * @param timeSeriesQuerySyntax An array containing five query syntax to be appended to the query.
     * @param organisation The organisation object that will store the time series metadata retrieved.
     */
    private void retrieveAllColAndTableNames(Connection conn, String database, String[] timeSeriesQuerySyntax, Organisation organisation) {
        try (Statement stmt = conn.createStatement()) {
            // Retrieve only from dbTable of each database and try to find certain info if available
            String queryTemplate = "SELECT \"columnName\",\"tableName\"," +
                    "CASE %s ELSE 'Unknown item' END AS item," +
                    "CASE %s ELSE 'Unknown measure' END AS measure," +
                    "CASE %s ELSE 'Unknown item group' END AS itemgroup " +
                    "FROM \"dbTable\" " +
                    "JOIN (VALUES %s) AS matches (datairi, timeseries) " +
                    "ON \"dbTable\".\"dataIRI\"= matches.datairi AND \"dbTable\".\"timeseriesIRI\"= matches.timeseries";
            String retrieveColAndTableNameQuery = String.format(queryTemplate, timeSeriesQuerySyntax[0],
                    timeSeriesQuerySyntax[1], timeSeriesQuerySyntax[2],timeSeriesQuerySyntax[3]);
            ResultSet rowResultSet = stmt.executeQuery(retrieveColAndTableNameQuery);
            // When there is a next row available in the result,
            while (rowResultSet.next()) {
                String itemName = rowResultSet.getString("item");
                String measure = rowResultSet.getString("measure");
                String itemGroup = rowResultSet.getString("itemgroup");
                String columnName = rowResultSet.getString("columnName");
                String tableName = rowResultSet.getString("tableName");
                organisation.addTimeSeries(itemGroup, measure, itemName, columnName, tableName, database);
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
    }
}
