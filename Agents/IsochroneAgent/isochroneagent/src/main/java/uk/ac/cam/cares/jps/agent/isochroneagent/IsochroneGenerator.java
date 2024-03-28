package uk.ac.cam.cares.jps.agent.isochroneagent;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;

public class IsochroneGenerator {
    /**
     * Generates isochrone based on time threshold, time interval, transportmode and
     * road conditions.
     * It derives TransportMode, RoadCondition from the .sql file name, edgeTableSQL
     * from its content.
     * 
     * @param remoteRDBStoreClient
     * @param upperTimeLimit       Maximum time limit of the multiple isochrones.
     * @param timeInterval         Time increments between each isochrone
     * @param EdgesTableSQLMap     The transport mode and road conditions to be used
     *                             for isochrone calculations
     */
    public void generateIsochrone(RemoteRDBStoreClient remoteRDBStoreClient, int upperTimeLimit, int timeInterval,
            Map<String, String> EdgesTableSQLMap) {

        generateTable(remoteRDBStoreClient);

        List<String> poiTypes;
        try {
            // Initilize poiTypes and get distinct poiType;
            poiTypes = getPoiTypes(remoteRDBStoreClient);

            // Outer loop: Run createIsochrone method for each poiType
            for (String poiType : poiTypes) {
                // Inner Loop: Run createIsochrone method for each transport mode

                for (Map.Entry<String, String> entry : EdgesTableSQLMap.entrySet()) {
                    // Get transport mode and replace the .sql with empty spaces
                    String strippedFileExtension = entry.getKey().replaceAll("\\.sql$", "");
                    ;

                    String[] parts = strippedFileExtension.split("_");
                    if (parts.length == 2) {
                        String transportMode = parts[0];
                        String roadCondition = parts[1];
                        String edgeTable = entry.getValue();

                        // Process each entry (key and value) as needed
                        createIsochrone(remoteRDBStoreClient, upperTimeLimit, timeInterval, transportMode,
                                roadCondition, edgeTable, poiType);

                    } else {
                        System.out.println("The input string is not in the right format.");
                    }
                }

            }

            smoothIsochrone(remoteRDBStoreClient);

        } catch (SQLException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
    }

    /**
     * Create isochrone_aggregated table to store all final isochrone.
     * 
     * @param remoteRDBStoreClient
     */
    private void generateTable(RemoteRDBStoreClient remoteRDBStoreClient) {
        try (Connection connection = remoteRDBStoreClient.getConnection()) {
            // Read SQL commands from file
            InputStream is = getClass().getResourceAsStream("create_isochrone_aggregated.sql");
            BufferedReader reader = new BufferedReader(new InputStreamReader(is));
            String line;
            while ((line = reader.readLine()) != null) {
                executeSql(connection, line);
            }
            System.out.println("Isochrone_aggregated table created. Calculating isochrones based on POI now.");
        } catch (Exception e) {
            e.printStackTrace();
            throw new JPSRuntimeException(e);
        }
    }

    /**
     * Perform isochrone calculations based on the multiple point of interests,
     * upperTimeLimit of isochrones, time interval between isocrhones, edgeTable.
     * 
     * @param remoteRDBStoreClient
     * @param upperTimeLimit
     * @param timeInterval         Time interval between isochrones
     * @param transportMode        Transport mode used to calculate isochrones
     * @param roadCondition        Road conditions of the isochrones
     * @param edgeTable            EdgeTable contains the cost table of the road
     *                             data
     * @param poiType              Different points of interest
     */
    private void createIsochrone(RemoteRDBStoreClient remoteRDBStoreClient, int upperTimeLimit, int timeInterval,
            String transportMode, String roadCondition, String edgeTable, String poiType) {

        InputStream is = getClass().getResourceAsStream("isochrone_function.sql");
        BufferedReader reader = new BufferedReader(new InputStreamReader(is));
        String line;

        try (Connection connection = remoteRDBStoreClient.getConnection()) {
            while ((line = reader.readLine()) != null) {
                executeSql(connection, line);
            }
            System.out.println("Isochrones added to tables");
        } catch (Exception e) {
            e.printStackTrace();
            throw new JPSRuntimeException(e);
        }

    }

    /**
     * Retrieve the different POI types.
     * 
     * @param remoteRDBStoreClient
     * @return
     * @throws SQLException
     */
    public List<String> getPoiTypes(RemoteRDBStoreClient remoteRDBStoreClient) throws SQLException {

        try (InputStream is = getClass().getResourceAsStream("get_nearest_node.sql");
                BufferedReader reader = new BufferedReader(new InputStreamReader(is));) {
            StringBuilder sb = new StringBuilder();
            String line;
            while ((line = reader.readLine()) != null) {
                sb.append(line);
                sb.append(System.lineSeparator());
            }
            String getNearestNodeSQL = sb.toString();

            List<String> poiTypes = new ArrayList<>();

            try (Connection connection = remoteRDBStoreClient.getConnection();
                    Statement statement = connection.createStatement();
                    ResultSet resultSet = statement.executeQuery(getNearestNodeSQL)) {

                while (resultSet.next()) {
                    poiTypes.add(resultSet.getString("poi_type"));
                }
            } catch (SQLException e) {
                throw new JPSRuntimeException(e);
            }

            return poiTypes;
        } catch (IOException e) {
            throw new JPSRuntimeException("Failed to read get_nearest_node.sql", e);
        }
    }

    /**
     * Create connection to remoteStoreClient and execute SQL statement
     * 
     * @param connection PostgreSQL connection object
     * @param sql        SQl statement to execute
     */
    private void executeSql(Connection connection, String sql) throws SQLException {
        try (Statement statement = connection.createStatement()) {
            statement.execute(sql);
        }
    }

    /**
     * Smoothen the hard corners of all isochrones using ChaikinSmoothing.
     * 
     * @param remoteRDBStoreClient
     */
    private void smoothIsochrone(RemoteRDBStoreClient remoteRDBStoreClient) {

        InputStream is = getClass().getResourceAsStream("smooth_isochrone.sql");

        BufferedReader reader = new BufferedReader(new InputStreamReader(is));
        String line;

        try (Connection connection = remoteRDBStoreClient.getConnection()) {
            while ((line = reader.readLine()) != null) {
                executeSql(connection, line);
            }
            System.out.println("Isochrone_aggregated smoothened.");
        } catch (Exception e) {
            e.printStackTrace();
            throw new JPSRuntimeException(e);
        }

    }

    /**
     * Create reference table to indicate each isochrone is originated by which
     * poi_iri.
     * 
     * @param remoteRDBStoreClient
     */
    public void createIsochroneBuilding(RemoteRDBStoreClient remoteRDBStoreClient) {

        InputStream is = getClass().getResourceAsStream("create_isochrone_building_table.sql");

        BufferedReader reader = new BufferedReader(new InputStreamReader(is));
        String line;

        try (Connection connection = remoteRDBStoreClient.getConnection()) {
            while ((line = reader.readLine()) != null) {
                executeSql(connection, line);
            }
            System.out.println("isochrone_building_ref table created.");
        } catch (Exception e) {
            e.printStackTrace();
            throw new JPSRuntimeException(e);
        }
    }

}
