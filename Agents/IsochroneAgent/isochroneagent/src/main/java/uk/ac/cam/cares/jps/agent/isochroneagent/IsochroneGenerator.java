package uk.ac.cam.cares.jps.agent.isochroneagent;

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

    String poiTableName; // default value
    String routeTableName;

    public IsochroneGenerator(String poiTableName, String routeTableName) {
        this.poiTableName = poiTableName;
        this.routeTableName = routeTableName;
    }

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
            // Initialize poiTypes and get distinct poiType;
            poiTypes = getPoiTypes(remoteRDBStoreClient);

            // Outer loop: Run createIsochrone method for each poiType
            for (String poiType : poiTypes) {
                // Inner Loop: Run createIsochrone method for each transport mode

                for (Map.Entry<String, String> entry : EdgesTableSQLMap.entrySet()) {
                    // Get transport mode and replace the .sql with empty spaces
                    String strippedFileExtension = entry.getKey().replaceAll("\\.sql$", "");

                    String[] parts = strippedFileExtension.split("_");
                    if (parts.length == 2) {
                        String transportMode = parts[0];
                        String roadCondition = parts[1];
                        String edgeTable = entry.getValue();

                        // Process each entry (key and value) as needed
                        System.out.println("Creating isochrones for the following scenario:");
                        System.out.println(
                                "Transport mode = " + transportMode + ", Road condition = " + roadCondition
                                        + ", Time limit (min) = " + upperTimeLimit +
                                        ", Time interval (min) = " + timeInterval);
                        createIsochrone(remoteRDBStoreClient, upperTimeLimit, timeInterval, transportMode,
                                roadCondition, edgeTable, poiType);

                    } else {
                        System.out.println("The input string must be in the form [TransportMode]_[RoadCondition].");
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

        String tableGeneration = "CREATE TABLE IF NOT EXISTS isochrone_aggregated (\n" +
                "    minute integer, transportmode VARCHAR, transportmode_iri VARCHAR,\n" +
                "    poi_type VARCHAR, roadcondition VARCHAR, roadcondition_iri VARCHAR,\n" +
                "    iri VARCHAR, geometry_iri VARCHAR,  geom geometry);";

        String add_uuid_ossp_Extension = "CREATE EXTENSION IF NOT EXISTS \"uuid-ossp\";";

        try (Connection connection = remoteRDBStoreClient.getConnection()) {
            executeSql(connection, add_uuid_ossp_Extension);
            executeSql(connection, tableGeneration);
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

        String isochroneFunction = "DO $$ \n" +
                "DECLARE node bigint;\n" +
                "BEGIN\n" +
                "DROP TABLE IF EXISTS eventspace_etas;\n" +
                "DROP TABLE IF EXISTS eventspace_isochrones;\n" +
                "DROP TABLE IF EXISTS isochrone_individual; \n" +
                "CREATE TABLE eventspace_etas (\n" +
                "id bigint, agg_cost double precision, the_geom geometry(PointZ, 4326));\n" +
                "CREATE TABLE eventspace_isochrones (\n" +
                "id bigint, geom geometry, transportmode VARCHAR, transportmode_iri VARCHAR,\n" +
                "roadcondition VARCHAR, roadcondition_iri VARCHAR, poi_type VARCHAR, minute integer);\n" +
                "CREATE TABLE isochrone_individual (\n" +
                "id bigint, minute integer, transportmode VARCHAR, transportmode_iri VARCHAR,\n" +
                "roadcondition VARCHAR, roadcondition_iri VARCHAR, poi_type VARCHAR, geom geometry\n);\n" +
                "FOR node IN (SELECT DISTINCT nearest_node FROM " + poiTableName + " WHERE poi_type = '" + poiType
                + "') LOOP\n" +
                "TRUNCATE TABLE eventspace_etas;\n" +
                "TRUNCATE TABLE eventspace_isochrones;\n" +
                "INSERT INTO eventspace_etas (id, agg_cost, the_geom)\n" +
                "SELECT id, dd.agg_cost,\n" +
                "ST_SetSRID(ST_MakePoint(ST_X(v.the_geom), ST_Y(v.the_geom), dd.agg_cost), 4326) AS the_geom\n" +
                "FROM pgr_drivingDistance(\n" +
                "'" + edgeTable + "', node, 10000, false) AS dd\n" + // 10000 is an arbitrary large number to ensure all
                                                                     // nodes are extracted
                "JOIN "+routeTableName+"_segment_vertices_pgr AS v ON dd.node = v.id;\n" +
                "UPDATE eventspace_etas SET the_geom = ST_SetSRID(ST_MakePoint(ST_X(the_geom), ST_Y(the_geom), agg_cost), 4326);\n"
                + "drop table if exists eventspace_delaunay;\n" +
                "create table eventspace_delaunay\n" +
                "as (select (ST_Dump(ST_DelaunayTriangles(ST_Collect(the_geom)))).geom from eventspace_etas);\n" +
                "INSERT INTO eventspace_isochrones (geom, minute)\n" +
                "SELECT ST_Union(ST_ConvexHull(ST_LocateBetweenElevations(ST_Boundary(geom), 0, minute * 60))) geom,\n"
                + "minute FROM eventspace_delaunay, generate_series(0, " + upperTimeLimit + ", " + timeInterval
                + " ) AS minute GROUP BY minute;\n" +
                "UPDATE eventspace_isochrones\n" +
                "SET transportmode = '" + transportMode + "', roadcondition = '" + roadCondition + "', poi_type = '"
                + poiType + "';\n" +
                "INSERT INTO isochrone_individual (minute, transportmode, roadcondition, poi_type, geom)\n" +
                "SELECT minute,transportmode, roadcondition, poi_type, ST_Union(geom) AS geom\n" +
                "FROM eventspace_isochrones GROUP BY minute,transportmode, roadcondition, poi_type;\n" +
                "END LOOP;\n" +
                "INSERT INTO isochrone_aggregated (minute, transportmode, roadcondition, poi_type, geom)\n" +
                "SELECT minute,transportmode, roadcondition, poi_type, ST_UNION(geom) AS geom\n" +
                "FROM isochrone_individual WHERE minute !=0 GROUP BY\n" +
                "minute,transportmode, roadcondition, poi_type;\n" +
                "WITH unique_values AS ( SELECT poi_type, minute, transportmode, roadcondition,\n" +
                "uuid_generate_v4()::text AS iri, uuid_generate_v4()::text AS geometry_iri\n" +
                "FROM isochrone_aggregated WHERE iri IS NULL\n" +
                "GROUP BY minute, poi_type, transportmode, roadcondition)\n" +
                "UPDATE isochrone_aggregated AS ia\n" +
                "SET iri = uv.iri, geometry_iri = uv.geometry_iri,\n" +
                "transportmode_iri =  '" + transportMode + "_' || uuid_generate_v4()::text,\n" +
                "roadcondition_iri =  '" + roadCondition + "_' || uuid_generate_v4()::text\n" +
                "FROM unique_values uv WHERE ia.iri IS NULL\n" +
                "AND ia.poi_type = uv.poi_type\n" +
                "AND ia.transportmode = uv.transportmode\n" +
                "AND ia.minute = uv.minute;" +
                "UPDATE isochrone_aggregated AS t1\n" +
                "SET roadcondition_iri = t2.min_iri\n" +
                "FROM (SELECT roadcondition, MIN(roadcondition_iri) AS min_iri\n" +
                "FROM isochrone_aggregated GROUP BY roadcondition) AS t2\n" +
                "WHERE t1.roadcondition = t2.roadcondition;\n" +
                "UPDATE isochrone_aggregated AS t1\n" +
                "SET transportmode_iri = t2.min_iri\n" +
                "FROM ( SELECT transportmode, MIN(transportmode_iri) AS min_iri\n" +
                "FROM isochrone_aggregated GROUP BY transportmode) AS t2\n" +
                "WHERE t1.transportmode = t2.transportmode;\n" +
                "DROP TABLE eventspace_etas;\n" +
                "DROP TABLE eventspace_delaunay;\n" +
                "DROP TABLE eventspace_isochrones;\n" +
                "DROP TABLE isochrone_individual;\n" +
                "END $$;";
        
                System.out.println(isochroneFunction);

        try (Connection connection = remoteRDBStoreClient.getConnection()) {
            executeSql(connection, isochroneFunction);
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
        String getNearestNode_sql = "SELECT DISTINCT pnn.poi_type FROM " + poiTableName + " AS pnn " +
                "LEFT JOIN isochrone_aggregated AS ia ON pnn.poi_type = ia.poi_type WHERE ia.poi_type IS NULL";

        List<String> poiTypes = new ArrayList<>();

        try (Connection connection = remoteRDBStoreClient.getConnection();
                Statement statement = connection.createStatement();
                ResultSet resultSet = statement.executeQuery(getNearestNode_sql)) {

            while (resultSet.next()) {
                poiTypes.add(resultSet.getString("poi_type"));
            }
        } catch (SQLException e) {
            e.printStackTrace();
            throw new JPSRuntimeException(e);
        }

        return poiTypes;
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

        String smoothIsochrone_sql = "update isochrone_aggregated set geom = ST_CollectionExtract(geom, 3);\n" +
                "update isochrone_aggregated set geom = ST_ChaikinSmoothing(geom, 3, false);";

        try (Connection connection = remoteRDBStoreClient.getConnection()) {
            executeSql(connection, smoothIsochrone_sql);
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

        String createIsochroneBuildingTable_sqlString = "DROP TABLE IF EXISTS isochrone_building_ref;\n" +
                "CREATE TABLE isochrone_building_ref AS\n" +
                "SELECT DISTINCT ia.iri, pno.poi_iri, pno.poi_type\n" +
                "FROM isochrone_aggregated AS ia\n" +
                "LEFT JOIN " + poiTableName + " AS pno ON ia.poi_type = pno.poi_type\n" +
                "ORDER BY ia.iri;";
        try (Connection connection = remoteRDBStoreClient.getConnection()) {
            executeSql(connection, createIsochroneBuildingTable_sqlString);
            System.out.println("isochrone_building_ref table created.");
        } catch (Exception e) {
            e.printStackTrace();
            throw new JPSRuntimeException(e);
        }

    }

}
