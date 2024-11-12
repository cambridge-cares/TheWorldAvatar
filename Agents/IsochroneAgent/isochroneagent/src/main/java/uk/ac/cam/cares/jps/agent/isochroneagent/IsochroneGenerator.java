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
            Map<String, String> EdgesTableSQLMap, boolean oncePerPOI) {

        generateTable(remoteRDBStoreClient);

        List<String> poiTypes;
        try {
            // Initialize poiTypes and get distinct poiType;
            poiTypes = getPoiTypes(remoteRDBStoreClient, oncePerPOI);

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

        String addExtension = "CREATE EXTENSION IF NOT EXISTS \"uuid-ossp\";";

        // Combination of minute, transport mode, road condition and POI type must be unique
        String tableConstraint = "DO $$ DECLARE constraint_count INTEGER; BEGIN\n"
                + "SELECT COUNT(*) INTO constraint_count FROM INFORMATION_SCHEMA.TABLE_CONSTRAINTS WHERE "
                + "TABLE_NAME = 'isochrone_aggregated' AND CONSTRAINT_NAME = 'unique_minute_transportmode_roadcondition_poi_type';\n"
                + "IF constraint_count = 0 THEN ALTER TABLE isochrone_aggregated "
                + "ADD CONSTRAINT unique_minute_transportmode_roadcondition_poi_type "
                + "UNIQUE (minute, transportmode, roadcondition, poi_type); END IF; END $$;";

        try (Connection connection = remoteRDBStoreClient.getConnection()) {
            executeSql(connection, addExtension);
            executeSql(connection, tableGeneration);
            executeSql(connection, tableConstraint);
            System.out.println("isochrone_aggregated table created. Calculating isochrones based on POI now.");
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

        // Define isochrone function
        String isochroneFunction = "DO $$ \nDECLARE node bigint;\nBEGIN\n";
        // Remove temporary tables
        isochroneFunction = isochroneFunction + "DROP TABLE IF EXISTS eventspace_etas;\n"
                + "DROP TABLE IF EXISTS eventspace_isochrones;\n" + "DROP TABLE IF EXISTS isochrone_individual; \n";
        // Initialise temporary tables
        isochroneFunction = isochroneFunction + "CREATE TABLE eventspace_etas (\n"
                + "id bigint, agg_cost double precision, the_geom geometry(PointZ, 4326));\n"
                + "CREATE TABLE eventspace_isochrones (\n"
                + "id bigint, geom geometry, transportmode VARCHAR, transportmode_iri VARCHAR,\n"
                + "roadcondition VARCHAR, roadcondition_iri VARCHAR, poi_type VARCHAR, minute integer);\n"
                + "CREATE TABLE isochrone_individual (\n"
                + "id bigint, minute integer, transportmode VARCHAR, transportmode_iri VARCHAR,\n"
                + "roadcondition VARCHAR, roadcondition_iri VARCHAR, poi_type VARCHAR, geom geometry\n);\n";
        // Loop for each POI of the same POI type
        isochroneFunction = isochroneFunction + "FOR node IN (SELECT DISTINCT nearest_node FROM " + poiTableName
                + " WHERE poi_type = '" + poiType + "') LOOP\nTRUNCATE TABLE eventspace_etas;\n"
                + "TRUNCATE TABLE eventspace_isochrones;\n";
        // Calculate driving distance from a POI
        // 10000 is an arbitrary large number to ensure all nodes are extracted
        isochroneFunction = isochroneFunction + "INSERT INTO eventspace_etas (id, agg_cost, the_geom)\n"
                + "SELECT id, dd.agg_cost,\n"
                + "ST_SetSRID(ST_MakePoint(ST_X(v.the_geom), ST_Y(v.the_geom), dd.agg_cost), 4326) AS the_geom\n"
                + "FROM pgr_drivingDistance(\n" + "'" + edgeTable + "', node, 10000, false) AS dd\n" + "JOIN "
                + routeTableName + "_segment_vertices_pgr AS v ON dd.node = v.id;\n";
        // Recreate delaunay triangles
        isochroneFunction = isochroneFunction + "drop table if exists eventspace_delaunay;\n"
                + "create table eventspace_delaunay\n"
                + "as (select (ST_Dump(ST_DelaunayTriangles(ST_Collect(the_geom)))).geom from eventspace_etas);\n";
        // Isochrones are convex hull of delaunay triangles, filtered by elevation which
        // is traveling cost
        isochroneFunction = isochroneFunction +
                "INSERT INTO eventspace_isochrones (geom, minute)\n"
                + "SELECT ST_Union(ST_ConvexHull(ST_LocateBetweenElevations(ST_Boundary(geom), 0, minute * 60))) geom,\n"
                + "minute FROM eventspace_delaunay, generate_series(0, " + upperTimeLimit + ", " + timeInterval
                + " ) AS minute GROUP BY minute;\n";
        // Add metadata to isochrones
        isochroneFunction = isochroneFunction + "UPDATE eventspace_isochrones\n" + "SET transportmode = '"
                + transportMode + "', roadcondition = '" + roadCondition + "', poi_type = '" + poiType + "';\n";
        // Add isochrones to isochrone_individual
        isochroneFunction = isochroneFunction
                + "INSERT INTO isochrone_individual (minute, transportmode, roadcondition, poi_type, geom)\n"
                + "SELECT minute,transportmode, roadcondition, poi_type, ST_Union(geom) AS geom\n"
                + "FROM eventspace_isochrones GROUP BY minute,transportmode, roadcondition, poi_type;\n"
                + "END LOOP;\n";
        // Aggregate isochrones of each POI
        isochroneFunction = isochroneFunction
                + "INSERT INTO isochrone_aggregated (minute, transportmode, roadcondition, poi_type, geom)\n"
                + "SELECT minute,transportmode, roadcondition, poi_type, ST_UNION(geom) AS geom\n"
                + "FROM isochrone_individual AS ii WHERE ii.minute !=0 GROUP BY\n"
                + "ii.minute, ii.transportmode, ii.roadcondition, ii.poi_type\n"
                + "ON CONFLICT (minute, transportmode, roadcondition, poi_type)\n"
                + "DO UPDATE SET geom = EXCLUDED.geom;";
        // Generate IRI for aggregated isochrones
        isochroneFunction = isochroneFunction
                + "WITH unique_values AS ( SELECT poi_type, minute, transportmode, roadcondition,\n"
                + "uuid_generate_v4()::text AS iri, uuid_generate_v4()::text AS geometry_iri\n"
                + "FROM isochrone_aggregated WHERE iri IS NULL\n"
                + "GROUP BY minute, poi_type, transportmode, roadcondition)\n" + "UPDATE isochrone_aggregated AS ia\n"
                + "SET iri = uv.iri, geometry_iri = uv.geometry_iri\n" + "FROM unique_values uv WHERE ia.iri IS NULL\n"
                + "AND ia.poi_type = uv.poi_type\n" + "AND ia.transportmode = uv.transportmode\n"
                + "AND ia.minute = uv.minute;";
        // Set road condition IRI, use existing value or generate new IRI
        isochroneFunction = isochroneFunction + "WITH distinct_roadcondition_iri AS (SELECT roadcondition, CASE\n"
                + "WHEN COUNT(DISTINCT roadcondition_iri) = 1 THEN MIN(roadcondition_iri)\n"
                + "WHEN COUNT(DISTINCT roadcondition_iri) = 0 THEN roadcondition || '_' || uuid_generate_v4()::text\n"
                + "ELSE NULL END AS roadcondition_iri FROM isochrone_aggregated GROUP BY roadcondition)\n"
                + "UPDATE isochrone_aggregated AS ia\n" + "SET roadcondition_iri = dri.roadcondition_iri\n"
                + "FROM distinct_roadcondition_iri AS dri\n" + "WHERE ia.roadcondition = dri.roadcondition;\n";
        // Set transport mode IRI to the minimum of existing transport mode IRIs
        isochroneFunction = isochroneFunction + "WITH distinct_transportmode_iri AS (SELECT transportmode, CASE\n"
                + "WHEN COUNT(DISTINCT transportmode_iri) = 1 THEN MIN(transportmode_iri)\n"
                + "WHEN COUNT(DISTINCT transportmode_iri) = 0 THEN transportmode || '_' || uuid_generate_v4()::text\n"
                + "ELSE NULL END AS transportmode_iri FROM isochrone_aggregated GROUP BY transportmode)\n"
                + "UPDATE isochrone_aggregated AS ia\n" + "SET transportmode_iri = dti.transportmode_iri\n"
                + "FROM distinct_transportmode_iri AS dti\n" + "WHERE ia.transportmode = dti.transportmode;\n";
        // Remove temporary tables
        isochroneFunction = isochroneFunction + "DROP TABLE eventspace_etas;\n" + "DROP TABLE eventspace_delaunay;\n"
                + "DROP TABLE eventspace_isochrones;\n" + "DROP TABLE isochrone_individual;\n" + "END $$;";
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
    public List<String> getPoiTypes(RemoteRDBStoreClient remoteRDBStoreClient, Boolean oncePerPOI) throws SQLException {
        String getPoiSQL = "SELECT DISTINCT pnn.poi_type FROM " + poiTableName + " AS pnn";
        if (oncePerPOI) {
            getPoiSQL = getPoiSQL
                    + " LEFT JOIN isochrone_aggregated AS ia ON pnn.poi_type = ia.poi_type WHERE ia.poi_type IS NULL";
        }
        List<String> poiTypes = new ArrayList<>();
        try (Connection connection = remoteRDBStoreClient.getConnection();
                Statement statement = connection.createStatement();
                ResultSet resultSet = statement.executeQuery(getPoiSQL)) {
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
