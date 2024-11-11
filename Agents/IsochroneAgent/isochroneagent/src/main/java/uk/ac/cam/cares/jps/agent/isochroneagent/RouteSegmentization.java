package uk.ac.cam.cares.jps.agent.isochroneagent;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import org.postgis.PGgeometry;
import org.json.JSONArray;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;

public class RouteSegmentization {

    String poiTableName; // default value
    String routeTableName;
    String routeSegmentTableName;

    public RouteSegmentization(String poiTableName, String routeTableName) {
        this.poiTableName = poiTableName;
        this.routeTableName = routeTableName;
        this.routeSegmentTableName = routeTableName + "_segment";
    }

    /**
     * Drop table of routeSegmentTableName if it exists.
     * Duplicate and store the segmentized roads in routeSegmentTableName.
     * Recalculate the topology.
     * 
     * @param remoteRDBStoreClient
     * @param segmentization_length length to segmentize
     */
    public void segmentize(RemoteRDBStoreClient remoteRDBStoreClient, double segmentization_length) {
        try (Connection connection = remoteRDBStoreClient.getConnection()) {
            String segmentizationCreateTable = "CREATE TABLE " + routeSegmentTableName + " AS\n" +
                    "SELECT * FROM " + routeTableName + " WHERE 1 = 0;\n";
            String segmentizationSplit = "INSERT INTO " + routeSegmentTableName + "\n" +
                    "SELECT gid, osm_id, tag_id, length, length_m, name, source, target,\n" +
                    "source_osm, target_osm, cost, reverse_cost, cost_s, reverse_cost_s, rule,\n" +
                    "one_way, oneway, x1, y1, x2, y2, maxspeed_forward, maxspeed_backward, priority,\n" +
                    "(ST_DumpSegments(ST_Segmentize(the_geom, " + segmentization_length
                    + "))).geom AS the_geom FROM " + routeTableName + ";\n" +
                    "UPDATE " + routeSegmentTableName + "\n" +
                    "SET \n length = ST_Length(the_geom), length_m = ST_Length(ST_Transform(the_geom, 3857)),\n" +
                    "cost = null, reverse_cost = null;\n" +
                    "UPDATE " + routeSegmentTableName + " SET\n" +
                    "cost_s = length_m / (maxspeed_forward * 1000 / 3600),\n" +
                    "reverse_cost_s = CASE\n" +
                    "WHEN reverse_cost_s > 0 THEN length_m / (maxspeed_backward* 1000 / 3600)\n" +
                    "WHEN reverse_cost_s < 0 THEN -length_m / (maxspeed_backward* 1000 / 3600)\n" +
                    "ELSE 0  -- Assuming default value when reverse_cost_s is 0\n END;";

            String segmentizationRearrangeSQL = "CREATE SEQUENCE temp_sequence;\n" +
                    "UPDATE " + routeSegmentTableName + "\n SET gid = nextval('temp_sequence');\n" +
                    "SELECT setval('temp_sequence', (SELECT max(gid) FROM " + routeSegmentTableName + ") + 1);\n" +
                    "DROP SEQUENCE temp_sequence;\n";

            executeSql(connection, segmentizationCreateTable);
            System.out.println("Duplicated route in a new table. (1/4)");

            executeSql(connection, segmentizationSplit);
            System.out.println("Split ways successfully.(2/4)");

            executeSql(connection, segmentizationRearrangeSQL);
            System.out.println("Reindexed the routes.(3/4)");

            System.out.println("Begin on recalculating topology, this may take awhile.");
            // 0.00001 in 4326 is about 1 m
            executeSql(connection,
                    ("SELECT pgr_createTopology('" + routeSegmentTableName
                            + "', 0.00001, 'the_geom', 'gid', 'source', 'target', clean := true);"));
            System.out.println("Recreated routing topology.");
            System.out.println("Analzye isolated edge in graph network.");
            executeSql(connection, "SELECT pgr_analyzeGraph ('" + routeSegmentTableName + "', " + segmentization_length
                    + ", 'the_geom', 'gid')");
            System.out.println("Dropping isolated networks  in graph network.");
            executeSql(connection, "CREATE TEMPORARY TABLE isolated AS\n" +
                    "SELECT a.gid as ways_id, b.id as source_vertice, c.id as target_vertice\n" +
                    "FROM " + routeSegmentTableName + " a, " + routeSegmentTableName + "_vertices_pgr b, "
                    + routeSegmentTableName + "_vertices_pgr c\n"
                    + "WHERE a.source=b.id AND b.cnt=1 AND a.target=c.id AND c.cnt=1;\n" +
                    "DELETE FROM " + routeSegmentTableName + " WHERE gid IN ( SELECT ways_id FROM isolated);\n" +
                    "DELETE FROM " + routeSegmentTableName + "_vertices_pgr\n WHERE id IN " +
                    "(SELECT source_vertice FROM isolated) OR id IN (SELECT target_vertice FROM isolated);");
            System.out.println("Segmentization completed. " + routeSegmentTableName + " table created. (4/4)");
        } catch (Exception e) {
            e.printStackTrace();
            throw new JPSRuntimeException(e);
        }
    }

    /**
     * Pass POI in arrays and finds the nearest nodes based on routeSegmentTableName
     * road data.
     * 
     * @param remoteRDBStoreClient
     * @param jsonArray            POI in array format
     */
    public void insertPoiData(RemoteRDBStoreClient remoteRDBStoreClient, JSONArray jsonArray) {

        try (Connection connection = remoteRDBStoreClient.getConnection()) {

            String initialiseTable = "CREATE TABLE IF NOT EXISTS " + poiTableName + " ("
                    + "poi_iri VARCHAR UNIQUE, poi_type VARCHAR, nearest_node BIGINT, geom geometry)";

            executeSql(connection, initialiseTable);
            System.out.println("Initialized " + poiTableName + " table.");

            String sql = "INSERT INTO " + poiTableName + " (poi_iri, poi_type, nearest_node, geom) VALUES (?, ?, ?, ?)" +
            "ON CONFLICT (poi_iri) DO UPDATE SET poi_type = EXCLUDED.poi_type, nearest_node = EXCLUDED.nearest_node , geom = EXCLUDED.geom";
            PreparedStatement preparedStatement = connection.prepareStatement(sql);

            for (int i = 0; i < jsonArray.length(); i++) {
                JSONObject poi = jsonArray.getJSONObject(i);
                String poiIri = poi.getString("poi_iri");
                String poiType = poi.getString("poi_type");
                // Remove the prefix from poiIri, poiType
                poiIri = poiIri.replace("https://www.theworldavatar.com/kg/Building/", "");
                poiType = poiType.replace("https://www.theworldavatar.com/kg/ontobuiltenv/", "");

                String geometry = poi.getString("geometry");
                String nearestNode = findNearestNode(connection, geometry);

                preparedStatement.setString(1, poiIri);
                preparedStatement.setString(2, poiType);
                preparedStatement.setInt(3, Integer.parseInt(nearestNode));
                preparedStatement.setObject(4, new PGgeometry(geometry));
                preparedStatement.addBatch();
            }
            preparedStatement.executeBatch();

            System.out.println("Table " + poiTableName + " created.");
        } catch (Exception e) {
            e.printStackTrace();
            throw new JPSRuntimeException(e);
        }
    }

    /**
     * Finds the nearest node of the POI from routeSegmentTableName table
     * 
     * @param connection
     * @param geom
     * @return
     * @throws SQLException
     */
    private String findNearestNode(Connection connection, String geom) throws SQLException {

        String geomConvert = "ST_GeometryFromText('" + geom + "', 4326)";

        String findNearestNodeSQL = "SELECT id, ST_Distance(the_geom, " + geomConvert + ") AS distance\n" +
                "FROM " + routeSegmentTableName + "_vertices_pgr ORDER BY the_geom <-> " + geomConvert + " LIMIT 1;\n";

        try (Statement statement = connection.createStatement()) {
            try (ResultSet resultSet = statement.executeQuery(findNearestNodeSQL)) {
                if (resultSet.next()) {
                    // Assuming 'id' and 'distance' are columns in your query result
                    int id = resultSet.getInt("id");
                    return Integer.toString(id);
                } else {
                    // No results found
                    return null;
                }
            }
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

    public boolean doesTableExist(RemoteRDBStoreClient remoteRDBStoreClient) {
        try (Connection connection = remoteRDBStoreClient.getConnection()) {
            try (Statement statement = connection.createStatement()) {
                // Use a ResultSet to query for the table's existence
                ResultSet resultSet = statement.executeQuery("SELECT 1 FROM " + routeSegmentTableName + "");
                // If the query is successful, the table exists
                return true;
            } catch (SQLException e) {
                // If an exception is thrown, the table does not exist
                return false;
            }
        } catch (Exception e) {
            e.printStackTrace();
            throw new JPSRuntimeException(e);
        }
    }

    public void createFloodCost(RemoteRDBStoreClient remoteRDBStoreClient, int floodDepthCM) {

        try (Connection connection = remoteRDBStoreClient.getConnection()) {

            String createFloodTableSQL = "CREATE\n" +
                    " MATERIALIZED VIEW IF NOT EXISTS flood_cost_" + floodDepthCM + "cm_segment AS\n" +
                    "SELECT rw.gid AS id, rw.tag_id as tag_id, rw.source, rw.target,\n" +
                    "CASE WHEN (EXISTS (SELECT 1 FROM flood_polygon_single_" + floodDepthCM + "cm WHERE\n" +
                    "st_intersects(rw.the_geom, flood_polygon_single_" + floodDepthCM
                    + "cm.geom))) THEN (- abs(rw.cost_s))\n" +
                    "ELSE rw.cost_s END AS cost_s,\n" +
                    "CASE WHEN (EXISTS (SELECT 1 FROM flood_polygon_single_" + floodDepthCM + "cm WHERE\n" +
                    "st_intersects(rw.the_geom, flood_polygon_single_" + floodDepthCM
                    + "cm.geom))) THEN (- abs(rw.reverse_cost_s))\n" +
                    "ELSE rw.reverse_cost_s END AS reverse_cost_s\n" +
                    "FROM " + routeSegmentTableName + " rw;";

            executeSql(connection, createFloodTableSQL);
            System.out.println("Flood cost segmented tables created.");
        } catch (Exception e) {
            e.printStackTrace();
            throw new JPSRuntimeException(e);
        }

    }

}
