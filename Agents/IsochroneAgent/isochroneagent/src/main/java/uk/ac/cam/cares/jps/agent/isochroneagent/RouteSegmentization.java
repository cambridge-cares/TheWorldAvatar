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

    /**
     * Drop table of routing_ways_segment if it exists.
     * Duplicate and store the segmentized roads in routing_ways_segment.
     * Recalculate the topology.
     * 
     * @param remoteRDBStoreClient
     * @param segmentization_length length to segmentize
     */
    public void segmentize(RemoteRDBStoreClient remoteRDBStoreClient, double segmentization_length) {
        try (Connection connection = remoteRDBStoreClient.getConnection()) {
            String segmentization_create_table = "CREATE TABLE routing_ways_segment AS\n" +
                    "SELECT * FROM routing_ways WHERE 1 = 0;\n";
            String segmentization_split = "INSERT INTO routing_ways_segment\n" +
                    "SELECT gid, osm_id, tag_id, length, length_m, name, source, target,\n" +
                    "source_osm, target_osm, cost, reverse_cost, cost_s, reverse_cost_s, rule,\n" +
                    "one_way, oneway, x1, y1, x2, y2, maxspeed_forward, maxspeed_backward, priority,\n" +
                    "(ST_DumpSegments(ST_Segmentize(the_geom, " + segmentization_length
                    + "))).geom AS the_geom FROM routing_ways;\n" +
                    "UPDATE routing_ways_segment\n" +
                    "SET \n length = ST_Length(the_geom), length_m = ST_Length(ST_Transform(the_geom, 3857)),\n" +
                    "cost_s = ST_Length(ST_Transform(the_geom, 3857)) / (maxspeed_forward * 1000 / 3600),\n" +
                    "reverse_cost_s = CASE\n" +
                    "WHEN reverse_cost_s > 0 THEN ST_Length(ST_Transform(the_geom, 3857)) / (maxspeed_backward* 1000 / 3600)\n"
                    +
                    "WHEN reverse_cost_s < 0 THEN -ST_Length(ST_Transform(the_geom, 3857)) / (maxspeed_backward* 1000 / 3600)\n"
                    +
                    "ELSE 0  -- Assuming default value when reverse_cost_s is 0\n END,\n" +
                    "cost = null, reverse_cost = null;";

            String segmentization_rearrange_sql = "CREATE SEQUENCE temp_sequence;\n" +
                    "UPDATE routing_ways_segment\n SET gid = nextval('temp_sequence');\n" +
                    "SELECT setval('temp_sequence', (SELECT max(gid) FROM routing_ways_segment) + 1);\n" +
                    "DROP SEQUENCE temp_sequence;\n";

            executeSql(connection, segmentization_create_table);
            System.out.println("Duplicated route in a new table. (1/4)");

            executeSql(connection, segmentization_split);
            System.out.println("Split ways successfully.(2/4)");

            executeSql(connection, segmentization_rearrange_sql);
            System.out.println("Reindexed the routes.(3/4)");

            System.out.println("Begin on recalculating topology, this may take awhile.");
            executeSql(connection,
                    ("SELECT pgr_createTopology('routing_ways_segment', 0.000001, 'the_geom', 'gid', 'source', 'target', clean := true);"));
            System.out.println("Recreated routing topology.");
            System.out.println("Analzye isolated edge in graph network.");
            executeSql(connection, "SELECT pgr_analyzeGraph ('routing_ways_segment', 0.001, 'the_geom', 'gid')");
            System.out.println("Dropping isolated networks  in graph network.");
            executeSql(connection, "CREATE TEMPORARY TABLE isolated AS\n" +
                    "SELECT a.gid as ways_id, b.id as source_vertice, c.id as target_vertice\n" +
                    "FROM routing_ways_segment a, routing_ways_segment_vertices_pgr b, routing_ways_segment_vertices_pgr c\n"
                    +
                    "WHERE a.source=b.id AND b.cnt=1 AND a.target=c.id AND c.cnt=1;\n" +
                    "DELETE FROM routing_ways_segment WHERE gid IN ( SELECT ways_id FROM isolated);\n" +
                    "DELETE FROM routing_ways_segment_vertices_pgr\n WHERE id IN ( SELECT source_vertice FROM isolated);\n" +
                    "DELETE FROM routing_ways_segment_vertices_pgr\n WHERE id IN ( SELECT target_vertice FROM isolated);");
            System.out.println("Segmentization completed. Routing_ways_segment table created. (4/4)");
        } catch (Exception e) {
            e.printStackTrace();
            throw new JPSRuntimeException(e);
        }
    }

    /**
     * Pass POI in arrays and finds the nearest nodes based on routing_ways_segment
     * road data.
     * 
     * @param remoteRDBStoreClient
     * @param jsonArray            POI in array format
     */
    public void insertPoiData(RemoteRDBStoreClient remoteRDBStoreClient, JSONArray jsonArray) {

        try (Connection connection = remoteRDBStoreClient.getConnection()) {

            String initialiseTable = "CREATE TABLE IF NOT EXISTS poi_nearest_node ("
                    + "poi_iri VARCHAR, poi_type VARCHAR, nearest_node BIGINT, geom geometry)";

            executeSql(connection, initialiseTable);
            System.out.println("Initialized poi_nearest_node table.");

            String sql = "INSERT INTO poi_nearest_node (poi_iri, poi_type, nearest_node, geom) VALUES (?, ?, ?, ?)";
            PreparedStatement preparedStatement = connection.prepareStatement(sql);

            for (int i = 0; i < jsonArray.length(); i++) {
                JSONObject poi = jsonArray.getJSONObject(i);
                String poiIri = poi.getString("poi_iri");
                String poiType = poi.getString("poi_type");
                // Remove the prefix from poiIri, poiType
                poiIri = poiIri.replace("https://www.theworldavatar.com/kg/Building/", "");
                poiType = poiType.replace("https://www.theworldavatar.com/kg/ontobuiltenv/", "");

                String geometry = poi.getString("geometry");
                String nearest_node = findNearestNode(connection, geometry);

                preparedStatement.setString(1, poiIri);
                preparedStatement.setString(2, poiType);
                preparedStatement.setInt(3, Integer.parseInt(nearest_node));
                preparedStatement.setObject(4, new PGgeometry(geometry));
                preparedStatement.addBatch();
            }
            preparedStatement.executeBatch();

            System.out.println("Table poi_nearest_node created.");
        } catch (Exception e) {
            e.printStackTrace();
            throw new JPSRuntimeException(e);
        }
    }

    /**
     * Finds the nearest node of the POI from routing_ways_segment table
     * 
     * @param connection
     * @param geom
     * @return
     * @throws SQLException
     */
    private String findNearestNode(Connection connection, String geom) throws SQLException {

        String geomConvert = "ST_GeometryFromText('" + geom + "', 4326)";

        String findNearestNode_sql = "SELECT id, ST_Distance(the_geom, " + geomConvert + ") AS distance\n" +
                "FROM routing_ways_segment_vertices_pgr ORDER BY the_geom <-> " + geomConvert + " LIMIT 1;\n";

        try (Statement statement = connection.createStatement()) {
            try (ResultSet resultSet = statement.executeQuery(findNearestNode_sql)) {
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
                ResultSet resultSet = statement.executeQuery("SELECT 1 FROM routing_ways_segment");
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

    public void createFloodCost(RemoteRDBStoreClient remoteRDBStoreClient, int floodDepth_cm) {

        try (Connection connection = remoteRDBStoreClient.getConnection()) {

            String createFloodTableSQL = "CREATE\n" +
                    " MATERIALIZED VIEW IF NOT EXISTS flood_cost_" + floodDepth_cm + "cm_segment AS\n" +
                    "SELECT rw.gid AS id, rw.tag_id as tag_id, rw.source, rw.target,\n" +
                    "CASE WHEN (EXISTS (SELECT 1 FROM flood_polygon_single_" + floodDepth_cm + "cm WHERE\n" +
                    "st_intersects(rw.the_geom, flood_polygon_single_" + floodDepth_cm + "cm.geom))) THEN (- abs(rw.cost_s))\n" +
                    "ELSE rw.cost_s END AS cost_s,\n" +
                    "CASE WHEN (EXISTS (SELECT 1 FROM flood_polygon_single_" + floodDepth_cm + "cm WHERE\n" +
                    "st_intersects(rw.the_geom, flood_polygon_single_" + floodDepth_cm + "cm.geom))) THEN (- abs(rw.reverse_cost_s))\n" +
                    "ELSE rw.reverse_cost_s END AS reverse_cost_s\n" +
                    "FROM routing_ways_segment rw;";

            executeSql(connection, createFloodTableSQL);
            System.out.println("Flood cost segmented tables created.");
        } catch (Exception e) {
            e.printStackTrace();
            throw new JPSRuntimeException(e);
        }

    }

}
