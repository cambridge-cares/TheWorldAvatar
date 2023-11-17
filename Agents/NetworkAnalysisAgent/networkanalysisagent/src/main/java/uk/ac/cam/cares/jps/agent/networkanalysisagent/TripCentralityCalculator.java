package uk.ac.cam.cares.jps.agent.networkanalysisagent;


import com.cmclinnovations.stack.clients.geoserver.GeoServerClient;
import com.cmclinnovations.stack.clients.geoserver.GeoServerVectorSettings;
import com.cmclinnovations.stack.clients.geoserver.UpdatedGSVirtualTableEncoder;
import org.json.JSONArray;
import org.json.JSONObject;
import org.postgis.PGgeometry;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;

import java.sql.*;

public class TripCentralityCalculator {


    /**
     * Pass POI_tsp in arrays and finds the nearest nodes based on routing_ways road data.
     *
     * @param remoteRDBStoreClient
     * @param jsonArray            POI_tsp in array format
     */
    public void calculateTripCentrality(RemoteRDBStoreClient remoteRDBStoreClient, JSONArray jsonArray,String tableName, String sql ) {


        try (Connection connection = remoteRDBStoreClient.getConnection()) {

            for (int i = 0; i < jsonArray.length(); i++) {
                JSONObject poi_tsp = jsonArray.getJSONObject(i);
                String geometry = poi_tsp.getString("geometry");
                PGgeometry pgGeometry = new PGgeometry(geometry);
                pgGeometry.getGeometry().setSrid(4326);

                int nearest_node = Integer.parseInt(findNearestNode(connection, geometry));
                generateTripCentralityTable(connection,nearest_node, tableName,sql);
                System.out.println("Table generated for " +tableName);

            }
        }
        catch (Exception e) {
            e.printStackTrace();
            throw new JPSRuntimeException(e);
        }

    }
    private void generateTripCentralityTable(Connection connection, int node, String tableName, String costTable) throws SQLException {

        String tripCentrality_sql = "CREATE TABLE tc_"+tableName+" AS (\n" +
                "    SELECT\n" +
                "        b.gid,\n" +
                "        b.the_geom AS geom,\n" +
                "        COUNT(b.the_geom) AS count\n" +
                "    FROM\n" +
                "        routing_ways AS t,\n" +
                "        pgr_dijkstra(\n" +
                "            '"+costTable+"', \n" +
                "             "+node+", t.target,\n" +
                "             directed := FALSE\n" +
                "        ) AS j\n" +
                "    JOIN routing_ways AS b ON j.edge = b.gid \n" +
                "    GROUP BY b.gid, b.the_geom \n" +
                "ORDER BY count DESC\n" +
                ");";
        executeSql(connection,tripCentrality_sql);
    }


    /**
     * Generate TSP route layer
     * @param geoServerClient
     * @param workspaceName
     * @param schema
     * @param dbName
     * @param LayerName
     * @param cost_table
     */
    public void generateTSPLayer(GeoServerClient geoServerClient, String workspaceName, String schema, String dbName, String LayerName, String cost_table){


        String tspLayer = "WITH tsp AS (\n" +
                "    SELECT *\n" +
                "    FROM pgr_TSP(\n" +
                "        $$SELECT * FROM pgr_dijkstraCostMatrix(\n" +
                "            '"+cost_table+"',\n" +
                "            (\n" +
                "                SELECT ARRAY_APPEND(array_agg(id), (%target%))\n" +
                "                FROM routing_ways_vertices_pgr\n" +
                "                WHERE id IN (SELECT DISTINCT nearest_node FROM poi_tsp_nearest_node, flood_polygon_single_10cm WHERE ST_Intersects(poi_tsp_nearest_node.geom, flood_polygon_single_10cm.geom) OR ST_DISTANCE (poi_tsp_nearest_node.geom, flood_polygon_single_10cm.geom) < 0.005)\n" +
                "            ),\n" +
                "            false\n" +
                "        )$$, \n" +
                "        (%target%),"+
                "        (%target%)"+
                "    )\n" +
                "),\n" +
                "tsp_route AS (\n" +
                "SELECT routing_ways.the_geom\n" +
                "FROM (\n" +
                "    SELECT ROW_NUMBER() OVER() as seq, tsp.node\n" +
                "    FROM tsp\n" +
                ") n1\n" +
                "JOIN (\n" +
                "    SELECT ROW_NUMBER() OVER() as seq, tsp.node\n" +
                "    FROM tsp\n" +
                ") n2 ON n1.seq + 1 = n2.seq\n" +
                "JOIN pgr_dijkstra(\n" +
                "    '"+cost_table+"',\n" +
                "    n1.node,\n" +
                "    n2.node, false\n" +
                ") AS di ON true\n" +
                "JOIN routing_ways ON di.edge = routing_ways.gid\n" +
                "ORDER BY n1.seq\n" +
                ")\n" +
                "SELECT tsp_route.the_geom as geom\n" +
                "FROM tsp_route ";

        // Find nearest TSP_node which is the nearest grid
        UpdatedGSVirtualTableEncoder virtualTableTSPRoute = new UpdatedGSVirtualTableEncoder();
        GeoServerVectorSettings geoServerVectorSettingsTSPRoute = new GeoServerVectorSettings();
        virtualTableTSPRoute.setSql(tspLayer);
        virtualTableTSPRoute.setEscapeSql(true);
        virtualTableTSPRoute.setName(LayerName);
        virtualTableTSPRoute.addVirtualTableParameter("target","4121","^[\\d]+$");
        virtualTableTSPRoute.addVirtualTableGeometry("geom", "Geometry", "4326"); // geom needs to match the sql query
        geoServerVectorSettingsTSPRoute.setVirtualTable(virtualTableTSPRoute);
        geoServerClient.createPostGISDataStore(workspaceName,LayerName, dbName, schema);
        geoServerClient.createPostGISLayer(workspaceName, dbName,LayerName ,geoServerVectorSettingsTSPRoute);
    }




    /**
     * Finds the nearest node of the POI_tsp from routing_ways table
     * @param connection
     * @param geom
     * @return
     * @throws SQLException
     */
    private String findNearestNode(Connection connection, String geom) throws SQLException {

        String geomConvert= "ST_GeometryFromText('"+geom+"', 4326)";


        String findNearestNode_sql = "SELECT id, ST_Distance(the_geom, " + geomConvert + ") AS distance\n" +
                "FROM routing_ways_vertices_pgr\n" +
                "ORDER BY the_geom <-> " + geomConvert + "\n" +
                "LIMIT 1;\n";

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
     * @param connection PostgreSQL connection object
     * @param sql SQl statement to execute
     */
    private void executeSql(Connection connection, String sql) throws SQLException {
        try (Statement statement = connection.createStatement()) {
            statement.execute(sql);
        }
    }
}
