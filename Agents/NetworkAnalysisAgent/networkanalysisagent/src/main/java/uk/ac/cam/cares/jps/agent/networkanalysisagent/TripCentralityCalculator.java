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
     * Get the nearest node of POI and create the tripcentrality table
     * @param remoteRDBStoreClient
     * @param jsonArray JSONArray of POI
     * @param tableName Table name of trip centrality table 
     * @param sql EdgeTableSQL 
     */
    public void calculateTripCentrality(RemoteRDBStoreClient remoteRDBStoreClient, JSONArray jsonArray,String tableName, String sql ) {


        try (Connection connection = remoteRDBStoreClient.getConnection()) {

            for (int i = 0; i < jsonArray.length(); i++) {
                JSONObject poi = jsonArray.getJSONObject(i);
                String geometry = poi.getString("geometry");
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

    /** Create trip centrality 
     * @param connection
     * @param node POI node id
     * @param tableName Table name of trip centrality table
     * @param costTable Edge table SQL
     * @throws SQLException
     */
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
     * Generate trip centrality layer that compares the two centrality table by finding the difference of it.
     * @param geoServerClient
     * @param workspaceName 
     * @param schema Schema name - default to public
     * @param dbName Datbase name
     * @param LayerName Geoserver layer name
     * @param normalTableName First trip centrality table 
     * @param floodTableName Second trip centrality table to be compared
     */
    public void generateTCLayer(GeoServerClient geoServerClient, String workspaceName, String schema, String dbName, String LayerName, String normalTableName, String floodTableName){


        String tcLayer = "WITH maxcount AS (\n" +
                "    SELECT MAX(ABS(COALESCE(f.count, 0) - n.count)) AS max\n" +
                "    FROM tc_"+normalTableName+" n\n" +
                "    LEFT JOIN tc_"+ floodTableName +" f ON n.gid = f.gid AND n.geom = f.geom\n" +
                ")\n" +
                "\n" +
                "SELECT\n" +
                "    n.gid,\n" +
                "    r.name,\n" +
                "    r.length_m,\n" +
                "    r.oneway,\n" +
                "    n.geom,\n" +
                "    n.count AS normal_count,\n" +
                "    COALESCE(f.count, 0) AS flooded_count,\n" +
                "    COALESCE(f.count, 0) - n.count AS count_difference,\n" +
                "    (COALESCE(f.count, 0) - n.count)::decimal / maxcount.max AS count_difference_percentage,\n" +
                "maxcount.max as max_countdifference\n" +
                "FROM\n" +
                "    tc_"+normalTableName+" n\n" +
                "LEFT JOIN\n" +
                "    tc_"+ floodTableName +" f ON n.gid = f.gid AND n.geom = f.geom\n" +
                "JOIN\n" +
                "    routing_ways r ON n.gid = r.gid\n" +
                "CROSS JOIN\n" +
                "    maxcount -- Include the CTE in the FROM clause\n" +
                "ORDER BY \n" +
                "    count_difference_percentage DESC";

        UpdatedGSVirtualTableEncoder virtualTableTC = new UpdatedGSVirtualTableEncoder();
        GeoServerVectorSettings geoServerVectorSettingsTC = new GeoServerVectorSettings();
        virtualTableTC.setSql(tcLayer);
        virtualTableTC.setEscapeSql(true);
        virtualTableTC.setName(LayerName);
        virtualTableTC.addVirtualTableGeometry("geom", "Geometry", "4326"); // geom needs to match the sql query
        geoServerVectorSettingsTC.setVirtualTable(virtualTableTC);
        geoServerClient.createPostGISDataStore(workspaceName,LayerName, dbName, schema);
        geoServerClient.createPostGISLayer(workspaceName, dbName,LayerName ,geoServerVectorSettingsTC);
    }


    /**
     * Find the nearest node
     * @param connection
     * @param geom geom wkt from poi
     * @return String of id
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
