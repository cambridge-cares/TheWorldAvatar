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

    String routeTablePrefix; // prefix of OSM routing tables

    public TripCentralityCalculator(String routeTablePrefix) {
        this.routeTablePrefix = routeTablePrefix;
    }

    /**
     * Get the nearest node of POI and create the tripcentrality table
     * 
     * @param remoteRDBStoreClient
     * @param jsonArray            JSONArray of POI
     * @param tableName            Table name of trip centrality table
     * @param sql                  EdgeTableSQL
     */
    public void calculateTripCentrality(RemoteRDBStoreClient remoteRDBStoreClient, JSONArray jsonArray,
            String tableName, String sql) {

        try (Connection connection = remoteRDBStoreClient.getConnection()) {

            for (int i = 0; i < jsonArray.length(); i++) {
                JSONObject poi = jsonArray.getJSONObject(i);
                String geometry = poi.getString("geometry");
                PGgeometry pgGeometry = new PGgeometry(geometry);
                pgGeometry.getGeometry().setSrid(4326);

                int nearestNode = Integer.parseInt(findNearestNode(connection, geometry));
                generateTripCentralityTable(connection, nearestNode, tableName, sql);
                System.out.println("Table generated for " + tableName);

            }
        } catch (Exception e) {
            e.printStackTrace();
            throw new JPSRuntimeException(e);
        }

    }

    /**
     * Create trip centrality
     * 
     * @param connection
     * @param node       POI node id
     * @param tableName  Table name of trip centrality table
     * @param costTable  Edge table SQL
     * @throws SQLException
     */
    private void generateTripCentralityTable(Connection connection, int node, String tableName, String costTable)
            throws SQLException {

        String dropTableSQL = "DROP TABLE IF EXISTS tc_" + tableName;
        executeSql(connection, dropTableSQL);

        String tripCentralitySQL = "WITH tmp AS (SELECT end_vid, edge FROM pgr_dijkstra('" + costTable + "', "
                + node + ", (SELECT ARRAY_AGG(id) FROM " + routeTablePrefix
                + "ways_vertices_pgr), directed := FALSE))\n SELECT b.gid, b.the_geom AS geom, COUNT(b.gid) AS count, "
                + "CAST(COUNT(b.gid) AS DECIMAL)/CAST(stat.total AS DECIMAL) AS bci INTO tc_" + tableName +
                " FROM tmp AS j JOIN " + routeTablePrefix + "ways AS b ON j.edge = b.gid CROSS JOIN " +
                "(SELECT COUNT (DISTINCT end_vid) AS total FROM tmp) AS stat GROUP BY b.gid, b.the_geom, stat.total";

        executeSql(connection, tripCentralitySQL);
    }

    /**
     * Generate trip centrality layer that compares the two centrality table by
     * finding the difference of it.
     * 
     * @param geoServerClient
     * @param workspaceName
     * @param schema          Schema name - default to public
     * @param dbName          Datbase name
     * @param layerName       Geoserver layer name
     * @param normalTableName First trip centrality table
     * @param floodTableName  Second trip centrality table to be compared
     */
    public void generateTCLayer(GeoServerClient geoServerClient, String workspaceName, String schema, String dbName,
            String layerName, String normalTableName, String floodTableName, String floodCostTableName) {

        String tcLayer = "SELECT a.gid, a.bci AS normal_betweeness_centrality_index, "
                + "COALESCE(b.bci, 0.0) AS flooded_betweeness_centrality_index, "
                + "COALESCE(b.bci, 0.0) - a.bci AS absolute_change_betweeness_centrality_index, "
                + "a.geom, r.name, r.length_m, r.oneway, " + "CASE WHEN EXISTS (SELECT 1 FROM " + floodCostTableName
                + " fc WHERE a.gid = fc.id AND fc.cost_s <0) THEN TRUE ELSE FALSE END AS is_flooded "
                + "FROM tc_" + normalTableName + " a "
                + "LEFT JOIN tc_" + floodTableName + " b on a.gid = b.gid JOIN " + routeTablePrefix
                + "ways r ON a.gid = r.gid";
        createGeoserverLayer(geoServerClient, tcLayer, layerName, workspaceName, dbName, schema);
    }

    /**
     * Find the nearest node
     * 
     * @param connection
     * @param geom       geom wkt from poi
     * @return String of id
     * @throws SQLException
     */
    private String findNearestNode(Connection connection, String geom) throws SQLException {

        String geomConvert = "ST_GeometryFromText('" + geom + "', 4326)";

        String findNearestNodeSQL = "SELECT id, ST_Distance(the_geom, " + geomConvert + ") AS distance\n" +
                "FROM " + routeTablePrefix + "ways_vertices_pgr\n" + "ORDER BY the_geom <-> " + geomConvert
                + " LIMIT 1;";

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

    private void createGeoserverLayer(GeoServerClient geoServerClient, String sql, String layerName,
            String workspaceName, String dbName, String schema) {
        UpdatedGSVirtualTableEncoder virtualTableTC = new UpdatedGSVirtualTableEncoder();
        GeoServerVectorSettings geoServerVectorSettingsTC = new GeoServerVectorSettings();
        virtualTableTC.setSql(sql);
        virtualTableTC.setEscapeSql(true);
        virtualTableTC.setName(layerName);
        virtualTableTC.addVirtualTableGeometry("geom", "Geometry", "4326"); // geom needs to match the sql query
        geoServerVectorSettingsTC.setVirtualTable(virtualTableTC);
        geoServerClient.createPostGISDataStore(workspaceName, layerName, dbName, schema);
        geoServerClient.createPostGISLayer(workspaceName, dbName, schema, layerName, geoServerVectorSettingsTC);
    }
}
