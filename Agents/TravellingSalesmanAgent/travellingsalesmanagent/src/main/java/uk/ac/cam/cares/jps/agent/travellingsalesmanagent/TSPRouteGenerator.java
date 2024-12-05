package uk.ac.cam.cares.jps.agent.travellingsalesmanagent;

import java.sql.Connection;
import java.sql.Statement;

import com.cmclinnovations.stack.clients.geoserver.GeoServerClient;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;

public class TSPRouteGenerator {

    String poiTableName; // table containing POI and nearest node information
    String floodTableName; // table defining flooded region
    String routeTablePrefix; // prefix of OSM routing tables
    Double floodCutOff = 0.0; // Cut-off distance in meters between POI and flood; if distance is below this
                              // value, POI is consider flooded

    public TSPRouteGenerator(String poiTableName, String floodTableName, String routeTablePrefix, Double floodCutOff) {
        this.poiTableName = poiTableName;
        this.floodTableName = floodTableName;
        this.routeTablePrefix = routeTablePrefix;
        this.floodCutOff = floodCutOff;
    }

    public void updatePOIFloodStatus(RemoteRDBStoreClient remoteRDBStoreClient) {
        String sql = "UPDATE " + poiTableName + " SET is_flooded = CASE WHEN nearest_node IN (" +
                "SELECT DISTINCT nearest_node FROM " + poiTableName + ", " + floodTableName +
                " WHERE ST_DISTANCE (" + poiTableName + ".geom::geography, " + floodTableName + ".geom::geography) <= "
                + Double.toString(floodCutOff) + ")" + " THEN TRUE ELSE FALSE END;";
        try (Connection connection = remoteRDBStoreClient.getConnection()) {
            try (Statement statement = connection.createStatement()) {
                statement.execute(sql);
            }
        } catch (Exception e) {
            e.printStackTrace();
            throw new JPSRuntimeException(e);
        }

    }

    /**
     * Generate TSP route layer
     * 
     * @param geoServerClient
     * @param workspaceName
     * @param schema
     * @param dbName
     * @param layerName
     * @param costTable
     */
    public void generateTSPLayer(GeoServerClient geoServerClient, String workspaceName, String schema, String dbName,
            String layerName, String costTable) {

        String tspLayer = generateTSPCTE(costTable) +
                "tsp_route AS (\n" +
                "SELECT " + routeTablePrefix + "ways.the_geom\n" +
                "FROM (\n" +
                "    SELECT ROW_NUMBER() OVER() as seq, tsp.node\n" +
                "    FROM tsp\n" +
                ") n1\n" +
                "JOIN (\n" +
                "    SELECT ROW_NUMBER() OVER() as seq, tsp.node\n" +
                "    FROM tsp\n" +
                ") n2 ON n1.seq + 1 = n2.seq\n" +
                "JOIN pgr_dijkstra(\n" +
                "    '" + costTable + "',\n" +
                "    n1.node,\n" +
                "    n2.node, false\n" +
                ") AS di ON true\n" +
                "JOIN " + routeTablePrefix + "ways ON di.edge = " + routeTablePrefix + "ways.gid\n" +
                "ORDER BY n1.seq\n" +
                ")\n" +
                "SELECT tsp_route.the_geom as geom\n" +
                "FROM tsp_route ";

        // Add TSP route as geoserver layer
        GeoServerInteractor.addTSPGeoserverLayer(geoServerClient, workspaceName, schema, dbName, layerName, tspLayer);
    }

    String generateTSPCTE(String costTable) {
        return "WITH tsp AS ( SELECT * FROM pgr_TSP(\n" +
                "$$SELECT * FROM pgr_dijkstraCostMatrix('" + costTable + "',\n" +
                "(SELECT ARRAY_APPEND(array_agg(id), (%target%))\n" +
                "FROM " + routeTablePrefix + "ways_vertices_pgr WHERE id IN (\n" +
                "SELECT DISTINCT nearest_node FROM " + poiTableName + " WHERE is_flooded = 'TRUE')\n" +
                "), false )$$, (%target%), (%target%) ) ),\n";
    }

    /**
     * Generate TSP sequence layer
     * 
     * @param geoServerClient
     * @param workspaceName
     * @param schema
     * @param dbName
     * @param layerName
     * @param costTable
     */
    public void generateSequenceLayer(GeoServerClient geoServerClient, String workspaceName, String schema,
            String dbName, String layerName, String costTable) {

        layerName = layerName + "_seq";

        String tspLayer = generateTSPCTE(costTable) +
                "tsp_seq AS (    SELECT nearest_node as id, " + poiTableName + ".geom\n" +
                "    FROM " + poiTableName + " WHERE is_flooded = 'TRUE')\n" +
                "SELECT DISTINCT tsp.seq -1 as seq, tsp.node, tsp.cost, tsp.agg_cost, tsp_seq.geom FROM tsp\n" +
                "FULL JOIN tsp_seq ON tsp.node = tsp_seq.id ORDER BY seq ASC";

        // Add TSP sequence as geoserver layer
        GeoServerInteractor.addTSPGeoserverLayer(geoServerClient, workspaceName, schema, dbName, layerName, tspLayer);

    }

}
