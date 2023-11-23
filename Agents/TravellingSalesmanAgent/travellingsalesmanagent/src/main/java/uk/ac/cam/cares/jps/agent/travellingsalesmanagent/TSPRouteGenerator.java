package uk.ac.cam.cares.jps.agent.travellingsalesmanagent;


import com.cmclinnovations.stack.clients.geoserver.GeoServerClient;
import com.cmclinnovations.stack.clients.geoserver.GeoServerVectorSettings;
import com.cmclinnovations.stack.clients.geoserver.UpdatedGSVirtualTableEncoder;

public class TSPRouteGenerator {


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
     * Generate TSP sequence layer
     * @param geoServerClient
     * @param workspaceName
     * @param schema
     * @param dbName
     * @param LayerName
     * @param cost_table
     */
    public void generateSequenceLayer(GeoServerClient geoServerClient, String workspaceName, String schema, String dbName, String LayerName, String cost_table){

        LayerName= LayerName+"_seq";


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
                "), tsp_seq AS (    SELECT nearest_node as id, poi_tsp_nearest_node.geom\n" +
                "    FROM poi_tsp_nearest_node, flood_polygon_single_10cm\n" +
                "                WHERE ST_Intersects(poi_tsp_nearest_node.geom, flood_polygon_single_10cm.geom)\n" +
                "                    OR ST_DISTANCE(poi_tsp_nearest_node.geom, flood_polygon_single_10cm.geom) < 0.005)\n" +
                "SELECT DISTINCT tsp.seq -1 as seq, tsp.node, tsp.cost, tsp.agg_cost, tsp_seq.geom FROM tsp\n" +
                "FULL JOIN tsp_seq ON tsp.node = tsp_seq.id ORDER BY seq ASC";

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
}
