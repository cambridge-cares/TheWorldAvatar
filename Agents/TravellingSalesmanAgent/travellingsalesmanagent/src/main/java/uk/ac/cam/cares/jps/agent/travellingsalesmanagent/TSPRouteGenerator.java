package uk.ac.cam.cares.jps.agent.travellingsalesmanagent;


import com.cmclinnovations.stack.clients.geoserver.GeoServerClient;
import com.cmclinnovations.stack.clients.geoserver.GeoServerVectorSettings;
import com.cmclinnovations.stack.clients.geoserver.UpdatedGSVirtualTableEncoder;

public class TSPRouteGenerator {


    /**
     * Create isochrone_aggregated table to store all final isochrone.
     */
    public void generateTSPLayer(GeoServerClient geoServerClient, String workspaceName, String schema, String dbName, String LayerName, String cost_table){

        String tspLayer = "WITH tsp AS (\n" +
                "    SELECT *\n" +
                "    FROM pgr_TSP(\n" +
                "        $$SELECT * FROM pgr_dijkstraCostMatrix(\n" +
                "            'SELECT gid as id, source, target, cost_s as cost FROM routing_ways',\n" +
                "            (\n" +
                "                SELECT array_agg(id)\n" +
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


    

}
