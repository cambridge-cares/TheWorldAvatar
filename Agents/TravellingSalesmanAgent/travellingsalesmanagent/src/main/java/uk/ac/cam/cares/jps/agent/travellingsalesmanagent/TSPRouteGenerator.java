package uk.ac.cam.cares.jps.agent.travellingsalesmanagent;

import com.cmclinnovations.stack.clients.geoserver.GeoServerClient;
import com.cmclinnovations.stack.clients.geoserver.GeoServerVectorSettings;
import com.cmclinnovations.stack.clients.geoserver.UpdatedGSVirtualTableEncoder;

public class TSPRouteGenerator {

    String poiTableName; // table containing POI and nearest node information
    String floodTableName; // table defining flooded region
    String routeTablePrefix; // prefix of OSM routing tables
    Double floodCutOff = 0.0; // Cut-off distance in meters between POI and flood; if distance is below this value, POI is consider flooded

    public TSPRouteGenerator(String poiTableName, String floodTableName, String routeTablePrefix, Double floodCutOff) {
        this.poiTableName = poiTableName;
        this.floodTableName = floodTableName;
        this.routeTablePrefix = routeTablePrefix;
        this.floodCutOff = floodCutOff;
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

        String tspLayer = "WITH tsp AS (\n" +
                "    SELECT *\n" +
                "    FROM pgr_TSP(\n" +
                "        $$SELECT * FROM pgr_dijkstraCostMatrix(\n" +
                "            '" + costTable + "',\n" +
                "            (\n" +
                "                SELECT ARRAY_APPEND(array_agg(id), (%target%))\n" +
                "                FROM "+routeTablePrefix+"ways_vertices_pgr\n" +
                "                WHERE id IN (SELECT DISTINCT nearest_node FROM " + poiTableName
                + ", "+floodTableName+" WHERE ST_DISTANCE (" + poiTableName
                + ".geom::geography, "+floodTableName+".geom::geography) <= "+Double.toString(floodCutOff)+")\n"
                +
                "            ),\n" +
                "            false\n" +
                "        )$$, \n" +
                "        (%target%)," +
                "        (%target%)" +
                "    )\n" +
                "),\n" +
                "tsp_route AS (\n" +
                "SELECT "+routeTablePrefix+"ways.the_geom\n" +
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
                "JOIN "+routeTablePrefix+"ways ON di.edge = "+routeTablePrefix+"ways.gid\n" +
                "ORDER BY n1.seq\n" +
                ")\n" +
                "SELECT tsp_route.the_geom as geom\n" +
                "FROM tsp_route ";

        // Add TSP route as geoserver layer
        addTSPGeoserverLayer(geoServerClient, workspaceName, schema, dbName, layerName, tspLayer);
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

        String tspLayer = "WITH tsp AS (\n" +
                "    SELECT *\n" +
                "    FROM pgr_TSP(\n" +
                "        $$SELECT * FROM pgr_dijkstraCostMatrix(\n" +
                "            '" + costTable + "',\n" +
                "            (\n" +
                "                SELECT ARRAY_APPEND(array_agg(id), (%target%))\n" +
                "                FROM "+routeTablePrefix+"ways_vertices_pgr\n" +
                "                WHERE id IN (SELECT DISTINCT nearest_node FROM " + poiTableName
                + ", "+floodTableName+" WHERE ST_DISTANCE (" + poiTableName
                + ".geom::geography, "+floodTableName+".geom::geography) <= "+Double.toString(floodCutOff)+")\n"
                +
                "            ),\n" +
                "            false\n" +
                "        )$$, \n" +
                "        (%target%)," +
                "        (%target%)" +
                "    )\n" +
                "), tsp_seq AS (    SELECT nearest_node as id, " + poiTableName + ".geom\n" +
                "    FROM "+poiTableName+", "+floodTableName+"\n" +
                "                WHERE ST_DISTANCE(" + poiTableName
                + ".geom::geography, "+floodTableName+".geom::geography) <= "+Double.toString(floodCutOff)+")\n"
                +
                "SELECT DISTINCT tsp.seq -1 as seq, tsp.node, tsp.cost, tsp.agg_cost, tsp_seq.geom FROM tsp\n" +
                "FULL JOIN tsp_seq ON tsp.node = tsp_seq.id ORDER BY seq ASC";

        // Add TSP sequence as geoserver layer
        addTSPGeoserverLayer(geoServerClient, workspaceName, schema, dbName, layerName, tspLayer);

    }

    void addTSPGeoserverLayer(GeoServerClient geoServerClient, String workspaceName, String schema,
            String dbName, String layerName, String tspLayer) {
        UpdatedGSVirtualTableEncoder virtualTableTSPRoute = new UpdatedGSVirtualTableEncoder();
        GeoServerVectorSettings geoServerVectorSettingsTSPRoute = new GeoServerVectorSettings();
        virtualTableTSPRoute.setSql(tspLayer);
        virtualTableTSPRoute.setEscapeSql(true);
        virtualTableTSPRoute.setName(layerName);
        virtualTableTSPRoute.addVirtualTableParameter("target", "4121", "^[\\d]+$");
        virtualTableTSPRoute.addVirtualTableGeometry("geom", "Geometry", "4326"); // geom needs to match the sql query
        geoServerVectorSettingsTSPRoute.setVirtualTable(virtualTableTSPRoute);
        geoServerClient.createPostGISDataStore(workspaceName, layerName, dbName, schema);
        geoServerClient.createPostGISLayer(workspaceName, dbName, schema, layerName, geoServerVectorSettingsTSPRoute);
    }
}
