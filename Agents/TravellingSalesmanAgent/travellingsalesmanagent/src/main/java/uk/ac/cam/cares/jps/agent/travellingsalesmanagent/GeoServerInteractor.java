package uk.ac.cam.cares.jps.agent.travellingsalesmanagent;

import java.util.ArrayList;
import java.util.List;

import com.cmclinnovations.stack.clients.geoserver.GeoServerClient;
import com.cmclinnovations.stack.clients.geoserver.GeoServerVectorSettings;
import com.cmclinnovations.stack.clients.geoserver.UpdatedGSVirtualTableEncoder;

public class GeoServerInteractor {

    public static void addTSPGeoserverLayer(GeoServerClient geoServerClient, String workspaceName, String schema,
            String dbName, String layerName, String sql) {
        List<String> parameter = new ArrayList<>(List.of("target", "4121", "^[\\d]+$"));
        addGeoserverLayer(geoServerClient, workspaceName, schema, dbName, layerName, sql, parameter);
        
    }

    public static void addPOIGeoserverLayer(GeoServerClient geoServerClient, String workspaceName, String schema,
            String dbName, String poiTableName, String poiLayerName) {
        String sql = "SELECT name, CONCAT('https://www.theworldavatar.com/kg/',poi_tsp_iri) as iri, poi_tsp_type, nearest_node, geom, is_flooded FROM "
                        + poiTableName;
        addGeoserverLayer(geoServerClient, workspaceName, schema, dbName, poiLayerName, sql, null);

    }

    static void addGeoserverLayer(GeoServerClient geoServerClient, String workspaceName, String schema,
    String dbName, String layerName, String sql, List<String> parameter) {
        UpdatedGSVirtualTableEncoder virtualTable = new UpdatedGSVirtualTableEncoder();
        GeoServerVectorSettings geoServerVectorSettings = new GeoServerVectorSettings();
        virtualTable.setSql(sql);
        virtualTable.setEscapeSql(true);
        virtualTable.setName(layerName);
        if (parameter != null) {
            virtualTable.addVirtualTableParameter(parameter.get(0), parameter.get(1), parameter.get(2));
        }
        virtualTable.addVirtualTableGeometry("geom", "Geometry", "4326"); // geom needs to match the sql query
        geoServerVectorSettings.setVirtualTable(virtualTable);
        geoServerClient.createPostGISDataStore(workspaceName, layerName, dbName, schema);
        geoServerClient.createPostGISLayer(workspaceName, dbName, schema, layerName, geoServerVectorSettings);

    }

}
