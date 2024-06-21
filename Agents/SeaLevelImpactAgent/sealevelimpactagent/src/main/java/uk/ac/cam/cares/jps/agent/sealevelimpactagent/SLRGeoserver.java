package uk.ac.cam.cares.jps.agent.sealevelimpactagent;

import com.cmclinnovations.stack.clients.geoserver.GeoServerClient;
import com.cmclinnovations.stack.clients.geoserver.GeoServerVectorSettings;
import com.cmclinnovations.stack.clients.geoserver.UpdatedGSVirtualTableEncoder;
public class SLRGeoserver {
    public static void createSeaLevelGeoserver(String ssp_scenario, Integer projectionyear, String confidence, Integer quantile){
        //Create geoserver layer
        String geoservernamestring= ssp_scenario+projectionyear+confidence+quantile;
        GeoServerClient geoServerClient = GeoServerClient.getInstance();
        String workspaceName= "twa";
        String schema = "public";
        geoServerClient.createWorkspace(workspaceName);

        
        UpdatedGSVirtualTableEncoder virtualTable = new UpdatedGSVirtualTableEncoder();
        GeoServerVectorSettings geoServerVectorSettings = new GeoServerVectorSettings();
        virtualTable.setSql("SELECT ssp_scenario, confidence, quantile, projectionyear, projectionreferenceyear, sealevelriseinmeters, CONCAT('https://www.theworldavatar.com/kg/ontosealevel/SeaLevelChange/',uuid) as iri, geom FROM sealevelprojections WHERE ssp_scenario = '"+ssp_scenario+"' AND confidence= '"+confidence+"' AND quantile= '"+quantile+"' AND projectionyear= '"+projectionyear+"'");
        virtualTable.setEscapeSql(true);
        virtualTable.setName("sealevelprojection"+geoservernamestring);
        virtualTable.addVirtualTableGeometry("geometry", "Geometry", "4326"); // geom needs to match the sql query
        geoServerVectorSettings.setVirtualTable(virtualTable);
        geoServerClient.createPostGISDataStore(workspaceName,"sealevelprojection"+geoservernamestring , SeaLevelImpactAgent.dbName, schema);
        geoServerClient.createPostGISLayer(workspaceName, SeaLevelImpactAgent.dbName,"sealevelprojection"+geoservernamestring ,geoServerVectorSettings);
    }

}
