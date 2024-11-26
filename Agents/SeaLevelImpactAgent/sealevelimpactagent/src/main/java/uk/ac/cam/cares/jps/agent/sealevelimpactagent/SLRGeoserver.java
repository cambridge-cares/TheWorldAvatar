package uk.ac.cam.cares.jps.agent.sealevelimpactagent;

import java.io.File;
import java.io.IOException;

import org.springframework.core.io.ClassPathResource;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import com.cmclinnovations.stack.clients.core.RESTEndpointConfig;
import com.cmclinnovations.stack.clients.geoserver.GeoServerClient;
import com.cmclinnovations.stack.clients.geoserver.GeoServerVectorSettings;
import com.cmclinnovations.stack.clients.geoserver.UpdatedGSVirtualTableEncoder;

import it.geosolutions.geoserver.rest.GeoServerRESTManager;

public class SLRGeoserver {
    static String workspaceName = "twa";
    static String styleName = "sealevelstyle";
    private static final Logger LOGGER = LogManager.getLogger(SLRGeoserver.class);

    public static void createSeaLevelGeoserver(String ssp_scenario, Integer projectionyear, String confidence,
            Integer quantile) {
        uploadStyle();
        // Create geoserver layer
        String geoservernamestring = ssp_scenario + projectionyear + confidence + quantile;
        GeoServerClient geoServerClient = GeoServerClient.getInstance();
        String schema = "public";
        geoServerClient.createWorkspace(workspaceName);

        UpdatedGSVirtualTableEncoder virtualTable = new UpdatedGSVirtualTableEncoder();
        GeoServerVectorSettings geoServerVectorSettings = new GeoServerVectorSettings();
        virtualTable.setSql(
                "SELECT ssp_scenario, confidence, quantile, projectionyear, projectionreferenceyear, sealevelriseinmeters, CONCAT('https://www.theworldavatar.com/kg/ontosealevel/SeaLevelChange/',uuid) as iri, geom FROM sealevelprojections WHERE ssp_scenario = '"
                        + ssp_scenario + "' AND LOWER(confidence) = LOWER('" + confidence + "') AND quantile= '"
                        + quantile + "' AND projectionyear= '" + projectionyear + "'");
        virtualTable.setEscapeSql(true);
        virtualTable.setName("sealevelprojection" + geoservernamestring);
        virtualTable.addVirtualTableGeometry("geom", "Geometry", "4326"); // geom needs to match the sql query
        geoServerVectorSettings.setVirtualTable(virtualTable);
        geoServerVectorSettings.setDefaultStyle(styleName);
        geoServerClient.createPostGISDataStore(workspaceName, "sealevelprojection" + geoservernamestring,
                SeaLevelImpactAgent.dbName, schema);
        geoServerClient.createPostGISLayer(workspaceName, SeaLevelImpactAgent.dbName,
                "sealevelprojection" + geoservernamestring, geoServerVectorSettings);
    }

    private static void uploadStyle() {
        RESTEndpointConfig geoserverEndpointConfig = GeoServerClient.getInstance().readEndpointConfig("geoserver",
                RESTEndpointConfig.class);
        GeoServerRESTManager manager = new GeoServerRESTManager(geoserverEndpointConfig.getUrl(),
                geoserverEndpointConfig.getUserName(), geoserverEndpointConfig.getPassword());

        if (!manager.getReader().existsStyle(workspaceName, styleName)) {
            try {
                File sldFile = new ClassPathResource("sealevelstyle.sld").getFile();
                if (manager.getPublisher().publishStyleInWorkspace(workspaceName, sldFile, styleName)) {
                    LOGGER.info("GeoServer style created");
                } else {
                    throw new RuntimeException("GeoServer style cannot be created");
                }
            } catch (IOException e) {
                LOGGER.error("Probably failed to read sld file");
                throw new RuntimeException("Probably failed to read sld file", e);
            }
        }
    }
}
