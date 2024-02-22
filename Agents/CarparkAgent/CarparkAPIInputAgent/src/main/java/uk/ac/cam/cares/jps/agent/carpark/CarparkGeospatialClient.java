package uk.ac.cam.cares.jps.agent.carpark;

import java.util.UUID;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONArray;
import org.json.JSONObject;

import com.cmclinnovations.stack.clients.gdal.GDALClient;
import com.cmclinnovations.stack.clients.gdal.Ogr2OgrOptions;
import com.cmclinnovations.stack.clients.geoserver.GeoServerClient;
import com.cmclinnovations.stack.clients.geoserver.GeoServerVectorSettings;

/**
 * Client to instantiate carpark information to postGIS and geoserver
 */
public class CarparkGeospatialClient {
	/**
     * Logger for reporting info/errors.
     */

        private static final Logger LOGGER = LogManager.getLogger(CarparkAgent.class);

	public void createGeospatialInformation(double lat, double lon, String carparkName, String carparkIRI) {
	        String geomUuid = "geometry_" + UUID.randomUUID();

                // create geojson object for PostGIS
                JSONObject geojson = new JSONObject();
                JSONObject geometry = new JSONObject();
                JSONObject properties = new JSONObject();
                geometry.put("type", "Point");
                geometry.put("coordinates", new JSONArray().put(lon).put(lat));
                properties.put("carpark_iri", carparkIRI);
                properties.put("carpark_uuid", carparkIRI.split("/ontocarpark/")[1]);
                properties.put("carpark_name", carparkName);
                properties.put("geom_uuid", geomUuid);
                properties.put("type", "carpark");

                geojson.put("type", "Feature").put("properties", properties).put("geometry", geometry);

                LOGGER.info("Uploading GeoJSON to PostGIS");
                GDALClient gdalclient = GDALClient.getInstance();
                gdalclient.uploadVectorStringToPostGIS(CarparkAgent.DATABASE, CarparkAgent.LAYERNAME, geojson.toString(),
                new Ogr2OgrOptions(), true);

                LOGGER.info("Creating layer in Geoserver");
                GeoServerClient geoserverclient = new GeoServerClient(null, null, null);
                geoserverclient.createWorkspace(CarparkAgent.GEOSERVER_WORKSPACE);
                geoserverclient.createPostGISLayer(CarparkAgent.GEOSERVER_WORKSPACE, CarparkAgent.DATABASE, CarparkAgent.LAYERNAME,
                new GeoServerVectorSettings());
	}


}
