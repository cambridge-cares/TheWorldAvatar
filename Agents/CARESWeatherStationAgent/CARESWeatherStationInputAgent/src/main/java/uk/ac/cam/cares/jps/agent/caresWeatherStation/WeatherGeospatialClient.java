package uk.ac.cam.cares.jps.agent.caresWeatherStation;

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
 * Client to instantiate geolocation data (lat, long etc) to postGIS and geoserver
 */
public class WeatherGeospatialClient {
	/**
     * Logger for reporting info/errors.
     */

	private static final Logger LOGGER = LogManager.getLogger(CARESWeatherStationInputAgentLauncher.class);

	/**
	 * Create geospatial information in the servers such as geoserver, postGIS etc
	 * @param lat latitide of the weather station
	 * @param lon longitude of the weather station
	 * @param name user defined name for the weather station
	 * @param reportingStationIRI IRI of the weather station instance
	 */
	public void createGeospatialInformation(double lat, double lon, String name, String reportingStationIRI) {
		String geomUuid = "geometry_" + UUID.randomUUID();

        // create geojson object for PostGIS
        JSONObject geojson = new JSONObject();
        JSONObject geometry = new JSONObject();
        JSONObject properties = new JSONObject();
        geometry.put("type", "Point");
        geometry.put("coordinates", new JSONArray().put(lon).put(lat));
        properties.put("iri", reportingStationIRI);
        properties.put("station_uuid", reportingStationIRI.split("/ontoems/")[1]);
        properties.put("geom_uuid", geomUuid);
        properties.put("type", "weather");
        if (name != null) {
            properties.put("name", name);
        } else {
            properties.put("name", String.format("Weather Station at (%f, %f)", lat, lon));
        }

        geojson.put("type", "Feature").put("properties", properties).put("geometry", geometry);

        LOGGER.info("Uploading GeoJSON to PostGIS");
        GDALClient gdalclient = GDALClient.getInstance();
        gdalclient.uploadVectorStringToPostGIS(CARESWeatherStationInputAgentLauncher.DATABASE, CARESWeatherStationInputAgentLauncher.LAYERNAME, geojson.toString(),
                new Ogr2OgrOptions(), true);

        LOGGER.info("Creating layer in Geoserver");
        GeoServerClient geoserverclient = new GeoServerClient(null, null, null);
        geoserverclient.createWorkspace(CARESWeatherStationInputAgentLauncher.GEOSERVER_WORKSPACE);
        geoserverclient.createPostGISLayer(CARESWeatherStationInputAgentLauncher.GEOSERVER_WORKSPACE, CARESWeatherStationInputAgentLauncher.DATABASE, CARESWeatherStationInputAgentLauncher.LAYERNAME,
                new GeoServerVectorSettings());
	}


}
