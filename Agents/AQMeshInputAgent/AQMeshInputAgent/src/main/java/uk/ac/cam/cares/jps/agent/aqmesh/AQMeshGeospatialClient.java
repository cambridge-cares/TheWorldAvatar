package uk.ac.cam.cares.jps.agent.aqmesh;

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
 * Client to instantiate aqmesh information to postGIS and geoserver
 */
public class AQMeshGeospatialClient {
	/**
        * Logger for reporting info/errors.
        */
        private static final Logger LOGGER = LogManager.getLogger(AQMeshGeospatialClient.class);

        /**
         * Instantiate geospatial information for each aqmesh in postGIS and geoserver
         * @param lat latitude of aqmesh pod
         * @param long longitude of aqmesh pod
         * @param carparkName name of aqmesh
         * @param carparkIRI aqmesh IRI
         */
	public void createGeospatialInformation(double lat, double lon, String aqmeshName, String aqmeshIRI, String sparqlEndpoint) {
	        String geomUuid = "geometry_" + UUID.randomUUID();

                // create geojson object for PostGIS
                JSONObject geojson = new JSONObject();
                JSONObject geometry = new JSONObject();
                JSONObject properties = new JSONObject();
                geometry.put("type", "Point");
                geometry.put("coordinates", new JSONArray().put(lon).put(lat));
                properties.put("iri", aqmeshIRI);
                properties.put("aqmesh_uuid", aqmeshIRI.split("/AQMesh.owl/")[1]);
                properties.put("name", aqmeshName);
                properties.put("geom_uuid", geomUuid);
                properties.put("type", "aqmesh");
                properties.put("endpoint", sparqlEndpoint);

                geojson.put("type", "Feature").put("properties", properties).put("geometry", geometry);

                LOGGER.info("Uploading GeoJSON to PostGIS");
                GDALClient gdalclient = GDALClient.getInstance();
                gdalclient.uploadVectorStringToPostGIS(System.getenv(AQMeshInputAgentLauncher.DATABASE_ENV), System.getenv(AQMeshInputAgentLauncher.LAYERNAME_ENV), geojson.toString(),
                new Ogr2OgrOptions(), true);

                LOGGER.info("Creating layer in Geoserver");
                GeoServerClient geoserverclient = new GeoServerClient(null, null, null);
                geoserverclient.createWorkspace(System.getenv(AQMeshInputAgentLauncher.GEOSERVER_WORKSPACE_ENV));
                geoserverclient.createPostGISLayer(System.getenv(AQMeshInputAgentLauncher.GEOSERVER_WORKSPACE_ENV), System.getenv(AQMeshInputAgentLauncher.DATABASE_ENV), System.getenv(AQMeshInputAgentLauncher.LAYERNAME_ENV),
                new GeoServerVectorSettings());
	}


}
