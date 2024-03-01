package uk.ac.cam.cares.jps.agent.carpark;

import java.io.IOException;
import java.util.Queue;

import org.apache.http.client.HttpResponseException;
import org.apache.http.client.methods.CloseableHttpResponse;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.entity.ContentType;
import org.apache.http.entity.StringEntity;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClients;
import org.apache.http.util.EntityUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.agent.carpark.file.ConfigReader;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

//Class to contact the building identification agent to match carparks to buildings
public class BuildingMatchingClient {
    private static final String BUILDING_MATCHING_ERROR = "Unable to execute building identification/matching for the carparks!" ;
    private static final Logger LOGGER = LogManager.getLogger(BuildingMatchingClient.class);

    private String buildingIdentificationAgentEndpoint;
    private static final String CARPARK_TABLE =CarparkAgent.LAYERNAME;
    private static final String CARPARK_GEOM_COLUMN = "wkb_geometry";

    public BuildingMatchingClient(String filePath) throws IOException {
        readBuildingMatchingProperties(filePath);
    }

    /**
     * Read buildingIdentificationAgent endpoint
     */
    private void readBuildingMatchingProperties(String filePath) throws IOException {
        Queue<String> buildingConfigs = ConfigReader.retrieveBuildingMatchingConfig(filePath);
        this.buildingIdentificationAgentEndpoint =  buildingConfigs.poll();
    }

    /**
     * Match carparks to buildings via the building identification agent postGIS route
     */
    public void matchCarparkToBuilding() {
        JSONObject requestBody = new JSONObject();
        requestBody.put("table", CARPARK_TABLE);
        requestBody.put("column", CARPARK_GEOM_COLUMN);
        JSONObject buildingMatchingInformation = sendPostRequest(requestBody.toString());
        LOGGER.info("The number of matches are " +  buildingMatchingInformation.getInt("number_matched"));
    }

    /**
    * Send post request to building identification agent
    * @param jsonObjectString json object string containing variables for the postGIS route
    * @return A response to the request called as a JSON Object
    */
    private JSONObject sendPostRequest(String jsonObjectString) {
        StringEntity requestEntity = new StringEntity(jsonObjectString, ContentType.APPLICATION_JSON);
        try (CloseableHttpClient httpClient = HttpClients.createDefault()) {
            HttpPost request = new HttpPost(buildingIdentificationAgentEndpoint);
            request.setEntity(requestEntity);
            try (CloseableHttpResponse response = httpClient.execute(request)) {
                int status = response.getStatusLine().getStatusCode();
                if (status == 200) {
                    return new JSONObject(EntityUtils.toString(response.getEntity()));
                } else {
                    LOGGER.fatal(BUILDING_MATCHING_ERROR);
                    throw new HttpResponseException(status, BUILDING_MATCHING_ERROR);
                }
            }
        } catch (Exception e) {
            throw new JPSRuntimeException(BUILDING_MATCHING_ERROR, e);
        }
    }
}
