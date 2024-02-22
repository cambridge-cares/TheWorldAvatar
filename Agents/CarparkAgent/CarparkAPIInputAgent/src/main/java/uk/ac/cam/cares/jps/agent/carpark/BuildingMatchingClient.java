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

public class BuildingMatchingClient {
    private static final String BUILDING_MATCHING_ERROR = "Unable to execute building identification/matching for the carparks!" ;
    private static final Logger LOGGER = LogManager.getLogger(BuildingMatchingClient.class);

    private String buildingIdentificationAgentEndpoint;
    private static final String CARPARK_TABLE = "public." + CarparkAgent.LAYERNAME;
    private static final String CARPARK_GEOM_COLUMN = "wkb_geometry";

    public BuildingMatchingClient(String filePath) throws IOException {
        readBuildingMatchingProperties(filePath);
    }

    //building identification agent endpoint - read via properties file
    //prefix for building IRI - read via properties file
    private void readBuildingMatchingProperties(String filePath) throws IOException {
        Queue<String> buildingConfigs = ConfigReader.retrieveBuildingMatchingConfig(filePath);
        this.buildingIdentificationAgentEndpoint =  buildingConfigs.poll();
    }

    public void matchCarparkToBuilding() {
        JSONObject requestBody = new JSONObject();
        requestBody.put("table", CARPARK_TABLE);
        requestBody.put("column", CARPARK_GEOM_COLUMN);
        JSONObject buildingMatchingInformation = sendPostRequest(requestBody.toString());
        LOGGER.info("The number of matches are " +  buildingMatchingInformation.getInt("number_matched"));
    }

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
