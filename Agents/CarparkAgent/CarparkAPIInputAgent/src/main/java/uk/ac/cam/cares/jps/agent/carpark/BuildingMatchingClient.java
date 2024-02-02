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
import org.json.JSONArray;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.agent.carpark.file.ConfigReader;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

public class BuildingMatchingClient {
    private static final String BUILDING_MATCHING_ERROR = "Unable to execute building identification/matching for the following latitude, longitude values: " ;
    private static final Logger LOGGER = LogManager.getLogger(BuildingMatchingClient.class);

    //latitude and longitude of carpark to match
    private String carparkLatitude;
    private String carparkLongitude;
    private String buildingIdentificationAgentEndpoint;
    private String buildingPrefix;

    public BuildingMatchingClient(String latitude, String longitude, String filePath) throws IOException {
        this.carparkLatitude = latitude;
        this.carparkLongitude = longitude;
        readBuildingMatchingProperties(filePath);
    }

    //building identification agent endpoint - read via properties file
    //prefix for building IRI - read via properties file
    private void readBuildingMatchingProperties(String filePath) throws IOException {
        Queue<String> buildingConfigs = ConfigReader.retrieveBuildingMatchingConfig(filePath);
        this.buildingIdentificationAgentEndpoint =  buildingConfigs.poll();
        this.buildingPrefix =  buildingConfigs.poll();
    }

    public String matchCarparkToBuilding() {
        JSONObject requestBody = new JSONObject();
        JSONArray coordinateArray = new JSONArray();
        JSONArray subArray = new JSONArray();
        subArray.put(carparkLongitude);
        subArray.put(carparkLatitude);
        coordinateArray.put(subArray);
        requestBody.put("coordinates", coordinateArray);
        JSONObject buildingMatchingInformation = sendPostRequest(requestBody.toString());
        String buildingIRI = constructBuildingIRI(buildingMatchingInformation);
        return buildingIRI;
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
                    JSONObject jsonObject = new JSONObject(jsonObjectString);
                    LOGGER.fatal(BUILDING_MATCHING_ERROR + jsonObject.getJSONArray("coordinates").toString());
                    throw new HttpResponseException(status, BUILDING_MATCHING_ERROR + jsonObject.getJSONArray("coordinates").toString());
                }
            }
        } catch (Exception e) {
            throw new JPSRuntimeException(BUILDING_MATCHING_ERROR, e);
        }
    }

    //return building IRI
    private String constructBuildingIRI(JSONObject buildingMatchingInformation) {
        JSONArray building_uuid_array = buildingMatchingInformation.getJSONArray("building_iri");
        String building_uuid = building_uuid_array.getString(0);
        String building_iri = buildingPrefix + building_uuid;
        return building_iri;
    }
}
