package uk.ac.cam.cares.jps.agent.bmsupdate;

import org.apache.http.client.methods.CloseableHttpResponse;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.methods.HttpPut;
import org.apache.http.entity.StringEntity;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClients;
import org.apache.http.util.EntityUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.apache.http.entity.ContentType;
import org.apache.http.client.HttpResponseException;
import org.json.JSONObject;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.util.Properties;

public class BMSWacnetAPIConnector {

    private static final Logger LOGGER = LogManager.getLogger(BMSWacnetAPIConnector.class);

    final String FAIL_TO_WRITE = "Fail to write the following value: ";

    private String api_url;

    /**
     * Constructor using a properties file
     * @param filepath Path to the properties file from which to read the api_url
     */
    public BMSWacnetAPIConnector(String filepath) throws IOException {
        loadAPIConfigs(filepath);
    }

    public JSONObject writePresentValue(String deviceId, String objectId, Double value) throws IOException {
        JSONObject responseBody = new JSONObject();
        try (CloseableHttpClient httpclient = HttpClients.createDefault()) {
            //http://localhost:47800/api/v1/bacnet/devices/<deviceId>/objects/<objectId>
            HttpPut putRequest = new HttpPut(api_url + "/devices/" + deviceId + "/objects/" +objectId);   
            JSONObject body = new JSONObject();
            JSONObject presentValue = new JSONObject();
            //"{\"properties\":{\"present-value\":\"700\"}}"
            presentValue.put("present-value", value); 
            body.put("properties", presentValue);
            putRequest.setEntity(new StringEntity(body.toString(), ContentType.APPLICATION_JSON));
                    		
            try (CloseableHttpResponse response = httpclient.execute(putRequest)) {
                int status = response.getStatusLine().getStatusCode();
                switch (status) {
                    case 400:
                    LOGGER.error(status + "(error in writing present value!)");
                    throw new HttpResponseException(status, "error in writing present value!");
                    case 401:
                    LOGGER.error(status + "(unathorized! Bacnet point is not writable!)");
                    throw new HttpResponseException(status, "unathorized! Bacnet point is not writable!");
                    case 200:
                        responseBody.put("message", "Successfully written " + value + " to the object with an ID: " + objectId);
                        return responseBody;
                    default:
                        throw new HttpResponseException(status, "Error!.");
                }
            }
        }
    }

    public Double readPresentValue(String deviceId, String objectId) throws IOException {
        JSONObject responseBody = new JSONObject();
        Double value;
        try (CloseableHttpClient httpclient = HttpClients.createDefault()) {
            //http://localhost:47800/api/v1/bacnet/devices/<deviceId>/objects/<objectId>
            HttpGet getRequest = new HttpGet(api_url + "/devices/" + deviceId + "/objects/" +objectId);
                    		
            try (CloseableHttpResponse response = httpclient.execute(getRequest)) {
                int status = response.getStatusLine().getStatusCode();
                switch (status) {
                    case 400:
                    LOGGER.error(status + "(error in reading present value!)");
                    throw new HttpResponseException(status, "error in reading present value!");
                    case 401:
                    LOGGER.error(status + "(unathorized! Bacnet point is not Readable!)");
                    throw new HttpResponseException(status, "unathorized! Bacnet point is not Readable!");
                    case 200:
                        responseBody = new JSONObject(EntityUtils.toString(response.getEntity()));
                        value = responseBody.getDouble("present-value");
                        return value;
                    default:
                        throw new HttpResponseException(status, "Error");
                }
            }
        }
    }

    /**
     * Reads the api_url needed to connect to the API from a properties file and saves it in fields.
     * @param filepath Path to the properties file
     */
    private void loadAPIConfigs(String filepath) throws IOException {
        // Check whether properties file exists at specified location
        File file = new File(filepath);
        if (!file.exists()) {
            throw new FileNotFoundException("No properties file found at specified filepath: " + filepath);
        }
        // Read api_url from properties file
        try (InputStream input = new FileInputStream(file)) {

            // Load properties file from specified path
            Properties prop = new Properties();
            prop.load(input);
            //api.url should have this format: http://localhost:47800/api/v1/bacnet
            if (prop.containsKey("api.url")) {
                this.api_url = prop.getProperty("api.url");
            } else {
                throw new IOException("Properties file is missing \"api.url=<api_url>\"");
            }
        }
}
}
