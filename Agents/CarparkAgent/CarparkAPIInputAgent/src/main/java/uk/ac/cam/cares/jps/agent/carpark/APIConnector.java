package uk.ac.cam.cares.jps.agent.carpark;

import org.apache.http.client.HttpResponseException;
import org.apache.http.client.methods.CloseableHttpResponse;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClients;
import org.apache.http.util.EntityUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONException;
import org.json.JSONObject;
import uk.ac.cam.cares.jps.agent.carpark.file.ConfigReader;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

import java.io.*;
import java.util.Queue;

public class APIConnector {
    private String lotApiEndpoint = "http://datamall2.mytransport.sg/ltaodataservice/CarParkAvailabilityv2";
    private String lotApiToken;
    private String pricingApiEndpoint = "https://data.gov.sg/api/action/datastore_search?resource_id=85207289-6ae7-4a56-9066-e6090a3684a5&limit=357";
    private String date;

    private static final String ERRORMSG = "Carpark data could not be retrieved";
    private static final Logger LOGGER = LogManager.getLogger(APIConnector.class);

    //Standard Constructor to initialise the instance variables
    public APIConnector(String URL, String d, String k, String Pricing_URL) {
        lotApiEndpoint = URL;
        lotApiToken = k;
        pricingApiEndpoint = Pricing_URL;
        date = d;
    }

    //Constructor to initialise the variables according to the Properties file
    public APIConnector(String filepath) throws IOException {
        Queue<String> apiConfigs = ConfigReader.retrieveAPIConfig(filepath);
        this.lotApiEndpoint = apiConfigs.poll();
        this.lotApiToken = apiConfigs.poll();
        this.pricingApiEndpoint = apiConfigs.poll();
    }

    // Obtains Weather data in JSON format containing key:value pairs
    public JSONObject getReadings() {
        try {
            return retrieveData();
        } catch (IOException e) {
            LOGGER.error(ERRORMSG, e);
            throw new JPSRuntimeException(ERRORMSG, e);
        }
    }

    public JSONObject getPrices() {
        try {
            return retreivePricingData();
        } catch (Exception e) {
            LOGGER.error(ERRORMSG, e);
            throw new JPSRuntimeException(ERRORMSG, e);
        }
    }

    private JSONObject retrieveData() throws IOException, JSONException {
        String path = lotApiEndpoint;
        try (CloseableHttpClient httpclient = HttpClients.createDefault()) {
            HttpGet readrequest = new HttpGet(path);
            readrequest.setHeader("AccountKey", lotApiToken);
            try (CloseableHttpResponse response = httpclient.execute(readrequest)) {
                int status = response.getStatusLine().getStatusCode();
                if (status == 200) {
                    return new JSONObject(EntityUtils.toString(response.getEntity()));
                } else {
                    LOGGER.fatal("Data could not be retrieved due to a server error");
                    throw new HttpResponseException(status, "Data could not be retrieved due to a server error");
                }
            }
        }
    }

    private JSONObject retreivePricingData() throws IOException, JSONException {
        String path = pricingApiEndpoint;

        try (CloseableHttpClient httpClient = HttpClients.createDefault()) {
            HttpGet readrequest = new HttpGet(path);
            try (CloseableHttpResponse response = httpClient.execute(readrequest)) {
                int status = response.getStatusLine().getStatusCode();
                if (status == 200) {
                    return new JSONObject(EntityUtils.toString(response.getEntity()));
                } else {
                    LOGGER.fatal("Pricing Data could not be retrieved due to a server error");
                    throw new HttpResponseException(status, "Pricing Data could not be retrieved due to a server error");
                }
            }
        }
    }
}
