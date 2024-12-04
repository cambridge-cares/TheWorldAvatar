package uk.ac.cam.cares.jps.agent.trafficincident;

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
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import java.io.*;

public class APIConnector {
    private String API_URL;
    private String accountKey;

    public static final String ERROR_MSG = "APIConnector failed while retrieving readings.";
    public static final Logger logger = LogManager.getLogger(TrafficIncidentAgent.class);

    public APIConnector(String URL, String key) {
        this.API_URL = URL;
        this.accountKey = key;
    }

    public JSONObject getReadings() {
        try {
            return retrieveData();
        } catch (IOException e) {
            logger.error(ERROR_MSG);
            throw new JPSRuntimeException(ERROR_MSG, e);
        }
    }

    private JSONObject retrieveData() throws IOException, JSONException {
        String path = this.API_URL;
        CloseableHttpClient httpClient = HttpClients.createDefault();
        HttpGet readRequest = new HttpGet(path);
        readRequest.setHeader("AccountKey", this.accountKey);
        readRequest.setHeader("accept", "application/json");
        CloseableHttpResponse response = httpClient.execute(readRequest);
        int status = response.getStatusLine().getStatusCode();
        if (status == 200) {
            return new JSONObject(EntityUtils.toString(response.getEntity()));
        } else {
            throw new HttpResponseException(status, " Data could not be retrieved.");
        }
    }
}
