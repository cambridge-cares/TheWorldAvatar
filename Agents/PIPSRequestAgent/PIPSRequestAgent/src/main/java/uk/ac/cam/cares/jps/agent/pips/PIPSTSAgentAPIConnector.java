package uk.ac.cam.cares.jps.agent.pips;

import org.apache.http.HttpHeaders;
import org.apache.http.HttpRequest;
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
import java.io.*;

public class PIPSTSAgentAPIConnector {
    private static final Logger LOGGER = LogManager.getLogger(PIPSTSAgentAPIConnector.class);

    // variables
    private String pipsAgentTimeSeriesPath;

    // error messages
    private static final String dataRetrievalError = "Could not retrieve timeseries data!";

    /**
     * Constructor for PIPSTSAgentAPIConnector
     */
    public PIPSTSAgentAPIConnector() {
        pipsAgentTimeSeriesPath = System.getenv("PIPS_AGENT_TIMESERIES_PATH");
    }

    /**
     * Get timeseries data from pips-timeseries-agent
     * @return result of request (e.g. unauthorized, authorized, invalid token)
     * @throws JSONException
     * @throws IOException
     */
    public JSONObject getTimeSeries(String accessToken) throws JSONException, IOException {
        return retrieveTimeSeriesData(accessToken);
    }

    /**
     * send get request to pips-timeseries-agent with bearer token
     * @param accessToken token to attached as bearer token
     * @return response from pips-timeseries-agent
     * @throws IOException
     */
    private JSONObject retrieveTimeSeriesData(String accessToken) throws IOException {
        LOGGER.info("Sending request to pips-timeseries-agent...");
        try (CloseableHttpClient httpclient = HttpClients.createDefault()) {
            HttpGet readingRequest = new HttpGet(pipsAgentTimeSeriesPath);
            setTokenAuthorization(readingRequest, accessToken);

            try (CloseableHttpResponse response = httpclient.execute(readingRequest)) {
                int status = response.getStatusLine().getStatusCode();
                if (status == 200) {
                    return new JSONObject(EntityUtils.toString(response.getEntity()));
                }
                else {
                    throw new HttpResponseException(status, response.getEntity().toString());
                }
            }
        }
    }

    /**
     * Sets the current token as authorization in the header of the request
     * @param request The request to which to add the token authorization
     */
    private void setTokenAuthorization(HttpRequest request, String accessToken) {
        String authHeader = "Bearer " + accessToken;
        request.setHeader(HttpHeaders.AUTHORIZATION, authHeader);
    }

}
