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

public class CarparkAPIConnector {
    private String lotApiEndpoint;
    private String lotApiToken;
    private String pricingApiEndpoint;

    private static final String RATES_DATA_RETRIEVAL_ERROR = "Carpark rates data could not be retrieved";
    private static final String AVAILABLE_LOTS_DATA_RETRIEVAL_ERROR = "Carpark available lots data could not be retrieved";
    private static final Logger LOGGER = LogManager.getLogger(CarparkAPIConnector.class);

    /**
     * Standard Constructor for APIConnector
     * @param availableLotsUrl endpoint URL to retrieve the available lots data
     * @param availableLotsAPIToken API token for accessing the endpoint
     * @param ratesUrl endpoint URL to retrieve the carpark rates data
     */
    public CarparkAPIConnector(String availableLotsUrl, String availableLotsAPIToken, String ratesUrl) {
        lotApiEndpoint = availableLotsUrl ;
        lotApiToken = availableLotsAPIToken;
        pricingApiEndpoint = ratesUrl;
    }

    /**
     * Standard Constructor for APIConnector
     * @param filepath filepath to file that contains properties for connecting to the APIs
     * @throws IOException
     */
    public CarparkAPIConnector(String filepath) throws IOException {
        Queue<String> apiConfigs = ConfigReader.retrieveAPIConfig(filepath);
        this.lotApiEndpoint = apiConfigs.poll();
        this.lotApiToken = apiConfigs.poll();
        this.pricingApiEndpoint = apiConfigs.poll();
    }

    /**
     * Get available carpark lots data
     * @return Current available carpark lots data as a JSONObject
     * @throws JSONException
     * @throws IOException
     */
    public JSONObject getAvailableLots() throws JSONException, IOException {
        return retrieveAvailableLotsData();
    }

    /**
     * Get carpark rates data
     * @return Current carpark rates data as a JSONObject
     * @throws JSONException
     * @throws IOException
     */
    public JSONObject getCarparkRates() throws JSONException, IOException {
        return retreiveRatesData();
    }

    /**
     * Retrieve carparks available lots data
     * @return carpark availabe lots data as a JSONObject
     */
    private JSONObject retrieveAvailableLotsData() {
        try (CloseableHttpClient httpclient = HttpClients.createDefault()) {
            HttpGet readrequest = new HttpGet(lotApiEndpoint);
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
        } catch (Exception e) {
            throw new JPSRuntimeException(AVAILABLE_LOTS_DATA_RETRIEVAL_ERROR, e);
        }
    }

    /**
     * Retrieve carpark rates data
     * @return carpark rates data as a JSONObject
     */
    private JSONObject retreiveRatesData() {
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
        } catch (Exception e) {
            throw new JPSRuntimeException(RATES_DATA_RETRIEVAL_ERROR, e);
        }
    }
}
