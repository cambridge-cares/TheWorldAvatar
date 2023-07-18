package uk.ac.cam.cares.jps.agent.thingspeak;

import org.apache.http.HttpRequest;
import org.apache.http.client.HttpResponseException;
import org.apache.http.client.methods.CloseableHttpResponse;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.entity.ContentType;
import org.apache.http.entity.StringEntity;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClients;
import org.apache.http.util.EntityUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONException;
import org.json.JSONArray;
import org.json.JSONObject;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import java.io.*;
import java.util.Properties;


public class ThingspeakAPIConnector {

    /**
     * Fields to access API
     */
    private String channelNumber;
    private String APIKey;
    private String results;
    String pathUrl = "https://api.thingspeak.com/channels/" ;
    
    /**
     * Logger for reporting info/errors.
     */
    private static final Logger LOGGER = LogManager.getLogger(ThingspeakInputAgentLauncher.class);
    /**
     * Error messages
     */
    private static final String CONNECTION_ERROR_MSG = "Unable to connect to Thingspeak API!";
    private static final String READINGS_ERROR_MSG ="Readings could not be retrieved";
    
    /**
     * Keys to be retrieved
     */
    private String keys;

    /**
     * Standard constructor
     * @param channelNumber the channel to retrieve readings from
     * @param APIKey the key required to read readings from the Thingspeak channel
     * @param results the number of readings to retrieve from the channel
     */
    public ThingspeakAPIConnector(String channelNumber, String APIKey, String results, String url) {
        this.channelNumber = channelNumber;
        this.APIKey = APIKey;
        this.results = results;
        this.pathUrl = url;
        
    }


    /**
     * Constructor using a properties file
     * @param filepath Path to the properties file from which to read the channel number, API Key and number of results
     */
    public ThingspeakAPIConnector(String filepath) throws IOException {
        loadAPIConfigs(filepath);
    }

    /**
     * Retrieves the latest readings from the Thingspeak API
     * @return a JSON Object containing key-value pairs
     */
    public JSONObject getAllReadings() {
        try {
            return retrieveReadings();
        }
        catch (IOException | JSONException e) {
            LOGGER.error(READINGS_ERROR_MSG, e);
            throw new JPSRuntimeException(READINGS_ERROR_MSG, e);
        }
    }

    /**
     * Retrieves the latest readings from the Thingspeak API
     * @return Readings in a JSON Object
     */
    private JSONObject retrieveReadings() throws IOException, JSONException {
    	
        if (!APIKey.contains("None")) {
    	String basicReadingPath = pathUrl + channelNumber + "/feeds.json?api_key=" + APIKey + "&results=" + results;     
        
        try (CloseableHttpClient httpclient = HttpClients.createDefault()) {
            HttpGet readingRequest = new HttpGet(basicReadingPath);

            try (CloseableHttpResponse response = httpclient.execute(readingRequest)) {
                int status = response.getStatusLine().getStatusCode();
                if (status == 200) {
                    return new JSONObject(EntityUtils.toString(response.getEntity()));
                    
                }
                else {
                    throw new HttpResponseException(status, "Could not retrieve readings due to server error.");
                }
            }
        }
    } 
        else {
    	String basicReadingPath = pathUrl +  channelNumber + "/feeds.json?results=" + results;     
        
        try (CloseableHttpClient httpclient = HttpClients.createDefault()) {
            HttpGet readingRequest = new HttpGet(basicReadingPath);

            try (CloseableHttpResponse response = httpclient.execute(readingRequest)) {
                int status = response.getStatusLine().getStatusCode();
                if (status == 200) {
                    return new JSONObject(EntityUtils.toString(response.getEntity()));
                    
                }
                else {
                    throw new HttpResponseException(status, "Could not retrieve readings due to server error.");
                }
            }
        }
        }
        }
   
   
    /**
     * Reads the parameters needed to connect to the API from a properties file and saves it in fields.
     * @param filepath Path to the properties file from which to read the parameters
     */
    private void loadAPIConfigs(String filepath) throws IOException {
        // Check whether properties file exists at specified location
        File file = new File(filepath);
        if (!file.exists()) {
            throw new FileNotFoundException("No properties file found at specified filepath: " + filepath);
        }
        // Try-with-resource to ensure closure of input stream
        try (InputStream input = new FileInputStream(file)) {

            // Load properties file from specified path
            Properties prop = new Properties();
            prop.load(input);

            // Get username, password, auth_url, api_url and device_token from properties file
            if (prop.containsKey("thingspeak.channelNumber")) {
                this.channelNumber = prop.getProperty("thingspeak.channelNumber");
            } else {
                throw new IOException("Properties file is missing \"thingspeak.channelNumber=<thingspeak_channelNumber>\"");
            }
            if (prop.containsKey("thingspeak.apiKey")) {
                this.APIKey = prop.getProperty("thingspeak.apiKey");
            } 
            
            if (prop.containsKey("thingspeak.results")) {
                this.results = prop.getProperty("thingspeak.results");
            } else {
                throw new IOException("Properties file is missing \"thingspeak.results=<results>\"");
            }
            
            if (prop.containsKey("path.url")) {
                this.pathUrl = prop.getProperty("path.url");
            } else {
                throw new IOException("Properties file is missing \"path.url=<path_url>\"");
            }

        }
}
}
