package uk.ac.cam.cares.jps.agent.rfid;

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


public class RFIDUpdateAPIConnector {

    private String limit;
    private String variable;
    private String path_url;
    
    /**
     * Logger for reporting info/errors.
     */
    private static final Logger LOGGER = LogManager.getLogger(RFIDUpdateAgentLauncher.class);
    /**
     * Error messages
     */
    private static final String CONNECTION_ERROR_MSG = "Unable to connect to RFID API!";
    private static final String READINGS_ERROR_MSG ="RFID readings could not be retrieved";
    
    /**
     * Keys to be retrieved from RFID API
     */
    private String keys;

    /**
     * Standard constructor
     * @param username the username to access ThingsBoard API
     * @param password the password to access ThingsBoarrd API
     * @param path_url the port that is hosting the ThingsBoard server
     * @param device_token the token required to retrieve readings from a specific device
     */
    public RFIDUpdateAPIConnector(String limit, String variable, String path_url, String keys) {
        this.limit = limit;
        this.variable = variable;
        this.path_url = path_url;
        this.keys = keys;    
    }


    /**
     * Constructor using a properties file
     * @param filepath Path to the properties file from which to read the limit, path_url, variable and keys
     */
    public RFIDUpdateAPIConnector(String filepath) throws IOException {
        loadAPIConfigs(filepath);
    }

    /**
     * Retrieves the latest readings from the RFID API
     * @return a JSON Object containing key-value pairs with the keys being the tag IDs, values being a JSON Array
     * containing multiple JSON Objects with each object containing two key-value pairs where the keys are "ts" and RSSI/status/antennanumber.
     */
    public JSONObject getAllReadings() {
        try {
            return retrieveReadings(keys);
        }
        catch (IOException | JSONException e) {
            LOGGER.error(READINGS_ERROR_MSG, e);
            throw new JPSRuntimeException(READINGS_ERROR_MSG, e);
        }
    }

    /**
     * Retrieves the latest readings from the RFID API
     * @param readingType Specifies the type of readings 
     * @return Readings in a JSON Object with multiple key-value pairs
     */
    private JSONObject retrieveReadings(String readingType) throws IOException, JSONException {

        
    	String readingPath = String.join("/",path_url, "values="+variable, "limit="+limit, "keys="+keys);
    	System.out.println(readingPath);
       
        
        try (CloseableHttpClient httpclient = HttpClients.createDefault()) {
            HttpGet readingRequest = new HttpGet(readingPath);

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
   
   
    /**
     * Reads the username, password, auth_url, api_url and device_token needed to connect to the API from a properties file and saves it in fields.
     * @param filepath Path to the properties file from which to read the username, password, auth_url, api_url and device_id
     */
    private void loadAPIConfigs(String filepath) throws IOException {
        // Check whether properties file exists at specified location
        File file = new File(filepath);
        if (!file.exists()) {
            throw new FileNotFoundException("No properties file found at specified filepath: " + filepath);
        }
        // Read username and password for ThingsBoard API from properties file
        // Try-with-resource to ensure closure of input stream
        try (InputStream input = new FileInputStream(file)) {

            // Load properties file from specified path
            Properties prop = new Properties();
            prop.load(input);

            // Get username, password, auth_url, api_url and device_token from properties file
            if (prop.containsKey("limit")) {
                this.limit = prop.getProperty("limit");
            } else {
                throw new IOException("Properties file is missing \"limit=<limit>\"");
            }
            if (prop.containsKey("variable")) {
                this.variable = prop.getProperty("variable");
            } else {
                throw new IOException("Properties file is missing \"variable=<variable>\"");
            }
            if (prop.containsKey("path.url")) {
                this.path_url = prop.getProperty("path.url");
            } else {
                throw new IOException("Properties file is missing \"path.url=<path_url>\"");
            }
            if (prop.containsKey("keys")) {
                this.keys = prop.getProperty("keys");
            } else {
                throw new IOException("Properties file is missing \"keys=<keys>\"");
            }
        }
}
}
