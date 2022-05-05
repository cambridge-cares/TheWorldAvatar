package uk.ac.cam.cares.jps.agent.thingsboard;

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


public class ThingsBoardAPIConnector {

    /**
     * Fields to access API
     */
    private String username;
    private String password;
    private String path_url;
    private String device_id;
    
    /**
     * Variables for retrieving historical timeseries data from ThingsBoard
     */
    private long startTs;
    private long endTs;
    
     /**
     * Token needed for all API calls (except to retrieve token)
     */
    private String token = "";
    
    protected static final String X_AUTHORIZATION = "X-Authorization";
    /**
     * Specific keys in response bodies
     */
    protected static final String TOKEN_KEY = "token";
    /**
     * Logger for reporting info/errors.
     */
    private static final Logger LOGGER = LogManager.getLogger(ThingsBoardInputAgentLauncher.class);
    /**
     * Error messages
     */
    private static final String CONNECTION_ERROR_MSG = "Unable to connect to ThingsBoard API!";
    private static final String READINGS_ERROR_MSG ="Fridge electrical readings, Temperature and Humidity readings could not be retrieved";
    
    /**
     * Keys to be retrieved from ThingsBoard
     */
    private String keys;

    /**
     * Standard constructor
     * @param username the username to access ThingsBoard API
     * @param password the password to access ThingsBoarrd API
     * @param path_url the port that is hosting the ThingsBoard server
     * @param device_token the token required to retrieve readings from a specific device
     */
    public ThingsBoardAPIConnector(String username, String password, String path_url, String device_id, String keys) {
        this.username = username;
        this.password = password;
        this.path_url = path_url;
        this.device_id = device_id;
        this.keys = keys;
        
        
    }


    /**
     * Constructor using a properties file
     * @param filepath Path to the properties file from which to read the username, password, auth_url, api_url and device_token
     */
    public ThingsBoardAPIConnector(String filepath) throws IOException {
        loadAPIConfigs(filepath);
    }

    /**
     * Connects to the ThingsBoard API by retrieving and setting an access token that is required for all other API calls for authentication
     */
    public void connect() {
        try {
            token = getAccessToken();
            String status = checkStatus();
            if (token!= null) {
            LOGGER.info("Token successfully obtained. Is server receiving readings from device? " + status + " Token will be valid for 150 minutes.");
            }
        } catch (IOException e) {
            LOGGER.error(CONNECTION_ERROR_MSG, e);
            throw  new JPSRuntimeException(CONNECTION_ERROR_MSG, e);
        }
    }

    /**
     * Get method for the access token. String is empty if connect() was not run
     * @return The access token as string
     */
    public String getToken() {
        return token;
    }
    /**
     * Retrieves an access token from the ThingsBoard API
     * @return The access token as string
     */
    private String getAccessToken() throws IOException {
        try (CloseableHttpClient httpclient = HttpClients.createDefault()) {
            HttpPost postRequest = new HttpPost(path_url + "/api/auth/login");
            JSONObject body = new JSONObject();
            body.put("username", username);
            body.put("password", password);
            postRequest.setEntity(new StringEntity(body.toString(), ContentType.APPLICATION_JSON));
                    		
            try (CloseableHttpResponse response = httpclient.execute(postRequest)) {
                int status = response.getStatusLine().getStatusCode();
                switch (status) {
                    case 400:
                    case 401:
                        throw new HttpResponseException(status, "Invalid username or password.");
                    case 200:
                        JSONObject responseBody = new JSONObject(EntityUtils.toString(response.getEntity()));
                        return responseBody.get(TOKEN_KEY).toString();
                    default:
                        throw new HttpResponseException(status, "Could not retrieve access token.");
                }
            }
        }
    }

    /**
     * Method to test whether the API is accessible after the token was set
     * @return server status
     */
    public String checkStatus() throws IOException {
    	boolean status = false;
    	
    	String serverStatus = null;
        if (token.equals("")) {
            throw new JPSRuntimeException("Token is not set. Use the connect method first.");
        }

        try (CloseableHttpClient httpclient = HttpClients.createDefault()) {
            HttpGet attributesRequest = new HttpGet(path_url + "/api/plugins/telemetry/DEVICE/" + device_id + "/values/attributes");
            setTokenAuthorization(attributesRequest);

            try (CloseableHttpResponse response = httpclient.execute(attributesRequest)) { 
                    if (response.getStatusLine().getStatusCode() != 200) {
                        throw new HttpResponseException(response.getStatusLine().getStatusCode(),
                                "Unexpected status code.");
                    }
                    else {
            	JSONArray checkAttributes = new JSONArray (EntityUtils.toString(response.getEntity()));
            	//navigate through the JSONObject to retrieve the status value, the status will either be true or false
                for (int j = 0; j < checkAttributes.length(); j++) {
            	JSONObject attributes = checkAttributes.getJSONObject(j);
            	if 
            	(attributes.get("value").toString() == "true" || attributes.get("value").toString() == "false") {
                status = attributes.getBoolean("value");
            	}
            	}
            	}
                serverStatus = "is server status active: "+ status;
        }   
        }
		return serverStatus;
    }
    /**
     * Sets the current token as authorization in the header of the request
     * @param request The request to which to add the token authorization
     */
    private void setTokenAuthorization(HttpRequest request) {
        String authHeader = "Bearer " + token;
        request.setHeader("X-Authorization", authHeader);
    }

    /**
     * Retrieves the latest electrical readings from the ThingsBoard API
     * @return a JSON Object containing key-value pairs with the keys being Current, Voltage etc, values being a JSON Array
     * containing multiple JSON Objects with each object containing two key-value pairs where the keys are "ts" and "value".
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
     * Retrieves the latest readings from the ThingsBoard API
     * @param readingType Specifies the type of readings (ELECTRICAL, TEMPERATURE, HUMIDITY)
     * @return Readings in a JSON Object with multiple key-value pairs
     */
    private JSONObject retrieveReadings(String readingType) throws IOException, JSONException {

        
    	String basicReadingPath = String.join("/",path_url, "api/plugins/telemetry/DEVICE", device_id, "values/timeseries?keys");
        String latestReadingPath = String.join("=",basicReadingPath,  readingType  );
        
        startTs = 1;
        endTs = System.currentTimeMillis();        
        LOGGER.info("The latest 600 readings are retrieved from an endTs=" + endTs);
        String historicalReadingPath = String.join("&", latestReadingPath, "startTs="+startTs, "endTs="+endTs, "limit=600", "agg=NONE");
        
        
        try (CloseableHttpClient httpclient = HttpClients.createDefault()) {
            HttpGet readingRequest = new HttpGet(historicalReadingPath);
            setTokenAuthorization(readingRequest);

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
            if (prop.containsKey("thingsboard.username")) {
                this.username = prop.getProperty("thingsboard.username");
            } else {
                throw new IOException("Properties file is missing \"thingsboard.username=<thingsboard_username>\"");
            }
            if (prop.containsKey("thingsboard.password")) {
                this.password = prop.getProperty("thingsboard.password");
            } else {
                throw new IOException("Properties file is missing \"thingsboard.password=<thingsboard_password>\"");
            }
            if (prop.containsKey("path.url")) {
                this.path_url = prop.getProperty("path.url");
            } else {
                throw new IOException("Properties file is missing \"path.url=<path_url>\"");
            }
            if (prop.containsKey("device.id")) {
                this.device_id = prop.getProperty("device.id");
            } else {
                throw new IOException("Properties file is missing \"device.id=<device_id>\"");
            }
            if (prop.containsKey("keys")) {
                this.keys = prop.getProperty("keys");
            } else {
                throw new IOException("Properties file is missing \"keys=<keys>\"");
            }
        }
}
}
