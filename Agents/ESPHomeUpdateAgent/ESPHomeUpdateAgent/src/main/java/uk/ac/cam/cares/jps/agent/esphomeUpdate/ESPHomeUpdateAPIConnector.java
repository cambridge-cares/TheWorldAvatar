package uk.ac.cam.cares.jps.agent.esphomeUpdate;

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


public class ESPHomeUpdateAPIConnector {

    /**
     * Fields to access API and retrieve necessary data
     */
    private String path_url;
    private String domain; //Type of component: Sensor, Binary Sensor, Switch, Light, Fan etc
    private String ID; //ID of the component, this ID is indicated in the web_control.yaml file when setting up the ESPHome server
    
    /**
     * Logger for reporting info/errors.
     */
    private static final Logger LOGGER = LogManager.getLogger(ESPHomeUpdateAgentLauncher.class);
    /**
     * Error messages
     */
    private static final String CONNECTION_ERROR_MSG = "Unable to connect to ESPHome API!";
    private static final String READINGS_ERROR_MSG ="state of the component could not be retrieved";
    
    /**
     * Standard constructor
     * @param path_url the IP address or URL of where the ESPHome server is located
     * @param domain the type of component
     * @param ID the ID of the component
     */
    public ESPHomeUpdateAPIConnector(String path_url, String domain, String ID) {
        this.path_url = path_url;
        this.domain = domain;
        this.ID = ID;       
        
    }


    /**
     * Constructor using a properties file
     * @param filepath Path to the properties file from which to read the path_url, domain and ID
     */
    public ESPHomeUpdateAPIConnector(String filepath) throws IOException {
        loadAPIConfigs(filepath);
    }


    /**
     * Method to test whether the API is accessible after the token was set
     * @return server status
     */
    public JSONObject checkStatus() throws IOException {
    	String state;
		String esphomePath = String.join("/",path_url, domain, ID);
		try (CloseableHttpClient httpclient = HttpClients.createDefault()) {
	        HttpGet readingRequest = new HttpGet(esphomePath);
	        try (CloseableHttpResponse response = httpclient.execute(readingRequest)) {
	            int status = response.getStatusLine().getStatusCode();
	            if (status == 200) {
	            	JSONObject responseBody = new JSONObject(EntityUtils.toString(response.getEntity()));
	            
	            	if (responseBody.isEmpty()) {
                        throw new JSONException("No assets available in returned JSON Object.");
	            	}
	            	else {
	            		try {
		            		state = responseBody.getString("state");
		            		}
		            		catch (JSONException e) {
		            			throw new JPSRuntimeException("Unable to check status of the component.");
		            		}
	            	}
	            }
	            else {
	            	LOGGER.error("Could not establish connection with ESPHome web server.");
	                throw new HttpResponseException(status, "Could not establish connection with ESPHome web server.");
	                
	            }
	            JSONObject a = new JSONObject();
	        	long timestamp = System.currentTimeMillis();
	        	a.put("value",state);
	        	a.put("ts", timestamp);
	        	JSONArray b = new JSONArray();
	        	b.put(a);
	        	JSONObject statusAndTimeStamp = new JSONObject();
	        	statusAndTimeStamp.put(ID,b); //statusAndTimestamp = {"ID":[{"value":"ON","ts":"123456000"}]}
		return statusAndTimeStamp;
		}
		}
	}
    
    /**
     * Reads the path URL, domain, domain ID needed to connect to the API from a properties file and saves it in fields.
     * @param filepath Path to the properties file from which to read the path URL, domain and domain ID
     */
    private void loadAPIConfigs(String filepath) throws IOException {
        // Check whether properties file exists at specified location
        File file = new File(filepath);
        if (!file.exists()) {
            throw new FileNotFoundException("No properties file found at specified filepath: " + filepath);
        }
        // Read username and password for ESPHome API from properties file
        // Try-with-resource to ensure closure of input stream
        try (InputStream input = new FileInputStream(file)) {

            // Load properties file from specified path
            Properties prop = new Properties();
            prop.load(input);

            // Get path URL, domain and domain ID from properties file
            if (prop.containsKey("path.url")) {
                this.path_url = prop.getProperty("path.url");
            } else {
                throw new IOException("Properties file is missing \"path.url=<path_url>\"");
            }
            if (prop.containsKey("esphome.domain")) {
                this.domain = prop.getProperty("esphome.domain");
            } else {
                throw new IOException("Properties file is missing \"esphome.domain=<domain>\"");
            }
            if (prop.containsKey("domain.ID")) {
                this.ID = prop.getProperty("domain.ID");
            } else {
                throw new IOException("Properties file is missing \"domain.ID=<ID>\"");
            }
        }
}
}
