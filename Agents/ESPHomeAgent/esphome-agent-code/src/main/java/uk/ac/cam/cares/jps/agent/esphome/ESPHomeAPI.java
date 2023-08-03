package uk.ac.cam.cares.jps.agent.esphome;

import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

import org.json.JSONException;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.util.Properties;

import org.apache.http.client.HttpResponseException;
import org.apache.http.client.methods.CloseableHttpResponse;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClients;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;


public class ESPHomeAPI{


	/**
     * Logger for reporting info/errors.
     */
    private static final Logger LOGGER = LogManager.getLogger(ESPHomeAgent.class);
    
    
    //esphome parameters
    private String path_url; //IP address of the ESPHome web server
    private String domain; //Type of component: Sensor, Binary Sensor, Switch, Light, Fan etc
    private String ID; //ID of the component, this ID is indicated in the web_control.yaml file when setting up the ESPHome server
    //JSONObjects
    JSONObject message;
    
    /**
     * Standard constructor
     * @param path_url the IP address or URL of where the ESPHome server is located
     * @param domain the type of component
     * @param ID the ID of the component
     * @param esphomeThreshold the threshold value that determines whether to turn a component on or off
     */
    public ESPHomeAPI(String path_url, String domain, String ID) {
        this.path_url = path_url;
        this.domain = domain;
        this.ID = ID;
        
    }
    
    public ESPHomeAPI(String propertiesFile) throws IOException {
    	loadESPHomeConfigs(propertiesFile);	
    }
    
    public JSONObject esphomeSwitchControl(double timeSeriesValue, String status, double esphomeThreshold) {
    	JSONObject message = new JSONObject();
    	if (timeSeriesValue > esphomeThreshold) {
    		if (status.contains("ON")) {
    			LOGGER.info("The component is already in the ON state.");
    			message.put("message", "The component is already in the ON state.");
    		}
    		else {
    			try {
    				message = turnOnComponent();
    			} catch (JSONException | IOException e) {
    				throw new JPSRuntimeException("Could not establish connection with ESPHome web server to turn on the component.", e);
    			}
    		}
    	}
    	else {
    		message = new JSONObject();
    		if (status.contains("ON")) {
    		try {
				message = turnOffComponent();
			} catch (JSONException | IOException e) {
				throw new JPSRuntimeException("Could not establish connection with ESPHome web server to turn off the component.", e);
			}
    			
    		}
    		else {
    			LOGGER.info("The component is already in the OFF state.");
    			message.put("message", "The component is already in the OFF state.");
    		}
    	}
    	return message;
    }
    
    /**
	 * Controls component by sending a POST request to the ESPHome web server 
	 * @return result indicating that a POST request has been sent to turn on a component in the form of a JSONObject
	 */
    private JSONObject turnOnComponent() throws IOException, JSONException {
    	JSONObject message = new JSONObject();
    		String turnOnReadingPath = String.join("/", path_url, domain, ID, "turn_on");	
    		try (CloseableHttpClient httpclient = HttpClients.createDefault()) {
            HttpPost readingRequest = new HttpPost(turnOnReadingPath);
            try (CloseableHttpResponse response = httpclient.execute(readingRequest)) {
                int status = response.getStatusLine().getStatusCode();
                if (status == 200) {
                	LOGGER.info("A POST request has been sent to turn on the device or component.");
                	
                	message.put("message", "A POST request has been sent to turn on the device or component.");
                }	
                else {
                    throw new HttpResponseException(status, "Could not establish connection with ESPHome web server.");
                }
            }
    		}
    		return message;
    }
    
    /**
	 * Controls component by sending a POST request to the ESPHome web server 
	 * @return result indicating that a POST request has been sent to turn off a component in the form of a JSONObject
	 */
    private JSONObject turnOffComponent() throws IOException, JSONException {
    	JSONObject message = new JSONObject();
    			String turnOffReadingPath = String.join("/", path_url, domain, ID, "turn_off");	
    			try (CloseableHttpClient httpclient = HttpClients.createDefault()) {
    		        HttpPost readingRequest = new HttpPost(turnOffReadingPath);
    		        try (CloseableHttpResponse response = httpclient.execute(readingRequest)) {
    		            int status = response.getStatusLine().getStatusCode();
    		            if (status == 200) {
    		            	LOGGER.info("A POST request has been sent to turn off the device or component.");
    		            	
    		            	message.put("message", "A POST request has been sent to turn off the device or component.");
    		            }
    		            else {
    		                throw new HttpResponseException(status, "Could not establish connection with ESPHome web server.");
    		            }
            }
    		}
    			return message;
    }
	
	/**
     * Reads the path URL, domain, domain ID and Threshold from a properties file and saves it in fields.
     * @param filepath Path to the properties file from which to read the path URL, domain, domain ID and Threshold value
     */
    private void loadESPHomeConfigs(String filepath) throws IOException {
        // Check whether properties file exists at specified location
        File file = new File(filepath);
        if (!file.exists()) {
            throw new FileNotFoundException("No properties file found at specified filepath: " + filepath);
        }
        // Read ESPHome properties from properties file
        // Try-with-resource to ensure closure of input stream
        try (InputStream input = new FileInputStream(file)) {

            // Load properties file from specified path
            Properties prop = new Properties();
            prop.load(input);

            // Get path URL, domain, domain ID and Threshold from properties file
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

