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
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClients;
import org.apache.http.util.EntityUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;


public class ESPHomeAPI{


	/**
     * Logger for reporting info/errors.
     */
    private static final Logger LOGGER = LogManager.getLogger(ESPHomeAPI.class);
    
    //POST request reading path
    private String esphomePath;
    
    //esphome parameters
    private String esphomeUrl;
    private double esphomeThreshold;
    //JSONObjects
    JSONObject message;
    
    public ESPHomeAPI(String propertiesFile) throws IOException {
    	loadEspHomeProperties(propertiesFile);	
    }
    
    public JSONObject esphomeSwitchControl(double timeSeriesValue, boolean status) {
    	JSONObject message = new JSONObject();
    	if (timeSeriesValue > esphomeThreshold) {
    		if (status == true) {
    			message.put("message", "The switch is already in the ON state.");
    		}
    		else {
    			try {
    				message = turnOnSwitch();
    			} catch (JSONException | IOException e) {
    				throw new JPSRuntimeException("Could not establish connection with ESPHome web server to turn on switch.", e);
    			}
    		}
    	}
    	else {
    		message = new JSONObject();
    		if (status == true) {
    		try {
				message = turnOffSwitch();
			} catch (JSONException | IOException e) {
				throw new JPSRuntimeException("Could not establish connection with ESPHome web server to turn off switch.", e);
			}
    			
    		}
    		else {
    			message.put("message", "The switch is already in the OFF state.");
    		}
    	}
    	return message;
    }
    
    /**
	 * Controls GPIO switch by sending a POST request to the ESPHome web server
     * @return 
     * @return 
	 * @return result indicating that a POST request has been sent to turn on a switch in the form of a JSONObject
     * 
	 */
    private JSONObject turnOnSwitch() throws IOException, JSONException {
    	JSONObject message = new JSONObject();
    		String turnOnReadingPath = String.join("/",esphomeUrl, "switch", "generic_output", "turn_on");	
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
	 * Controls GPIO switch by sending a POST request to the ESPHome web server
     * @return 
	 * @return result indicating that a POST request has been sent to turn off a switch in the form of a JSONObject
     * 
	 */
    private JSONObject turnOffSwitch() throws IOException, JSONException {
    	JSONObject message = new JSONObject();
    			String turnOffReadingPath = String.join("/",esphomeUrl, "switch", "generic_output", "turn_off");	
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
	 * Checks status of the GPIO switch
	 * @return true if the switch is in the on state and false if the switch is in the off state
	 * @throws IOException
	 * @throws JSONException
	 */
	public boolean checkStatus() throws IOException, JSONException {
		boolean state;
		esphomePath = String.join("/",esphomeUrl, "switch", "generic_output");
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
	            		state = responseBody.getBoolean("value");
	            	}
	            }
	            else {
	            	LOGGER.error("Could not establish connection with ESPHome web server.");
	                throw new HttpResponseException(status, "Could not establish connection with ESPHome web server.");
	                
	            }
		return state;
		}
		}
	}
	
	/**
     * Reads the username, password, sparql endpoint, esphome url and switch control threshold value from a properties file and saves it in fields.
     * @param filepath Path to the properties file from which to read the username, password, sparql endpoints, esphome url and switch control threshold value
     */
    private void loadEspHomeProperties(String filepath) throws IOException {
        // Check whether properties file exists at specified location
        File file = new File(filepath);
        if (!file.exists()) {
            throw new FileNotFoundException("No properties file found at specified filepath: " + filepath);
        }
        try (InputStream input = new FileInputStream(file)) {

            // Load properties file from specified path
            Properties prop = new Properties();
            prop.load(input);
            
	        if (prop.containsKey("esphome.url")) {
	        	this.esphomeUrl = prop.getProperty("esphome.url");
	        } else {
	        	throw new IOException("Properties file is missing \"esphome.url=<esphome_url>\" ");
	        	
	        }
	        if (prop.containsKey("esphome.threshold")) {
	        	this.esphomeThreshold = Double.valueOf(prop.getProperty("esphome.threshold"));
	        } else {
	        	throw new IOException("Properties file is missing \"esphome.threshold=<esphome_threshold>\" ");
	        }
        }
        }
    }

