package uk.ac.cam.cares.jps.agent.caresWeatherStation;


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
import java.util.Properties;

/**
 * Class that connects to the weather station API
 * @author  GMMajal*/
public class CARESWeatherStationAPIConnector {
    private String api_key;
    private String api_url="https://api.weather.com/";
    private String stationId;

    private static final String ERR_MSG="Weather data could not be retrieved";
    private static final Logger LOGGER = LogManager.getLogger(CARESWeatherStationInputAgentLauncher.class);

    /**
     * Standard constructor
     * @param api_key the <apikey> to access the weather station API
     * @param stationId the id of the weather station device.
     * @param api_url the url that is hosting the weather station server
     */
    public CARESWeatherStationAPIConnector(String key, String id, String url){
        this.api_key=key;
        this.stationId=id;
        this.api_url=url;
    }

    /**
     * Constructor using a properties file
     * @param filepath Path to the properties file from which to read the api_key, stationId and api_url
     **/
    public CARESWeatherStationAPIConnector(String filepath)throws IOException{
        loadAPIconfigs(filepath);
    }

    /**
     * Retrieves the latest weather readings from the weather station API
     * @return a JSON Object containing key-value pairs
     */

    public JSONObject getWeatherReadings(){
        try{
            return retrieveWeatherReadings();
        }
        catch(IOException | JSONException e){
            LOGGER.error(ERR_MSG,e);
            throw new JPSRuntimeException(ERR_MSG,e);
        }
    }

    /**
     * Retrieves the latest readings from the weather station API
     * @return Readings in a JSON Object with multiple key-value pairs
     */
    private JSONObject retrieveWeatherReadings() throws IOException, JSONException{
        String path=api_url+"v2/pws/observations/all/1day?stationId="+stationId+"&format=json&units=s&numericPrecision=decimal&"
                +"apiKey="+api_key;

        try(CloseableHttpClient httpclient= HttpClients.createDefault()){
            HttpGet readingRequest = new HttpGet(path);

            try(CloseableHttpResponse response= httpclient.execute(readingRequest)){
                int status=response.getStatusLine().getStatusCode();
                if(status==200){
                    return new JSONObject(EntityUtils.toString(response.getEntity()));
                }
                else
                    throw new HttpResponseException(status,"Data could not be retrieved due to a server error");
            }
        }
    }
    /**
     * Reads the api_key, stationId and api_url needed to connect to the API from a properties file and saves it in fields.
     * @param filepath Path to the properties file from which to read the api_key, stationId and api_url
     */
    private void loadAPIconfigs(String filepath) throws IOException{
        File file=new File(filepath);
        if(!file.exists()){
            throw new FileNotFoundException("There was no properties file found in the specified path: "+filepath);
        }
        try(InputStream input= new FileInputStream(file)){
            Properties prop=new Properties();
            prop.load(input);
            if (prop.containsKey("weather.api_key")){
                this.api_key=prop.getProperty("weather.api_key");
            }else{
                throw new IOException("The properties file is missing \"weather.api_key=<api_key>\"");
            }
            if (prop.containsKey("weather.stationId")){
                this.stationId=prop.getProperty("weather.stationId");
            }else{
                throw new IOException("The properties file is missing \"weather.stationId=<stationId>\"");
            }
            if (prop.containsKey("weather.api_url")){
                this.api_url=prop.getProperty("weather.api_url");
            }else{
                throw new IOException("The properties file is missing \"weather.api_url=<api_url>\"");
            }
        }
    }
}