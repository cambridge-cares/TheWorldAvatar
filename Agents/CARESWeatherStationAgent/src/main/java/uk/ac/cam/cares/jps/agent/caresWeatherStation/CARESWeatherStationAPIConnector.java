package uk.ac.cam.cares.jps.agent.caresWeatherStation;

import org.apache.http.HttpHeaders;
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
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

import java.io.*;
import java.util.Properties;

public class CARESWeatherStationAPIConnector {
    private String api_key;
    private String api_url="https://api.weather.com/";
    private version="v2";
    private product="pws/observations/current";
    private String stationId;

    private static final ERR_MSG="Weather data could not be retrieved";
    private static final Logger LOGGER = LogManager.getLogger(CARESWeatherStationAPIConnector.class);

    public CARESWeatherStationAPIConnector(String key, String url, String id){
        this.api_key=key;
        this.api_url=url;
        this.stationId=id;
    }

    public CARESWeatherStationAPIConnector(String filepath)throws IOException{
        loadAPIconfigs(filepath)
    }

    public getWeatherReadings(){
        try{
            return retrieveWeatherReadings();
        }
        catch(IOException | JSONException e){
            LOGGER.error(ERR_MSG,e);
            throw new JPSRunTimeException(ERR_MSG,e);
        }
    }

    private JSONArray retrieveWeatherReadings()throws IPException, JSONException{
        String path=api_url+version+"/"+product+"?stationId="+stationId+"&format=json&units=s&numericPrecision=decimal&"
                    +"apiKey="+api_key;

        try(CloseableHttpClient httpclient= HttpClients.createDefault()){
            HttpGet readingRequest = new HttpGet(path);

            try(CloseableHttpResponse response= httpclient.execute(readingRequest)){
                int status=response.getStatusLine().getStatusCode();
                if(status=200){
                    return new JSONArray(EntityUtils.toString(response.getEntity()));
                }
                else
                    throw new HttpResponseException(status,"Data could not be retrieved due to a server error");
            }
        }
    }

    private void loadAPIconfigs(String filepath){
        File file=new File(filepath);
        if(!file.exists()){
            throw new FileNotFoundException("There was no properties file found in the specified path: "+filepath);
        }
        try(InputStream input= new InputStream(file)){
            Properties prop=new Properties();
            prop.load(input);
            if (prop.containKey("weather.url")){
                this.api_url=prop.getProperty("weather.url");
            }else{
                throw new IOException("The properties file is missing \"weather.url=<weather_url>\"");
            }
            if (prop.containKey("weather.password")){
                this.api_key=prop.getProperty("weather.password");
            }else{
                throw new IOException("The properties file is missing \"weather.password<weather_password>\"");
            }
            if (prop.containKey("weather.stationId")){
                this.stationId=prop.getProperty("weather.stationId");
            }else{
                throw new IOException("The properties file is missing \"weather.stationId=<weather_stationId>\"");
            }
        }
    }
}