package uk.ac.cam.cares.jps.agent.HawkerCentre;


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


public class APIConnector
{
    private String hawkerURL = "https://data.gov.sg/api/action/datastore_search?resource_id=8f6bba57-19fc-4f36-8dcf-c0bda382364d&limit=107";
    private String stallsURL ="https://data.gov.sg/api/action/datastore_search?resource_id=34f86c3e-a90c-4de9-a69f-afc86b7f31b6&limit=36687";
    

    private static final String ERRORMSG = "Hawker centre data could not be retrieved";
    private static final Logger LOG = LogManager.getLogger(APIAgentLauncher.class);
  

    //Standard Constructor to initialise the instance variables
    
    public APIConnector(String hawkURL, String stURL)
    {
        hawkerURL = hawkURL;
        stallsURL = stURL;
    }
    

    //Constructor to initialise the variables according to the Properties file

    public APIConnector(String filepath) throws IOException
    {
        loadAPIConfigs(filepath);
    }      

    // Obtains School data in JSON format containing key:value pairs

    public JSONObject getHawkerReadings()
    {
        try{
            return retrieveHawkerData();
        }
        catch(IOException e)
        {
            LOG.error(ERRORMSG);
            throw new JPSRuntimeException(ERRORMSG,e);
        }
    }

    public JSONObject getStallsReadings()
    {
        try
        {
            return retreiveStallsData();
        }
        catch(Exception e)
        {
            LOG.error(ERRORMSG);
            throw new JPSRuntimeException(ERRORMSG,e);
        }
    }


    private JSONObject retrieveHawkerData() throws IOException, JSONException
    {  
        String path = hawkerURL;

        try ( CloseableHttpClient httpclient =  HttpClients.createDefault())
        {
            HttpGet readrequest = new HttpGet(path);
            try ( CloseableHttpResponse response = httpclient.execute(readrequest))
            {
                int status = response.getStatusLine().getStatusCode();

                if(status==200) 
                {
                    return new JSONObject(EntityUtils.toString(response.getEntity()));

                }
                else
                {
                    throw new HttpResponseException(status,"Data could not be retrieved due to a server error");
                }

            }

        }

    }

    private JSONObject retreiveStallsData() throws IOException, JSONException
    {
        String path = stallsURL;

        try(CloseableHttpClient httpClient = HttpClients.createDefault())
        {
            HttpGet readrequest = new HttpGet(path);

            try(CloseableHttpResponse response = httpClient.execute(readrequest))
            {
                int status = response.getStatusLine().getStatusCode();

                if(status==200)
                {
                    return new JSONObject(EntityUtils.toString(response.getEntity()));
                }
                else
                {
                    throw new HttpResponseException(status,"Stalls Data could not be retrieved due to a server");
                }
            }
        }
    }


    private void loadAPIConfigs(String filepath) throws IOException
    {
        File file = new File(filepath);
        if(!file.exists())
        {
            throw new FileNotFoundException("There was no file found in the path");
        }
        
        try (InputStream input = new FileInputStream(file))
        {
            Properties prop = new Properties();
            prop.load(input);

            if(prop.containsKey("hawker.api_url"))
            {
                hawkerURL = prop.getProperty("hawker.api_url");
            }
            else
            {
                throw new IOException("The file is missing: \"hawker.api_url=<api_url>\"");
            }

            if(prop.containsKey("stalls.api_url"))
            {
                stallsURL = prop.getProperty("stalls.api_url");
            }
            else
            {
                throw new IOException("The file is missing: \"stalls.api_url=<api_url>\"");
            } 
        }
    }

}
