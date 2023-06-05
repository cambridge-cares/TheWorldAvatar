package uk.ac.cam.cares.jps.agent.Carpark;
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
    private String API_URL = "http://datamall2.mytransport.sg/ltaodataservice/CarParkAvailabilityv2";
    private String PRICING_API_URL = "https://data.gov.sg/api/action/datastore_search?resource_id=85207289-6ae7-4a56-9066-e6090a3684a5&limit=357";
    private String date;
    private String accountKey;

    private static final String ERRORMSG = "Carpark data could not be retrieved";
    private static final Logger LOG = LogManager.getLogger(CarparkAgent.class);
  

    //Standard Constructor to initialise the instance variables
    public APIConnector(String URL, String d, String k, String Pricing_URL)
    {
        API_URL=URL;
        PRICING_API_URL = Pricing_URL;
        date = d;
        accountKey = k;
    }
    

    //Constructor to initialise the variables according to the Properties file

    public APIConnector(String filepath) throws IOException
    {
        loadAPIConfigs(filepath);
    }      

    // Obtains Weather data in JSON format containing key:value pairs

    public JSONObject getReadings()
    {
        try{
            return retrieveData();
        }
        catch(IOException e)
        {
            LOG.error(ERRORMSG);
            throw new JPSRuntimeException(ERRORMSG,e);
        }
    }

    public JSONObject getPrices()
    {
        try
        {
            return retreivePricingData();
        }
        catch(Exception e)
        {
            LOG.error(ERRORMSG);
            throw new JPSRuntimeException(ERRORMSG,e);
        }
    }

    private JSONObject retrieveData() throws IOException, JSONException
    {  
        String path = API_URL;

        try ( CloseableHttpClient httpclient =  HttpClients.createDefault())
        {
            HttpGet readrequest = new HttpGet(path);
            readrequest.setHeader("AccountKey", accountKey); 
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

    private JSONObject retreivePricingData() throws IOException, JSONException
    {
        String path = PRICING_API_URL;

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
                    throw new HttpResponseException(status,"Pricing Data could not be retrieved due to a server");
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

            if(prop.containsKey("carpark.api_url"))
            {
                this.API_URL = prop.getProperty("carpark.api_url");
            }
            else
            {
                throw new IOException("The file is missing: \"carpark.api_url=<api_url>\"");
            }
            
            if(prop.containsKey("carpark.accountKey"))
            {
                this.accountKey = prop.getProperty("carpark.accountKey");
            }
            else
            {
                throw new IOException("The file is missing: \"carpark.accountKey=<accountKey>\"");
            }
            

        }
    }

}
