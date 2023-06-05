package uk.ac.cam.cares.jps.agent.Carpark;
import org.json.JSONObject;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;
import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import java.util.*;
import java.io.IOException;
import java.text.SimpleDateFormat;
import java.time.OffsetDateTime;
import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.InputStream;

@WebServlet(urlPatterns = {"/retrieve", "/status"})

public class CarparkAgent extends JPSAgent
{
    public static final String Key_AgentProp = "agentProperties";
    public static final String Key_APIProp = "apiProperties";
    public static final String Key_ClientProp = "clientProperties";
    
    private static final Logger LOGGER = LogManager.getLogger(CarparkAgent.class);


    private static final String ARGUMENT_MISMATCH_MSG = "Need three properties files in the following order: 1) input agent 2) time series client 3) API connector.";
    private static final String AGENT_ERROR_MSG = "The timeseries handler could not be constructed!";
    private static final String TSCLIENT_ERROR_MSG = "Could not construct the time series client needed by the timeseries handler!";
    private static final String INITIALIZE_ERROR_MSG = "Could not initialize time series.";
    private static final String CONNECTOR_ERROR_MSG = "Could not construct the carpark API connector needed to interact with the API!";
    private static final String GET_READINGS_ERROR_MSG = "Some readings could not be retrieved.";

    String dbUrl;
    String dbUsername;
    String dbPassword;
    String sparqlQueryEndpoint;
    String sparqlUpdateEndpoint;
    String sparqlUsername;
    String sparqlPassword;

    /**
     * Servlet init.
     *
     * @throws ServletException
     */
    @Override
    public void init() throws ServletException {
        super.init();
        LOGGER.debug("This is a debug message.");
        LOGGER.info("This is an info message.");
        LOGGER.warn("This is a warn message.");
        LOGGER.error("This is an error message.");
        LOGGER.fatal("This is a fatal message.");
    }

    /**
     * Handle request and route to different functions based on the path.
     * @param requestParams Parameters sent with HTTP request
     * @param request HTTPServletRequest instance
     * @return result of the request
     */
    @Override
    public JSONObject processRequestParameters(JSONObject requestParams, HttpServletRequest request) {
        SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss.SSSS");
        String datetime = dateFormat.format(new Date());
        LOGGER.info("Request received at: {" + datetime + "}");

        JSONObject msg = new JSONObject();

        String url = request.getRequestURI();

        if (url.contains("status")) {
            msg = getStatus();
        }

        if (url.contains("retrieve")) {
            String agentProperties = System.getenv("Carpark_AGENTPROPERTIES");
            String clientProperties = System.getenv("Carpark_CLIENTPROPERTIES");
            String apiProperties = System.getenv("Carpark_APIPROPERTIES");
            
            String[] args = new String []{agentProperties,clientProperties,apiProperties};
            msg = initializeAgent(args);
        }

        return msg;
        
    }

    /**
     * Handle GET /status route and return the status of the agent.
     * @return Status of the agent
     */
    private JSONObject getStatus() {
        LOGGER.info("Detected request to get agent status...");
        JSONObject result = new JSONObject();
        result.put("description", "CarparkAgent is ready.");
        return result;
    }

    public JSONObject initializeAgent(String []args)
    {
        if(args.length!=3)
        {
            LOGGER.error(ARGUMENT_MISMATCH_MSG);
            throw new JPSRuntimeException(ARGUMENT_MISMATCH_MSG);
        }

        LOGGER.debug("Launcher called with the following files: " + String.join(" ",args));

        TimeSeriesHandler tsHandler;
        try
        {
            tsHandler = new TimeSeriesHandler(args[0]);

        }
        catch(IOException e)
        {
            LOGGER.error(AGENT_ERROR_MSG,e);
            throw new JPSRuntimeException(AGENT_ERROR_MSG,e);
        }

        LOGGER.info("Input Agent object initialized");
        JSONObject jsonMessage = new JSONObject();
        jsonMessage.accumulate("Result","Input Agent Object Initialized");

        TimeSeriesClient<OffsetDateTime> tsclient;
        try
        {
            loadTSClientConfigs(args[1]);
            RemoteStoreClient kbClient = new RemoteStoreClient();
            kbClient.setQueryEndpoint(sparqlQueryEndpoint);
            kbClient.setUpdateEndpoint(sparqlUpdateEndpoint);
            LOGGER.info("The username is " + sparqlUsername);
            LOGGER.info("The password is " + sparqlPassword);
            kbClient.setUser(sparqlUsername);
            kbClient.setPassword(sparqlPassword);
            //tsclient = new TimeSeriesClient<>(OffsetDateTime.class, args[1]);
            tsclient = new TimeSeriesClient<>(kbClient, OffsetDateTime.class, dbUrl, dbUsername, dbPassword);
            tsHandler.setTsClient(tsclient);
        }
        catch(Exception e)
        {
            LOGGER.error(TSCLIENT_ERROR_MSG,e);
            throw new JPSRuntimeException(TSCLIENT_ERROR_MSG, e); 
        }

        LOGGER.info("Time Series object initialized");
        jsonMessage.accumulate("Result","Time Series Client Object Initialized");

        try
        {
            tsHandler.initializeTimeSeriesIfNotExist();
        }
        catch(JPSRuntimeException e)
        {
            LOGGER.error(INITIALIZE_ERROR_MSG);
            throw new JPSRuntimeException(INITIALIZE_ERROR_MSG,e);
        }

        APIConnector connector;
        try
        {
            connector = new APIConnector(args[2]);
        }
        catch(IOException e)
        {
            LOGGER.error(CONNECTOR_ERROR_MSG,e);
            throw new JPSRuntimeException(CONNECTOR_ERROR_MSG,e);
        }

        LOGGER.info("API Connector Object Initialized");
        jsonMessage.accumulate("Result","API Connector object Initialized");

        JSONObject carparkReadings;

        try
        {
            carparkReadings = connector.getReadings();
        }
        catch(Exception e)
        {
            LOGGER.error(GET_READINGS_ERROR_MSG,e);
            throw new JPSRuntimeException(GET_READINGS_ERROR_MSG,e);
        }

        LOGGER.info(String.format("Retrieved %d carpark readings", carparkReadings.length()));
        jsonMessage.accumulate("Result","Retrieved"+carparkReadings.getJSONArray("value").length()+" carpark readings");

        if(!carparkReadings.isEmpty())
        {
            tsHandler.updateData(carparkReadings);
            LOGGER.info("Data updated with new API Readings");
            jsonMessage.accumulate("Result","Data updated with new API Readings");

        }
        else if(carparkReadings.isEmpty())
        {
            LOGGER.info("No new carpark data recorded");
            jsonMessage.accumulate("Result","No new carpark data recorded");
        }

        JSONObject pricingReadings;

        try
        {
            pricingReadings = connector.getPrices();
        }
        catch(Exception e)
        {
            LOGGER.error(GET_READINGS_ERROR_MSG);
            throw new JPSRuntimeException(GET_READINGS_ERROR_MSG);
        }
        
        LOGGER.info(String.format("Retrieved pricing readings for %d carparks", pricingReadings.length()));
        jsonMessage.accumulate("Result","Retrieved"+pricingReadings.getJSONObject("result").getJSONArray("records").length()+"carpark price readings");


        //To call APIQueryBuilder
       
        SparqlHandler sparqlHandler;

        try
        {
            sparqlHandler = new SparqlHandler(args[0],args[1]);
            LOGGER.info("QueryBuilder constructed");
          
        }
        catch(Exception e)
        {
            LOGGER.error("Could not build the QueryBuilder");
            throw new JPSRuntimeException("Could not successfully initialise the QueryBuilder Object");
        }

        {
            sparqlHandler.instantiateIfNotInstantiated(carparkReadings,pricingReadings);
            LOGGER.info("All Data IRIs within Carpark Readings successfully instantiated");
            jsonMessage.accumulate("Result","All Data IRIs successfully instantiated");

        }

        
       return jsonMessage;
    }

      /**
     * Reads the parameters needed for the timeseries client
     * @param filepath Path to the properties file from which to read the parameters
     */
    private void loadTSClientConfigs(String filepath) throws IOException {
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

            // Get timeseries client parameters from properties file
            if (prop.containsKey("db.url")) {
                this.dbUrl= prop.getProperty("db.url");
            } else {
                throw new IOException("Properties file is missing \"db.url=<db_url>\"");
            }
            if (prop.containsKey("db.user")) {
                this.dbUsername = prop.getProperty("db.user");
            } else {
                throw new IOException("Properties file is missing \"db.user=<db_user>\"");
            }
            if (prop.containsKey("db.password")) {
                this.dbPassword = prop.getProperty("db.password");
            } else {
                throw new IOException("Properties file is missing \"db.password=<db_password>\"");
            }
            if (prop.containsKey("sparql.query.endpoint")) {
                this.sparqlQueryEndpoint = prop.getProperty("sparql.query.endpoint");
            } else {
                throw new IOException("Properties file is missing \"sparql.query.endpoint=<sparql_query_endpoint>\"");
            }
            if (prop.containsKey("sparql.update.endpoint")) {
                this.sparqlUpdateEndpoint = prop.getProperty("sparql.update.endpoint");
            } else {
                throw new IOException("Properties file is missing \"sparql.update.endpoint=<sparql_update_endpoint>\"");
            }
            if (prop.containsKey("sparql.username")) {
                this.sparqlUsername = prop.getProperty("sparql.username");
            }
            if (prop.containsKey("sparql.password")) {
                this.sparqlPassword = prop.getProperty("sparql.password");
            }
        }
    }

}
