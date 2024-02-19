package uk.ac.cam.cares.jps.agent.HawkerCentre;


import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import java.io.IOException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.BadRequestException;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

@WebServlet(urlPatterns = {"/retrieve"})

public class APIAgentLauncher extends JPSAgent
{
    public static final String Key_APIProp = "apiProperties";
    public static final String Key_ClientProp = "clientProperties";
    
    private static final Logger Log = LogManager.getLogger(APIAgentLauncher.class);


    private static final String ARGUMENT_MISMATCH_MSG = "Need three properties files in the following order: 1) input agent 2) time series client 3) API connector.";
    private static final String CONNECTOR_ERROR_MSG = "Could not construct the carpark API connector needed to interact with the API!";
    private static final String GET_READINGS_ERROR_MSG = "Some readings could not be retrieved.";

    public JSONObject processRequestParameters(JSONObject requestparams, HttpServletRequest request)
    {
        return processRequestParameters(requestparams);
    }
    
    public JSONObject processRequestParameters(JSONObject requestparams)
    {
        JSONObject jsonMessage = new JSONObject();

        if(validateInput(requestparams))
        {   
            Log.info("Passing Request to API Input Agent");
            String clientProperties = System.getenv(requestparams.getString(Key_ClientProp));
            String apiProperties = System.getenv(requestparams.getString(Key_APIProp));
            
            String[] args = new String []{clientProperties,apiProperties};
            jsonMessage = initializeAgent(args);
            jsonMessage.accumulate("Result","TimeSeries has been updated");

            requestparams = jsonMessage;

        }
        else
        {
            jsonMessage.put("Result","Request Parameters not defined correctly");
            requestparams=jsonMessage;
        }
        return requestparams;
    }
    

    public boolean validateInput(JSONObject requestparams) throws BadRequestException
    {
        boolean validate = true;
        String clientProperties;
        String apiproperties;
        if(requestparams.isEmpty())
         validate = false;
        else
        {
             validate = requestparams.has(Key_ClientProp);
            if(validate)
             validate = requestparams.has(Key_APIProp);
            if(validate)
            {
                clientProperties = (requestparams.getString(Key_ClientProp));
                apiproperties = (requestparams.getString(Key_APIProp));

                if((System.getenv(clientProperties)==null) || (System.getenv(apiproperties)==null))
                 validate=false;

            }
        }

        return validate;
    }

    public static JSONObject initializeAgent(String []args)
    {
        if(args.length!=2)
        {
            Log.error(ARGUMENT_MISMATCH_MSG);
            throw new JPSRuntimeException(ARGUMENT_MISMATCH_MSG);
        }

        Log.debug("Launcher called with the following files: " + String.join(" ",args));

       
        JSONObject jsonMessage = new JSONObject();
        APIConnector connector;
        try
        {
            connector = new APIConnector(args[1]);
        }
        catch(IOException e)
        {
            Log.error(CONNECTOR_ERROR_MSG,e);
            throw new JPSRuntimeException(CONNECTOR_ERROR_MSG,e);
        }

        Log.info("API Connector Object Initialized");
        jsonMessage.accumulate("Result","API Connector object Initialized");

        JSONObject hawkerReadings,stallsReadings;


        try
        {
            hawkerReadings = connector.getHawkerReadings();
        }
        catch(Exception e)
        {
            Log.error(GET_READINGS_ERROR_MSG,e);
            throw new JPSRuntimeException(GET_READINGS_ERROR_MSG,e);
        }

        Log.info(String.format("Retrieved %d hawker readings", hawkerReadings.getJSONObject("result").getJSONArray("records").length()));
        jsonMessage.accumulate("Result","Retrieved"+hawkerReadings.getJSONObject("result").getJSONArray("records").length()+" hawker centre readings");

        

        try
        {
            stallsReadings = connector.getStallsReadings();
        }
        catch(Exception e)
        {
            Log.error(GET_READINGS_ERROR_MSG);
            throw new JPSRuntimeException(GET_READINGS_ERROR_MSG);
        }
        
        Log.info(String.format("Retrieved stalls readings for %d stalls", stallsReadings.getJSONObject("result").getJSONArray("records").length()));
        jsonMessage.accumulate("Result","Retrieved"+stallsReadings.getJSONObject("result").getJSONArray("records").length()+"stalls readings");


        //To call APIQueryBuilder
       
        APIQueryBuilder queryBuilder;

        try
        {
            queryBuilder = new APIQueryBuilder(args[0]);
            Log.info("QueryBuilder constructed");
          
        }
        catch(Exception e)
        {
            Log.error("Could not build the QueryBuilder");
            throw new JPSRuntimeException("Could not successfully initialise the QueryBuilder Object");
        }

        try
        {
            queryBuilder.instantiateIfNotInstantiated(hawkerReadings,stallsReadings);
            Log.info("All Data IRIs within Hawker Readings successfully instantiated");
            jsonMessage.accumulate("Result","All Data IRIs successfully instantiated");

        }
        catch(Exception e)
        {
            Log.error("Could not Instantiate the IRIs successfully");
            jsonMessage.accumulate("Result","IRIs not instantiated properly");
            throw new JPSRuntimeException("Unable to instantiate the IRIs successfully", e);
        }

        
       return jsonMessage;
    }
}