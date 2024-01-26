package uk.ac.cam.cares.jps.agent.historicalntuenergy;

import org.json.JSONArray;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;

import java.io.*;
import java.time.OffsetDateTime;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.util.*;

import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.BadRequestException;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;


@WebServlet(urlPatterns = {"/retrieve"})
public class HistoricalNTUEnergyAgentLauncher extends JPSAgent {

    public static final String KEY_AGENTPROPERTIES = "agentProperties";
    public static final String KEY_XLSXCONNECTORPROPERTIES = "xlsxConnectorProperties";
    public static final String KEY_CLIENTPROPERTIES = "clientProperties";
    public static String queryEndpoint;
    public static String updateEndpoint;

    /**
     * Logger for reporting info/errors.
     */
    private static final Logger LOGGER = LogManager.getLogger(HistoricalNTUEnergyAgentLauncher.class);
    /**
     * Logging / error messages
     */
    private static final String ARGUMENT_MISMATCH_MSG = "Need two properties files in the following order: 1) input agent 2) xlsx connector.";
    private static final String NAMESPACE_NOTFOUND_MSG = "Could not find a knowledge base endpoint for ntuenergy, please create a namespace ntuenergy";
    private static final String AGENT_ERROR_MSG = "The Historical NTUEnergy agent could not be constructed!";
    private static final String TSCLIENT_ERROR_MSG = "Could not construct the time series client needed by the input agent!";
    private static final String INITIALIZE_ERROR_MSG = "Could not initialize time series.";
    private static final String CONNECTOR_ERROR_MSG = "Could not construct the NTUEnergy XLSX connector needed to interact with the Excel file!";
    private static final String GET_READINGS_ERROR_MSG = "The energy reading could not be retrieved, this might have created a mismatch" +
            " in the pointers if one readings was successful and needs to be fixed!";
    private static final String GET_SPECS_ERROR_MSG = "One or multiple specs could not be retrieved. ";
    private static final String ONE_READING_EMPTY_ERROR_MSG = "Readings are empty, that means there is " +
            "a mismatch in the pointer for the readings. This should be fixed (and might require a clean up of the database)!";

    @Override
    public JSONObject processRequestParameters(JSONObject requestParams, HttpServletRequest request) {
        return processRequestParameters(requestParams);
    }

    @Override
    public JSONObject processRequestParameters(JSONObject requestParams) {
        JSONObject jsonMessage = new JSONObject();
        if(requestParams.length() == 7 && validateInput(requestParams)){
            String agentProperties = System.getenv(requestParams.getString(KEY_AGENTPROPERTIES));
            String xlsxConnectorProperties = System.getenv(requestParams.getString(KEY_XLSXCONNECTORPROPERTIES));
            String[] properties = new String[]{agentProperties, xlsxConnectorProperties};
            try {
                jsonMessage = initializeAgent(properties);
            } catch (IOException e) {
                throw new RuntimeException(e);
            }
            jsonMessage.accumulate("Result", "Timeseries Data has been updated.");
            requestParams = jsonMessage;
        }
        else if (requestParams.length() == 8 && validateInput(requestParams)) {
            String agentProperties = System.getenv(requestParams.getString(KEY_AGENTPROPERTIES));
            String clientProperties = System.getenv(requestParams.getString(KEY_CLIENTPROPERTIES));
            String xlsxConnectorProperties = System.getenv(requestParams.getString(KEY_XLSXCONNECTORPROPERTIES));
            String[] properties = new String[]{agentProperties, clientProperties, xlsxConnectorProperties};
            try {
                jsonMessage = initializeAgent(properties);
            } catch (IOException e) {
                throw new RuntimeException(e);
            }
            jsonMessage.accumulate("Result", "Timeseries Data has been updated.");
            requestParams = jsonMessage;
        } else {
            jsonMessage.put("Result", "Request parameters are not defined correctly.");
            requestParams = jsonMessage;
        }
        return requestParams;
    }

    @Override
    public boolean validateInput(JSONObject requestParams) throws BadRequestException {
        if (requestParams == null || requestParams.isEmpty()) {
            return false;
        }
        String agentProperties = requestParams.optString(KEY_AGENTPROPERTIES);
        String xlsxConnectorProperties = requestParams.optString(KEY_XLSXCONNECTORPROPERTIES);

        return validateProperty(agentProperties) && validateProperty(xlsxConnectorProperties);
    }

    private boolean validateProperty(String property) {
        if (property == null || property.isEmpty()) {
            return false;
        }
        String value = System.getenv(property);
        return (value != null && !value.isEmpty());
    }

    public static JSONObject initializeAgent(String[] properties) throws IOException {
        JSONObject jsonMessage = new JSONObject();
        if (properties.length != 2 && properties.length != 3) {
            throw new JPSRuntimeException(ARGUMENT_MISMATCH_MSG);
        }
        HistoricalNTUEnergyAgent agent;
        try {
            agent = new HistoricalNTUEnergyAgent(properties[0]);
        } catch (IOException e) {
            LOGGER.error(AGENT_ERROR_MSG, e);
            throw new JPSRuntimeException(AGENT_ERROR_MSG, e);
        }
        RemoteStoreClient kbClient = null;
        TimeSeriesClient timeSeriesClient;
        if (properties.length == 2) {
            EndpointConfig config = new EndpointConfig();
            for (String endpoint : config.getKgurls()) {
                if (endpoint.contains("ntuenergy")) {
                    kbClient = new RemoteStoreClient(endpoint, endpoint, config.getKguser(), config.getKgpassword());
                }
            }
            try {
                timeSeriesClient = new TimeSeriesClient<>(kbClient, OffsetDateTime.class, config.getDburl(), config.getDbuser(), config.getDbpassword());
                agent.setTsClient(timeSeriesClient);
            } catch (JPSRuntimeException e) {
                LOGGER.error(TSCLIENT_ERROR_MSG, e);
                throw new JPSRuntimeException(TSCLIENT_ERROR_MSG, e);
            }
        } else if (properties.length == 3) {
            kbClient = new RemoteStoreClient();
            loadconfigs(properties[1]);
            kbClient.setQueryEndpoint(queryEndpoint);
            kbClient.setUpdateEndpoint(updateEndpoint);
            try {
                timeSeriesClient = new TimeSeriesClient<>(OffsetDateTime.class, properties[1]);
                agent.setTsClient(timeSeriesClient);
            } catch (IOException | JPSRuntimeException e) {
            }
        }
        if (kbClient == null) {
            throw new JPSRuntimeException(NAMESPACE_NOTFOUND_MSG);
        }

        LOGGER.info("Time series client object initialized.");
        // Initialize time series'
        try {
            agent.initializeTimeSeriesIfNotExist();
        } catch (JPSRuntimeException e) {
            LOGGER.error(INITIALIZE_ERROR_MSG, e);
            throw new JPSRuntimeException(INITIALIZE_ERROR_MSG, e);
        }

        // Create the connector to interact with the Energy XLSX
        HistoricalNTUEnergyAgentXLSXConnector connector;
        try {
            if (properties.length == 2) {
                connector = new HistoricalNTUEnergyAgentXLSXConnector(System.getenv("ENERGY_READINGS"), properties[1],
                        System.getenv("GENERATOR_SPECS"), System.getenv("BUSNODE_SPECS"), System.getenv("BRANCH_SPECS"), System.getenv("PV_SPECS"), System.getenv("VENUE_INFO"), System.getenv("CLASS_SCHEDULE"));
            } else {
                connector = new HistoricalNTUEnergyAgentXLSXConnector(System.getenv("ENERGY_READINGS"), properties[2],
                        System.getenv("GENERATOR_SPECS"), System.getenv("BUSNODE_SPECS"), System.getenv("BRANCH_SPECS"),  System.getenv("PV_SPECS"), System.getenv("VENUE_INFO"), System.getenv("CLASS_SCHEDULE"));
            }
        } catch (IOException e) {
            LOGGER.error(CONNECTOR_ERROR_MSG, e);
            throw new JPSRuntimeException(CONNECTOR_ERROR_MSG, e);
        }
        LOGGER.info("xlsx connector object initialized.");
        /*
            Retrieve energy readings from the XLSX file
         */
        JSONArray energyReadings;
        try {
            energyReadings = connector.getEnergyReadings();
        } catch (Exception e) {
            LOGGER.error(GET_READINGS_ERROR_MSG, e);
            throw new JPSRuntimeException(GET_READINGS_ERROR_MSG, e);
        }

        LOGGER.info(String.format("Retrieved %d energy readings.",
                energyReadings.length()));

        // If the file readings are not empty there is new data
        if (!energyReadings.isEmpty()) {
            // Update the data
            agent.updateData(energyReadings);
            LOGGER.info("Data updated with new readings from API.");
            jsonMessage.put("Result", "Data updated with new readings from API.");
        }
        // If the file readings are empty, then no new readings are available
        else if (energyReadings.isEmpty()) {
            LOGGER.info("No new readings are available.");
            jsonMessage.put("Result", "No new readings  are available.");
        } else {
            LOGGER.error(ONE_READING_EMPTY_ERROR_MSG);
            throw new JPSRuntimeException(ONE_READING_EMPTY_ERROR_MSG);
        }

        /*
            Retrieve energy readings from the CSV file
         */
        JSONArray waterReadings;
        try {
            waterReadings = connector.getWaterReadings();
        } catch (Exception e) {
            LOGGER.error(GET_READINGS_ERROR_MSG, e);
            throw new JPSRuntimeException(GET_READINGS_ERROR_MSG, e);
        }
        if (!waterReadings.isEmpty()) {
            // Update the data
            agent.updateData(waterReadings);
            LOGGER.info("Data updated with new readings from API.");
            jsonMessage.put("Result", "Data updated with new readings from API.");
        }
        else if(waterReadings.isEmpty()) {
            LOGGER.info("No new readings are available.");
            jsonMessage.put("Result", "No new readings are available.");
        } else {
            LOGGER.error(ONE_READING_EMPTY_ERROR_MSG);
            throw new JPSRuntimeException(ONE_READING_EMPTY_ERROR_MSG);
        }


        //Retrieve specs
        JSONArray busNodeSpecs;
        JSONArray branchSpecs;
        JSONArray generatorSpecs;
        JSONArray pvSpecs;
        JSONArray venueInfo;
        JSONArray classSchedule;

        try {
            busNodeSpecs = connector.getBusNodeSpecs();
            branchSpecs = connector.getBranchSpecs();
            generatorSpecs = connector.getGeneratorSpecs();
            pvSpecs = connector.getPVSpecs();
            venueInfo = connector.getVenueInfo();
            classSchedule = connector.getClassSchedule();
        } catch (Exception e) {
            LOGGER.error(GET_SPECS_ERROR_MSG, e);
            throw new JPSRuntimeException(GET_SPECS_ERROR_MSG, e);
        }

        HistoricalQueryBuilder queryBuilder;
        try {
            queryBuilder = new HistoricalQueryBuilder(properties[0], kbClient, busNodeSpecs, branchSpecs, generatorSpecs, pvSpecs, venueInfo);
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
        queryBuilder.instantiateTriples();

        CourseInstantiator courseInstantiator;
        courseInstantiator = new CourseInstantiator(kbClient, classSchedule);
        courseInstantiator.instantiateTriples();

        return jsonMessage;
    }
    public static void loadconfigs(String filepath) throws IOException {
        File file = new File(filepath);
        if (!file.exists()) {
            throw new FileNotFoundException("There was no file found in the path");
        }

        try (InputStream input = new FileInputStream(file)) {
            Properties prop = new Properties();
            prop.load(input);

            if (prop.containsKey("sparql.query.endpoint")) {
                queryEndpoint = prop.getProperty("sparql.query.endpoint");
            } else {
                throw new IOException("The file is missing: \"sparql.query.endpoint=<queryEndpoint>\"");
            }

            if (prop.containsKey("sparql.update.endpoint")) {
                updateEndpoint = prop.getProperty("sparql.update.endpoint");
            } else {
                throw new IOException("The file is missing: \"sparql.update.endpoint=<updateEndpoint>\"");
            }
        }
    }

}
