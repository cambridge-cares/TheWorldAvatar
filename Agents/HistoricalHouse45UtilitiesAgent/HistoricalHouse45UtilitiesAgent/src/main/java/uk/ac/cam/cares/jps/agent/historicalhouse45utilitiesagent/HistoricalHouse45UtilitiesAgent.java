package uk.ac.cam.cares.jps.agent.historicalhouse45utilitiesagent;

import org.json.JSONObject;

import uk.ac.cam.cares.jps.agent.historicalhouse45utilitiesagent.ontobim.OntoBimAdapter;
import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.time.Instant;
import java.util.List;
import java.util.Map;
import java.time.LocalDate;

import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.BadRequestException;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesSparql;

/**
 * This class acts as an agent that receives POST requests, and coordinate with the other classes
 * to instantiate the timeseries data from Excel into a SPARQL endpoint and RDB.
 *
 * @author qhouyee
 */
@WebServlet(urlPatterns = {"/retrieve"})
public class HistoricalHouse45UtilitiesAgent extends JPSAgent {
    /**
     * Logger for reporting info/errors.
     */
    private static final Logger LOGGER = LogManager.getLogger(HistoricalHouse45UtilitiesAgent.class);
    /**
     * Logging / error messages
     */
    private static final String ARGUMENT_MISMATCH_MSG = "Require 2 arguments! Please send only the file path to the Excel properties and workbook. " +
            "Do note that the properties file need not exist and would be generated on the first build process.";
    private static final String PARSER_ERROR_MSG = "Could not construct the Excel parser needed to interact with the Excel Workbook!";
    private static final String GET_READINGS_ERROR_MSG = "Readings could not be retrieved!";
    private static final String HANDLER_ERROR_MSG = "Could not construct the time series Properties handler!";
    private static final String GET_IRIMAP_ERROR_MSG = "IRI mappings could not be generated or retrieved!";
    private static final String TSCLIENT_ERROR_MSG = "Could not construct the time series client needed by the input agent!";
    private static final String INITIALIZE_ERROR_MSG = "Could not initialize time series.";
    private static final String DATA_UPDATE_ERROR_MSG = "Could not update time series!";
    // POST request keys
    private static final String KEY_CLIENTPROPERTIES = "clientProperties";
    private static final String KEY_CITYGML_BUILDING_INST = "cityGmlBuildingIri";

    // Edit these fields per your requirements
    public static final String iriPrefix = TimeSeriesSparql.TIMESERIES_NAMESPACE  + "45utility/"; // The prefix to use for generating IRI
    public static final int rowStart = 3;
    protected static Map<String, String> clientConfig;
    private static String dateKey;

    // Optional arguments
    private static int[] dateArrays;
    private static BuildingIRISingleton singleton;

    @Override
    public JSONObject processRequestParameters(JSONObject requestParams, HttpServletRequest request) {
        return processRequestParameters(requestParams);
    }

    @Override
    public JSONObject processRequestParameters(JSONObject requestParams) {
        JSONObject jsonMessage = new JSONObject();
        if (validateInput(requestParams)) {
            this.setDateKey("reading_datestamp");
            this.setDateArray(new int[]{0, 1, 2});
            singleton = BuildingIRISingleton.getInstance(); // Create new singleton
            LOGGER.info("Passing request to Historical House45 Utilities Agent..");
            // Initialise required values
            if (requestParams.has(KEY_CITYGML_BUILDING_INST)) {
                singleton.setOntoCityGmlBuildingIri(requestParams.getString(KEY_CITYGML_BUILDING_INST));
            }
            String excelProperties = FileManager.PROPERTIES;
            String excelFile;
            try {
                excelFile = FileManager.retrieveExcelPath();
            } catch (IOException e) {
                throw new JPSRuntimeException(e);
            }
            String clientProperties = System.getenv(requestParams.getString(KEY_CLIENTPROPERTIES));
            clientConfig = FileManager.retrieveClientProperties(clientProperties);
            String[] parameters = new String[]{excelProperties, excelFile};
            jsonMessage = this.initializeAgent(parameters);
            jsonMessage.accumulate("Result", "Time Series Data has been updated.");
        } else {
            jsonMessage.put("Result", "Request parameters are not defined correctly.");
        }
        requestParams = jsonMessage;
        return requestParams;
    }

    @Override
    public boolean validateInput(JSONObject requestParams) throws BadRequestException {
        boolean validate;
        String clientProperties;
        if (requestParams.isEmpty()) {
            validate = false;
        } else {
            validate = requestParams.has(KEY_CLIENTPROPERTIES);
            if (validate) {
                clientProperties = (requestParams.getString(KEY_CLIENTPROPERTIES));
                if (System.getenv(clientProperties) == null) {
                    validate = false;
                }
            }
        }
        return validate;
    }

    public void setDateKey(String dateText) {
        dateKey = dateText;
    }

    public void setDateArray(int[] dateArray) {
        dateArrays = dateArray; // Date array must contain column indices for day, month, year
    }

    public JSONObject initializeAgent(String[] args) {
        JSONObject jsonMessage = new JSONObject();
        // Ensure that there are one properties file and one Excel file
        if (args.length != 2) {
            LOGGER.error(ARGUMENT_MISMATCH_MSG);
            throw new JPSRuntimeException(ARGUMENT_MISMATCH_MSG);
        }
        LOGGER.debug("Launcher called with the following files: " + String.join(" ", args));


        // Create an Excel parser object to retrieve Excel content
        ExcelParser parser;
        try {
            parser = new ExcelParser(args[1], dateKey);
        } catch (FileNotFoundException e) {
            LOGGER.error(PARSER_ERROR_MSG + e);
            throw new JPSRuntimeException(PARSER_ERROR_MSG, e);
        }
        LOGGER.info("Excel parser initialized.");

        // Parse Excel values into a hashmap
        Map<String, List<?>> excelReadings;
        try {
            // In this workbook, readings begin from the 4th row with an index of 3
            excelReadings = parser.parseToHashMap(rowStart, dateArrays);
        } catch (Exception e) {
            LOGGER.error(GET_READINGS_ERROR_MSG, e);
            throw new JPSRuntimeException(GET_READINGS_ERROR_MSG, e);
        }
        LOGGER.info(String.format("Retrieved %d Excel readings.", excelReadings.size()));

        // Initialize a Properties handler to handle properties file management
        TSPropertiesHandler handler;
        try {
            handler = new TSPropertiesHandler(excelReadings, dateKey);
        } catch (IOException | IllegalArgumentException e) {
            LOGGER.error(HANDLER_ERROR_MSG, e);
            throw new JPSRuntimeException(HANDLER_ERROR_MSG, e);
        }
        LOGGER.info("Properties handler initialized.");

        // Generate data IRI mappings to measures names and store in an external properties file
        Map<String, String> iriMappings;
        try {
            iriMappings = handler.generateIRIMappings(args[0]);
        } catch (IOException e) {
            LOGGER.error(GET_IRIMAP_ERROR_MSG, e);
            throw new JPSRuntimeException(GET_IRIMAP_ERROR_MSG, e);
        }
        LOGGER.info("Data IRI mappings to their Excel header names have been generated/retrieved. " +
                "They can be found in a properties file at: " + args[0]);

        // Initialize and set the time series client for this agent
        DateTSClientDecorator agentTSClient;
        try {
            RemoteStoreClient kbClient = new RemoteStoreClient(
                    clientConfig.get(FileManager.QUERY_ENDPOINT_KEY),
                    clientConfig.get(FileManager.UPDATE_ENDPOINT_KEY));
            TimeSeriesClient<Instant> tsClient = new TimeSeriesClient<>(kbClient, Instant.class);
            agentTSClient = new DateTSClientDecorator(dateKey, dateArrays);
            agentTSClient.setTsClient(tsClient);
            agentTSClient.setRDBClient(clientConfig.get(FileManager.RDB_URL_KEY), clientConfig.get(FileManager.RDB_USER_KEY), clientConfig.get(FileManager.RDB_PASS_KEY));
        } catch (JPSRuntimeException e) {
            LOGGER.error(TSCLIENT_ERROR_MSG, e);
            throw new JPSRuntimeException(TSCLIENT_ERROR_MSG, e);
        }
        LOGGER.info("Time series client wrapper initialized.");

        // Initialize the time series database if it doesn't exist
        try {
            agentTSClient.initializeTimeSeries(excelReadings, iriMappings);
        } catch (Exception e) {
            LOGGER.error(INITIALIZE_ERROR_MSG, e);
            throw new JPSRuntimeException(INITIALIZE_ERROR_MSG, e);
        }

        try {
            agentTSClient.updateData(excelReadings, iriMappings);
        } catch (Exception e) {
            LOGGER.error(DATA_UPDATE_ERROR_MSG, e);
            throw new JPSRuntimeException(DATA_UPDATE_ERROR_MSG, e);
        }

            OntoBimAdapter.addSupplementaryTriples(
                clientConfig.get(FileManager.QUERY_ENDPOINT_KEY),
                clientConfig.get(FileManager.UPDATE_ENDPOINT_KEY), singleton);
        LOGGER.info("Data updated with new readings from Excel Workbook.");
        jsonMessage.put("Result", "Data updated with new readings from Excel Workbook.");
        return jsonMessage;
    }
}
