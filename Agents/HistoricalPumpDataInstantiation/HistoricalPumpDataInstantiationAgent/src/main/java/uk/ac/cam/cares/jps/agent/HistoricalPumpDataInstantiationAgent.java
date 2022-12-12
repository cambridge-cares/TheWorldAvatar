package uk.ac.cam.cares.jps.agent;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONObject;
import uk.ac.cam.cares.jps.agent.sparql.SparqlAdapter;
import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesSparql;

import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.BadRequestException;
import java.io.IOException;
import java.time.Instant;
import java.util.List;
import java.util.Map;

/**
 * This class acts as an agent that receives POST requests, and coordinate with the other classes
 * to instantiate the timeseries data from Excel into a SPARQL endpoint and RDB.
 *
 * @author qhouyee
 */
@WebServlet(urlPatterns = {"/run"})
public class HistoricalPumpDataInstantiationAgent extends JPSAgent {
    /**
     * Logger for reporting info/errors.
     */
    private static final Logger LOGGER = LogManager.getLogger(HistoricalPumpDataInstantiationAgent.class);
    /**
     * Logging / error messages
     */
    private static final String ARGUMENT_MISMATCH_MSG = "Require 3 arguments! Please send the time header, starting value row, and col index of the group time series.";
    private static final String PARSER_ERROR_MSG = "Could not construct the Excel parser needed to interact with the Excel Workbook!";
    private static final String GET_READINGS_ERROR_MSG = "Readings could not be retrieved!";
    private static final String HANDLER_ERROR_MSG = "Could not construct the time series Properties handler!";
    private static final String GET_IRIMAP_ERROR_MSG = "IRI mappings could not be generated or retrieved!";
    private static final String TSCLIENT_ERROR_MSG = "Could not construct the time series client needed by the input agent!";
    private static final String INITIALIZE_ERROR_MSG = "Could not initialize time series.";
    private static final String DATA_UPDATE_ERROR_MSG = "Could not update time series!";
    // POST request keys
    private static final String KEY_TIMEHEADER = "timeHeader";
    private static final String KEY_IRI_PREFIX = "iriPrefix";
    private static final String KEY_ADD_TRIPLE = "addTriple";
    private static final String KEY_STARTING_ROW = "startingRow";
    private static final String KEY_MULTI_TS_COL_INDEX = "multiTSColIndex";
    public static String iriPrefix = TimeSeriesSparql.TIMESERIES_NAMESPACE; // The prefix to use for generating IRI

    @Override
    public JSONObject processRequestParameters(JSONObject requestParams, HttpServletRequest request) {
        return processRequestParameters(requestParams);
    }

    @Override
    public JSONObject processRequestParameters(JSONObject requestParams) {
        JSONObject jsonMessage = new JSONObject();
        if (validateInput(requestParams)) {
            LOGGER.info("Passing request to Historical Pump Data Instantiation Agent..");
            // Initialise required values
            String timeKey = requestParams.getString(KEY_TIMEHEADER);
            String iriValue = requestParams.getString(KEY_IRI_PREFIX);
            String addTriple = requestParams.getString(KEY_ADD_TRIPLE);
            String startingRow = requestParams.has(KEY_STARTING_ROW) ? requestParams.getString(KEY_STARTING_ROW) : "1";
            // If there is no grouping key, set an invalid value
            String bulkColIndex = requestParams.has(KEY_MULTI_TS_COL_INDEX) ? requestParams.getString(KEY_MULTI_TS_COL_INDEX) : "-1";
            // Modify iriPrefix to new IRI if it is a new address
            if (iriValue.startsWith("http")) {
                iriPrefix = iriValue;
            } else {
                // If it is only a namespace, append it to the preset time series namespace
                iriPrefix += iriValue;
            }
            String[] parameters = new String[]{timeKey, addTriple, startingRow, bulkColIndex};
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
        if (requestParams.isEmpty()) {
            LOGGER.error("No parameters are passed!");
            validate = false;
        } else {
            LOGGER.info("Validating " + KEY_TIMEHEADER + " parameter");
            validate = requestParams.has(KEY_TIMEHEADER);
            if (validate) {
                // Ensure that the time header is of valid format
                validate = requestParams.getString(KEY_TIMEHEADER).equalsIgnoreCase("year") || requestParams.getString(KEY_TIMEHEADER).equalsIgnoreCase("timestamp");
                LOGGER.debug(KEY_TIMEHEADER + " is " + validate);

                LOGGER.info("Validating " + KEY_IRI_PREFIX + " parameter");
                validate = requestParams.has(KEY_IRI_PREFIX);
                if (validate) {
                    // Ensure that the IRI prefix is of valid format
                    String iriValue = requestParams.getString(KEY_IRI_PREFIX);
                    // If it is a new IRI
                    if (iriValue.startsWith("http")) {
                        validate = (iriValue.startsWith("http://www.") || iriValue.startsWith("https://www.")) && iriValue.endsWith("/");
                    } else {
                        // If it is only a namespace, check if it does not start with http or /
                        validate = !iriValue.startsWith("/") && iriValue.indexOf("/") == iriValue.length() - 1;
                    }
                    if (validate) {
                        LOGGER.info("Validating " + KEY_ADD_TRIPLE + " parameter");
                        validate = requestParams.has(KEY_ADD_TRIPLE);
                        if (validate) {
                            String addTripleBoolean = requestParams.getString(KEY_ADD_TRIPLE);
                            // Ensure that the boolean parameter is of valid String format
                            validate = addTripleBoolean.equalsIgnoreCase("False") || addTripleBoolean.equalsIgnoreCase("True");
                            LOGGER.debug(KEY_ADD_TRIPLE + " is " + validate);
                        }
                    }
                }
                LOGGER.debug(KEY_IRI_PREFIX + " is " + validate);
            }
            if (requestParams.has(KEY_STARTING_ROW)) {
                LOGGER.info("Detected " + KEY_STARTING_ROW + " parameter");
                LOGGER.info("Validating parameter...");
                try {
                    // Check that starting row key is an Integer otherwise, parameter is invalid
                    Integer.parseInt(requestParams.getString(KEY_STARTING_ROW));
                } catch (NumberFormatException e) {
                    validate = false;
                    LOGGER.debug(KEY_STARTING_ROW + " is false");

                }
            }
            if (requestParams.has(KEY_MULTI_TS_COL_INDEX)) {
                LOGGER.info("Detected " + KEY_MULTI_TS_COL_INDEX + " parameter");
                LOGGER.info("Validating parameter...");
                try {
                    // Check that the index of the grouping column for multiple TS is an Integer that is at least 0
                    // Otherwise, parameter is invalid
                    int index = Integer.parseInt(requestParams.getString(KEY_MULTI_TS_COL_INDEX));
                    validate = index >= 0;
                } catch (NumberFormatException e) {
                    validate = false;
                    LOGGER.debug(KEY_STARTING_ROW + " is false");
                }
            }
        }
        return validate;
    }

    public JSONObject initializeAgent(String[] args) {
        JSONObject jsonMessage = new JSONObject();
        if (args.length != 4) {
            LOGGER.error(ARGUMENT_MISMATCH_MSG);
            throw new JPSRuntimeException(ARGUMENT_MISMATCH_MSG);
        }
        LOGGER.debug("Launcher called with the following files: " + String.join(" ", args));

        // Create an Excel parser object to retrieve Excel content
        ExcelParser parser;
        try {
            parser = new ExcelParser(FileManager.retrieveExcelPath(), args[0]);
        } catch (IOException e) {
            LOGGER.error(PARSER_ERROR_MSG + e);
            throw new JPSRuntimeException(PARSER_ERROR_MSG, e);
        }
        LOGGER.info("Excel parser initialized.");

        // Parse Excel values into a hashmap
        Map<String, Map<String, List<?>>> excelReadings;
        try {
            excelReadings = parser.parseToHashMap(Integer.parseInt(args[2]), Integer.parseInt(args[3]));
        } catch (Exception e) {
            LOGGER.error(GET_READINGS_ERROR_MSG, e);
            throw new JPSRuntimeException(GET_READINGS_ERROR_MSG, e);
        }
        LOGGER.info(String.format("Retrieved %d Excel readings.", excelReadings.size()));

        // Initialize a Properties handler to handle properties file management
        TSPropertiesHandler handler;
        try {
            handler = new TSPropertiesHandler(excelReadings, args[0]);
        } catch (IOException | IllegalArgumentException e) {
            LOGGER.error(HANDLER_ERROR_MSG, e);
            throw new JPSRuntimeException(HANDLER_ERROR_MSG, e);
        }
        LOGGER.info("Properties handler initialized.");

        // Generate data IRI mappings to measures names and store in an external properties file
        Map<String, Map<String, String>> iriMappings;
        try {
            iriMappings = handler.generateIRIMappings(FileManager.PROPERTIES);
        } catch (IOException e) {
            LOGGER.error(GET_IRIMAP_ERROR_MSG, e);
            throw new JPSRuntimeException(GET_IRIMAP_ERROR_MSG, e);
        }
        LOGGER.info("Data IRI mappings to their Excel header names have been generated/retrieved. " +
                "They can be found in a properties file at the data directory.");

        // Initialize and set the time series client for this agent
        TimeSeriesClientDecorator agentTSClient;
        Map<String, String> clientConfig;
        try {
            clientConfig = FileManager.retrieveClientProperties();
            RemoteStoreClient kbClient = new RemoteStoreClient(
                    clientConfig.get(FileManager.QUERY_ENDPOINT_KEY),
                    clientConfig.get(FileManager.UPDATE_ENDPOINT_KEY));
            TimeSeriesClient<Instant> tsClient = new TimeSeriesClient<>(kbClient, Instant.class);
            agentTSClient = new TimeSeriesClientDecorator(args[0]);
            agentTSClient.setTsClient(tsClient);
            agentTSClient.setRDBClient(clientConfig.get(FileManager.RDB_URL_KEY), clientConfig.get(FileManager.RDB_USER_KEY), clientConfig.get(FileManager.RDB_PASS_KEY));
        } catch (JPSRuntimeException e) {
            LOGGER.error(TSCLIENT_ERROR_MSG, e);
            throw new JPSRuntimeException(TSCLIENT_ERROR_MSG, e);
        }
        LOGGER.info("Time series client wrapper initialized.");

        // Initialize and update time series for each group
        for (String group : excelReadings.keySet()) {
            LOGGER.debug("Initializing time series for " + group + "...");
            // Initialize the time series database if it doesn't exist
            try {
                agentTSClient.initializeTimeSeries(excelReadings.get(group), iriMappings.get(group));
            } catch (Exception e) {
                LOGGER.error(INITIALIZE_ERROR_MSG, e);
                throw new JPSRuntimeException(INITIALIZE_ERROR_MSG, e);
            }

            LOGGER.debug("Updating time series for " + group + "...");
            try {
                agentTSClient.updateData(excelReadings.get(group), iriMappings.get(group));
            } catch (Exception e) {
                LOGGER.error(DATA_UPDATE_ERROR_MSG, e);
                throw new JPSRuntimeException(DATA_UPDATE_ERROR_MSG, e);
            }
        }
        LOGGER.debug("Adding supplementary triples...");
        boolean addTripleBoolean = args[1].equalsIgnoreCase("True");
        if (addTripleBoolean) {
            SparqlAdapter.addSupplementaryTriples(clientConfig.get(FileManager.QUERY_ENDPOINT_KEY),
                    clientConfig.get(FileManager.UPDATE_ENDPOINT_KEY),
                    iriMappings);
        }
        LOGGER.info("Data updated with new readings from Excel Workbook.");
        jsonMessage.put("Result", "Data updated with new readings from Excel Workbook.");
        return jsonMessage;
    }
}
