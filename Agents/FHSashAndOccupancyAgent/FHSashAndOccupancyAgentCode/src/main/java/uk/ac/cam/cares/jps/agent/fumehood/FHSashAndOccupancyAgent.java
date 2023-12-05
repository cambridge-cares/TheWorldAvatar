package uk.ac.cam.cares.jps.agent.fumehood;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONException;
import org.json.JSONObject;
import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.sql.Connection;
import java.text.SimpleDateFormat;
import java.time.OffsetDateTime;
import java.util.*;

@WebServlet(urlPatterns = {"/status", "/retrieve", "/latestsash"})
public class FHSashAndOccupancyAgent extends JPSAgent {
    private static final Logger LOGGER = LogManager.getLogger(FHSashAndOccupancyAgent.class);


    public static final String PARAMETERS_VALIDATION_ERROR_MSG = "Unable to validate request sent to the agent.";
    public static final String THRESHOLD_ERROR_MSG = "Missing sash opening threshold value in request!";
    public static final String DELAY_ERROR_MSG = "Missing delay value in request!";
    public static final String EMPTY_PARAMETER_ERROR_MSG = "Empty Request.";
    public static final String AGENT_CONSTRUCTION_ERROR_MSG = "The Agent could not be constructed.";
    public static final String LOADTSCLIENTCONFIG_ERROR_MSG = "Unable to load timeseries client configs!";
    public static final String QUERYSTORE_CONSTRUCTION_ERROR_MSG = "Unable to construct QueryStore!";
    public static final String GETLATESTDATA_ERROR_MSG = "Unable to get latest timeseries data for the following IRI: ";
    public static final String NO_DEVICE_IRI = "No device IRI is provided.";
    private static final String GETFHANDWFHDEVICES_ERROR_MSG = "Unable to query for fumehood and/or walkin-fumehood devices and their labels!";
    private static final String WAIT_ERROR_MSG = "An error has occurred while waiting!";

    String dbUrlForOccupiedState;
    String dbUsernameForOccupiedState;
    String dbPasswordForOccupiedState;
    String dbUrlForSashOpening;
    String dbUsernameForSashOpening;
    String dbPasswordForSashOpening;
    String sparqlQueryEndpoint;
    String sparqlUpdateEndpoint;
    String bgUsername;
    String bgPassword;
    Double thresholdValue;
    int delayMinutes;

    TimeSeriesClient<OffsetDateTime> tsClient;
    RemoteRDBStoreClient RDBClient;
    TimeSeries<OffsetDateTime> timeseries;

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
            try {
            thresholdValue = Double.parseDouble(requestParams.getString("sashThreshold"));
            } catch (Exception e) {
                throw new JPSRuntimeException(THRESHOLD_ERROR_MSG);
            }

            try {
            delayMinutes = Integer.parseInt(requestParams.getString("delayMinutes"));
            } catch (Exception e) {
                throw new JPSRuntimeException(DELAY_ERROR_MSG);
            }
            msg = runAgent();
        }

        if (url.contains("latestsash")) {
            try {
                String deviceIri = requestParams.getString("deviceIri");
                // invalid deviceIRI will cause query of SashOpeningIRI to failed and is handled in getSashOpeningTsData()
                // msg will contain the error message, and the request will have respond code 200
                msg = getLatestSash(deviceIri);
            } catch (JSONException e) {
                throw new JPSRuntimeException(NO_DEVICE_IRI);
            }
        }

        return msg;
        
    }

    private QueryStore setupQueryStore() {
        try {
            loadTSClientConfigs(System.getenv("CLIENTPROPERTIES_01"),System.getenv("CLIENTPROPERTIES_02"));
        } catch (IOException e) {
            throw new JPSRuntimeException(LOADTSCLIENTCONFIG_ERROR_MSG, e);
        }

        try {
            return new QueryStore(sparqlUpdateEndpoint, sparqlQueryEndpoint, bgUsername, bgPassword);
        } catch (IOException e) {
            throw new JPSRuntimeException(QUERYSTORE_CONSTRUCTION_ERROR_MSG);
        }
    }

    /**
     * Initialise agent
     * @return successful initialisation message
     */
    private JSONObject runAgent() {
        QueryStore queryStore = setupQueryStore();
        Map<String, List<String>> map = new HashMap<>();

        try {
        map = queryStore.queryForFHandWFHDevices();
        } catch (Exception e) {
            throw new JPSRuntimeException(GETFHANDWFHDEVICES_ERROR_MSG);
        }

        map.put("OccupiedStateIRIs", new ArrayList<>());
        map.put("SashOpeningIRIs", new ArrayList<>());

        for (int i = 0; i < map.get("FHandWFH").size(); i++){
            String IRI = queryStore.queryForOccupiedState(map.get("FHandWFH").get(i));
            map.get("OccupiedStateIRIs").add(IRI);
        }

        for (int i = 0; i < map.get("FHandWFH").size(); i++){
            String IRI = queryStore.queryForSashOpening(map.get("FHandWFH").get(i));
            map.get("SashOpeningIRIs").add(IRI);
        }
        
        setTsClientAndRDBClient(dbUsernameForOccupiedState, dbPasswordForOccupiedState, dbUrlForOccupiedState, bgUsername, bgPassword, sparqlUpdateEndpoint, sparqlQueryEndpoint);

        map = getOccupiedStateTsData(map);

        setTsClientAndRDBClient(dbUsernameForSashOpening, dbPasswordForSashOpening, dbUrlForSashOpening, bgUsername, bgPassword, sparqlUpdateEndpoint, sparqlQueryEndpoint);

        map = getSashOpeningTsData(map);

        if (checkSashAndOccupancy(map, thresholdValue)) {
            try {
                Thread.sleep(delayMinutes * 60 * 1000);
            } catch (InterruptedException e) {
                throw new JPSRuntimeException(WAIT_ERROR_MSG, e);
            }
            
            setTsClientAndRDBClient(dbUsernameForOccupiedState, dbPasswordForOccupiedState, dbUrlForOccupiedState, bgUsername, bgPassword, sparqlUpdateEndpoint, sparqlQueryEndpoint);

            map = getOccupiedStateTsData(map);

            setTsClientAndRDBClient(dbUsernameForSashOpening, dbPasswordForSashOpening, dbUrlForSashOpening, bgUsername, bgPassword, sparqlUpdateEndpoint, sparqlQueryEndpoint);

            map = getSashOpeningTsData(map);

            if (checkSashAndOccupancy(map, thresholdValue)) {
                EmailBuilder emailBuilder = new EmailBuilder();
                String emailContent = emailBuilder.parsesMapAndPostProcessing(map, thresholdValue);
                emailBuilder.sendEmail(emailContent);
            }
        }

        LOGGER.info( map.get("FHandWFH").toString());
        LOGGER.info( map.get("Label").toString());
        LOGGER.info( map.get("OccupiedStateIRIs").toString());
        LOGGER.info( map.get("SashOpeningIRIs").toString());
        LOGGER.info(map.get("OccupiedStateTsData").toString());
        LOGGER.info(map.get("SashOpeningTsData").toString());
        
        JSONObject msg = new JSONObject();
        msg.put("result", "Agent has successfully query and check through all fumehoods and walkin-fumehoods.");
        return msg;
    }

    /**
     * Handle GET /status route and return the status of the agent.
     * @return Status of the agent
     */
    private JSONObject getStatus() {
        LOGGER.info("Detected request to get agent status...");
        JSONObject result = new JSONObject();
        result.put("description", "FHSashAndOccupancyAgent is ready.");
        return result;
    }

    /**
     * Reads the parameters needed for the timeseries client
     * @param filepath Path to the properties file from which to read the parameters
     * @param filepath2 Path to second properties file from which to read the parameters
     */
    private void loadTSClientConfigs(String filepath , String filepath2) throws IOException {
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
                this.dbUrlForOccupiedState = prop.getProperty("db.url");
            } else {
                throw new IOException("Properties file is missing \"db.url=<db_url>\"");
            }
            if (prop.containsKey("db.user")) {
                this.dbUsernameForOccupiedState = prop.getProperty("db.user");
            } else {
                throw new IOException("Properties file is missing \"db.user=<db_user>\"");
            }
            if (prop.containsKey("db.password")) {
                this.dbPasswordForOccupiedState = prop.getProperty("db.password");
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
            if (prop.containsKey("bg.username")) {
                this.bgUsername = prop.getProperty("bg.username");
            }
            if (prop.containsKey("bg.password")) {
                this.bgPassword = prop.getProperty("bg.password");
            }
        }

        // Check whether properties file exists at specified location
        file = new File(filepath2);
        if (!file.exists()) {
            throw new FileNotFoundException("No properties file found at specified filepath: " + filepath2);
        }

        // Try-with-resource to ensure closure of input stream
        try (InputStream input = new FileInputStream(file)) {

            // Load properties file from specified path
            Properties prop = new Properties();
            prop.load(input);

            // Get timeseries client parameters from properties file
            if (prop.containsKey("db.url")) {
                this.dbUrlForSashOpening = prop.getProperty("db.url");
            } else {
                throw new IOException("Properties file is missing \"db.url=<db_url>\"");
            }
            if (prop.containsKey("db.user")) {
                this.dbUsernameForSashOpening = prop.getProperty("db.user");
            } else {
                throw new IOException("Properties file is missing \"db.user=<db_user>\"");
            }
            if (prop.containsKey("db.password")) {
                this.dbPasswordForSashOpening = prop.getProperty("db.password");
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
            if (prop.containsKey("bg.username")) {
                this.bgUsername = prop.getProperty("bg.username");
            }
            if (prop.containsKey("bg.password")) {
                this.bgPassword = prop.getProperty("bg.password");
            }
        }
    }

    /**
     * set tsClient and RDBClient
     * @param dbUsername username for accessing the postgreSQL database
     * @param dbPassword password for accessing the postgreSQL database
     * @param dbUrl jdbc URL for accessing the postgreSQL database
     * @param bgUsername username for accessing an authentication enabled blazegraph
     * @param bgPassword password for accessing an authentication enabled blazegraph
     * @param sparqlUpdateEndpoint sparql endpoint for executing updates
     * @param sparqlQueryEndpoint sparql endpoint for executing queries
     */
    private void setTsClientAndRDBClient(String dbUsername, String dbPassword, String dbUrl, String bgUsername, String bgPassword, String sparqlUpdateEndpoint, String sparqlQueryEndpoint) {
        RemoteStoreClient kbClient = new RemoteStoreClient();
        kbClient.setQueryEndpoint(sparqlQueryEndpoint);
        kbClient.setUpdateEndpoint(sparqlUpdateEndpoint);
        kbClient.setUser(bgUsername);
        kbClient.setPassword(bgPassword);

        tsClient = new TimeSeriesClient<>(kbClient ,OffsetDateTime.class);
        RDBClient = new RemoteRDBStoreClient(dbUrl, dbUsername, dbPassword);
    }

    /**
     * Retrieve timeseries data for occupied state
     * @param map map that consists of several keys where each key has its own List of Strings
     */
    public Map<String, List<String>> getOccupiedStateTsData (Map<String, List<String>> map) {
        map.put("OccupiedStateTsData", new ArrayList<>());
        map.put("OccupiedStateTimeStamps", new ArrayList<>());
        for (int i = 0; i < map.get("FHandWFH").size(); i++) {
            String occupiedStateIRI = map.get("OccupiedStateIRIs").get(i);

            if (!occupiedStateIRI.contains("This device does not have a occupied state.")) {
                try (Connection conn = RDBClient.getConnection()) {
                    timeseries = tsClient.getLatestData(occupiedStateIRI, conn);
                    map.get("OccupiedStateTsData").add(timeseries.getValuesAsString(occupiedStateIRI).get(timeseries.getValuesAsString(occupiedStateIRI).size() - 1));

                    OffsetDateTime latestTimeStamp = timeseries.getTimes().get(timeseries.getTimes().size() - 1);
                    Date date = new java.util.Date(latestTimeStamp.toEpochSecond()*1000);
                    SimpleDateFormat sdf = new java.text.SimpleDateFormat("yyyy-MM-dd HH:mm:ss a z");
                    sdf.setTimeZone(TimeZone.getDefault());
                    Object ts = sdf.format(date);
                    map.get("OccupiedStateTimeStamps").add(ts.toString());
                } catch (Exception e) {
                    throw new JPSRuntimeException(GETLATESTDATA_ERROR_MSG + occupiedStateIRI);
                }
            } else {
                map.get("OccupiedStateTsData").add("This device does not have an occupied state.");
                map.get("OccupiedStateTimeStamps").add("Not applicable");
            }
        }
        return map;
    }

    /**
     * Retrieve timeseries data for sash opening
     * @param map map that consists of several keys where each key has its own List of Strings
     */
    public Map<String, List<String>> getSashOpeningTsData (Map<String, List<String>> map) {
        map.put("SashOpeningTsData", new ArrayList<>());
        map.put("SashOpeningTimeStamps", new ArrayList<>());
        for (int i = 0; i < map.get("FHandWFH").size(); i++) {
            String sashOpeningIRI = map.get("SashOpeningIRIs").get(i);

            if (!sashOpeningIRI.contains("This device does not have a Sash Opening Percentage.")) {
                try (Connection conn = RDBClient.getConnection()) {
                    timeseries = tsClient.getLatestData(sashOpeningIRI, conn);
                    map.get("SashOpeningTsData").add(timeseries.getValuesAsString(sashOpeningIRI).get(timeseries.getValuesAsString(sashOpeningIRI).size() - 1));

                    OffsetDateTime latestTimeStamp = timeseries.getTimes().get(timeseries.getTimes().size() - 1);
                    Date date = new java.util.Date(latestTimeStamp.toEpochSecond()*1000);
                    SimpleDateFormat sdf = new java.text.SimpleDateFormat("yyyy-MM-dd HH:mm:ss a z");
                    sdf.setTimeZone(TimeZone.getDefault());
                    Object ts = sdf.format(date);
                    map.get("SashOpeningTimeStamps").add(ts.toString());
                } catch (Exception e) {
                    throw new JPSRuntimeException(GETLATESTDATA_ERROR_MSG + sashOpeningIRI);
                }
            } else {
                map.get("SashOpeningTsData").add("This device does not have a sash opening.");
                map.get("SashOpeningTimeStamps").add("Not applicable");
            }
        }
        return map;
    }

    /**
     * Check sash and occupancy values
     * @param map map that consists of several keys where each key has its own List of Strings
     * @return boolean of whether there exist a fumehood that is not occupied and have a sash opening value of more than the threshold
     */
    private Boolean checkSashAndOccupancy(Map<String, List<String>> map, Double thresholdValue) {
        Boolean check = false;
        for (int i = 0; i < map.get("FHandWFH").size(); i++){
            String occupiedStateData = map.get("OccupiedStateTsData").get(i);
            String sashOpeningData = map.get("SashOpeningTsData").get(i);
            if (occupiedStateData.contains("This device does not have an occupied state.") | sashOpeningData.contains("This device does not have a sash opening.")) {
                check = false;
            } else {
                if (Double.parseDouble(occupiedStateData) == 0.0 && Double.parseDouble(sashOpeningData) > thresholdValue) {
                    check = true;
                    return check;
                }
            }
        }
        LOGGER.info("Check is " + check);
        return check;
    }

    /**
     * Get the latest sash value and the time stamp
     * @param deviceIri the IRI of FH/WFH
     * @return the latest sash value and the time stamp
     */
    private JSONObject getLatestSash(String deviceIri) {
        QueryStore queryStore = setupQueryStore();

        Map<String, List<String>> map = new HashMap<>();
        map.put("FHandWFH", Collections.singletonList(deviceIri));

        String sashOpeningIri = queryStore.queryForSashOpening(deviceIri);
        map.put("SashOpeningIRIs", Collections.singletonList(sashOpeningIri));

        setTsClientAndRDBClient(dbUsernameForSashOpening, dbPasswordForSashOpening, dbUrlForSashOpening, bgUsername, bgPassword, sparqlUpdateEndpoint, sparqlQueryEndpoint);
        map = getSashOpeningTsData(map);

        LOGGER.info(map.get("FHandWFH").toString());
        LOGGER.info(map.get("SashOpeningIRIs").toString());
        LOGGER.info(map.get("SashOpeningTsData").toString());

        JSONObject result = new JSONObject();
        result.put("sash", map.get("SashOpeningTsData").get(0));
        result.put("time", map.get("SashOpeningTimeStamps").get(0));
        return result;
    }

}
