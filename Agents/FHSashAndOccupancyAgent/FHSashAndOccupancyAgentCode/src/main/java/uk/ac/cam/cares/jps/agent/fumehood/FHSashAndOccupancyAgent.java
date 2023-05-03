package uk.ac.cam.cares.jps.agent.fumehood;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
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

@WebServlet(urlPatterns = {"/retrieve"})
public class FHSashAndOccupancyAgent extends JPSAgent {
    private static final Logger LOGGER = LogManager.getLogger(FHSashAndOccupancyAgent.class);


    public static final String PARAMETERS_VALIDATION_ERROR_MSG = "Unable to validate request sent to the agent.";
    public static final String EMPTY_PARAMETER_ERROR_MSG = "Empty Request.";
    public static final String AGENT_CONSTRUCTION_ERROR_MSG = "The Agent could not be constructed.";
    public static final String LOADTSCLIENTCONFIG_ERROR_MSG = "Unable to load timeseries client configs!";
    public static final String QUERYSTORE_CONSTRUCTION_ERROR_MSG = "Unable to construct QueryStore!";
    public static final String GETLATESTDATA_ERROR_MSG = "Unable to get latest timeseries data for the following IRI: ";

    String dbUrl;
    String dbUsername;
    String dbPassword;
    String sparqlQueryEndpoint;
    String sparqlUpdateEndpoint;
    String bgUsername;
    String bgPassword;

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
     * Handle GET request and route to different functions based on the path.
     * @param requestParams Parameters sent with HTTP request
     * @param request HTTPServletRequest instance
     * @return result of the request
     */
    @Override
    public JSONObject processRequestParameters(JSONObject requestParams, HttpServletRequest request) {
        SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss.SSSS");
        String datetime = dateFormat.format(new Date());
        LOGGER.info("Request received at: {}", datetime);

        JSONObject msg = new JSONObject();

        String url = request.getRequestURI();

        if (url.contains("status")) {
            msg = getStatus();
        }

        if (url.contains("retrieve")) {
            msg = runAgent();
        }

        return msg;
        
    }

    private JSONObject runAgent() {
        QueryStore queryStore;
        Map<String, List<String>> map = new HashMap<>();
        try {
            loadTSClientConfigs(System.getenv("CLIENTPROPERTIES"));
        } catch (IOException e) {
            LOGGER.info("Unable to read timeseries client configs from properties file");
            LOGGER.info("Attempting to load configs via stack clients...");
        }

        try {
            queryStore = new QueryStore(sparqlUpdateEndpoint, sparqlQueryEndpoint, bgUsername, bgPassword);
        } catch (IOException e) {
            throw new JPSRuntimeException(QUERYSTORE_CONSTRUCTION_ERROR_MSG, e);
        }
        map = queryStore.queryForFHandWFHDevices();
        map = queryStore.queryForOccupancyState(map);
        map = queryStore.queryForSashOpening(map);

        setTsClientAndRDBClient(dbUsername, dbPassword, dbUrl, bgUsername, bgPassword, sparqlUpdateEndpoint, sparqlQueryEndpoint);
        map = getOccupiedStateTsData(map);
        map = getSashOpeningTsData(map);

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
        result.put("description", "BMSQueryAgent is ready.");
        return result;
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
                this.dbUrl = prop.getProperty("db.url");
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
            if (prop.containsKey("bg.username")) {
                this.bgUsername = prop.getProperty("bg.username");
            }
            if (prop.containsKey("bg.password")) {
                this.bgPassword = prop.getProperty("bg.password");
            }
        }
    }

    private void setTsClientAndRDBClient(String dbUsername, String dbPassword, String dbUrl, String bgUsername, String bgPassword, String sparqlUpdateEndpoint, String sparqlQueryEndpoint) {
        RemoteStoreClient kbClient = new RemoteStoreClient();
        kbClient.setQueryEndpoint(sparqlQueryEndpoint);
        kbClient.setUpdateEndpoint(sparqlUpdateEndpoint);
        kbClient.setUser(bgUsername);
        kbClient.setPassword(bgPassword);

        this.tsClient = new TimeSeriesClient<>(kbClient ,OffsetDateTime.class);
        this.RDBClient = new RemoteRDBStoreClient(dbUrl, dbUsername, dbPassword);
    }

    private Map<String, List<String>> getOccupiedStateTsData (Map<String, List<String>> map) {
        map.put("OccupiedStateTsData", new ArrayList<>());

        for (int i = 0; i < map.get("FHandWFH").size(); i++) {
            String occupiedStateIRI = map.get("OccupancyIRIs").get(i);

            if (occupiedStateIRI != "This device does not have an occupied state.") {
                try (Connection conn = RDBClient.getConnection()) {
                    timeseries = tsClient.getLatestData(occupiedStateIRI, conn);
                    map.get("OccupiedStateTsData").add(timeseries.getValuesAsString(occupiedStateIRI).get(timeseries.getValuesAsString(occupiedStateIRI).size() - 1));
                } catch (Exception e) {
                    throw new JPSRuntimeException(GETLATESTDATA_ERROR_MSG + occupiedStateIRI);
                }
            } else {
                map.get("OccupiedStateTsData").add("This device does not have an occupied state.");
            }
        }

        return map;
    }

    private Map<String, List<String>> getSashOpeningTsData (Map<String, List<String>> map) {
        map.put("SashOpeningTsData", new ArrayList<>());

        for (int i = 0; i < map.get("FHandWFH").size(); i++) {
            String sashOpeningIRI = map.get("SashOpeningIRIs").get(i);

            if (sashOpeningIRI != "This device does not have a Sash Opening Percentage.") {
                try (Connection conn = RDBClient.getConnection()) {
                    timeseries = tsClient.getLatestData(sashOpeningIRI, conn);
                    map.get("SashOpeningTsData").add(timeseries.getValuesAsString(sashOpeningIRI).get(timeseries.getValuesAsString(sashOpeningIRI).size() - 1));
                } catch (Exception e) {
                    throw new JPSRuntimeException(GETLATESTDATA_ERROR_MSG + sashOpeningIRI);
                }
            } else {
                map.get("SashOpeningTsData").add("This device does not have a sash opening.");
            }
        }

        return map;
    }

}
