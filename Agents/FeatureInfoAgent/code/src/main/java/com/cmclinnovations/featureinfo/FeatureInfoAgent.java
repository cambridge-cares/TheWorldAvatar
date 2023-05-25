package com.cmclinnovations.featureinfo;

import java.io.IOException;
import java.sql.Connection;
import java.text.SimpleDateFormat;
import java.time.Instant;
import java.util.Arrays;
import java.util.Date;
import java.util.List;
import java.util.Optional;
import java.util.regex.Pattern;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.ws.rs.BadRequestException;
import javax.ws.rs.core.Response;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONArray;
import org.json.JSONObject;
import org.springframework.stereotype.Controller;

import com.cmclinnovations.featureinfo.config.ConfigEndpoint;
import com.cmclinnovations.featureinfo.config.ConfigStore;
import com.cmclinnovations.featureinfo.config.EndpointType;
import com.cmclinnovations.featureinfo.kg.ClassHandler;
import com.cmclinnovations.featureinfo.kg.MetaHandler;
import com.cmclinnovations.featureinfo.kg.TimeHandler;

import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;

/**
 * This agent expects a HTTP request containing a JSON string with the "iri" and for an individual
 * feature. This information is then used (in conjunction with file-based SPARQL queries) to gather KG
 * data and (if available) timeseries data on that feature and return it as a JSON object.
 *
 * @author Michael Hillman {@literal <mdhillman@cmclinnovations.com>}
 */
@Controller
@WebServlet(urlPatterns = {"/get", "/status"})
public class FeatureInfoAgent extends JPSAgent {

    /**
     * Logger for reporting info/errors.
     */
    private static final Logger LOGGER = LogManager.getLogger(FeatureInfoAgent.class);

    /**
     * Reads and stores configuration details.
     */
    public static ConfigStore CONFIG = new ConfigStore();

    /**
     * Is the FeatureInfoAgent in a valid state.
     */
    private static boolean VALID = true;

    /**
     * Override for KG client to allow mocking during tests
     */
    protected static RemoteStoreClient RS_CLIENT_OVER;

    /**
     * Override for RDB client to allow mocking during tests
     */
    protected static RemoteRDBStoreClient RDB_CLIENT_OVER;

    /**
     * Override for Timeseries client to allow mocking during tests
     */
    protected static TimeSeriesClient<Instant> TS_CLIENT_OVER;

    /**
     * Common RDB connection
     */
    protected static Connection RDB_CONN;

    /**
     * If the request enforces an endpoint, cache it here.
     */
    private ConfigEndpoint enforcedEndpoint;

    /**
     * Load the configuration file.
     */
    private final static void loadConfig() {
        try {
            LOGGER.info("Attempting to load configuration settings...");
            CONFIG.load();
        } catch(Exception exception) {
            FeatureInfoAgent.VALID = false;
            LOGGER.error("Could not initialise agent configuration!", exception);
        }
    }
    
    /**
     * Perform required setup.
     *
     * @throws ServletException if in invalid state
     */
    @Override
    public synchronized void init() throws ServletException {
        try {
            super.init();

            // Test the logging
            LOGGER.debug("This is a test DEBUG message");
            LOGGER.info("This is a test INFO message");
            LOGGER.warn("This is a test WARN message");
            LOGGER.error("This is a test ERROR message");
            LOGGER.fatal("This is a test FATAL message");
            System.out.println("This is a test SYSTEM.OUT message");

            // Load configuration
            if(CONFIG == null) CONFIG = new ConfigStore();
            if(!CONFIG.isReady()) FeatureInfoAgent.loadConfig();

        } catch(Exception exception) {
            FeatureInfoAgent.VALID = false;
            LOGGER.error("Could not initialise an agent instance!", exception);
        }
    }

    /**
     * Processes HTTP requests with originating details.
     *
     * @param requestParams Request parameters as a JSONObject.
     * @param request HTTP Servlet Request.
     * 
     * @return response in JSON format.
     */
    @Override
    @SuppressWarnings("java:S1989")
    public void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
        response.setContentType("text/json");

        SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss.SSSS");
        String datetime = dateFormat.format(new Date());
        LOGGER.info("Request received at: {}", datetime);

        if(this.check(response)) {
            // Parse request as JSON
            JSONObject requestParams = AgentCaller.readJsonParameter(request);

            // Get the requested route
            String url = request.getRequestURI();
            url = url.substring(url.lastIndexOf("/"), url.length());
            if (url.contains("?")) url = url.split(Pattern.quote("?"))[0];

            // Run logic based on request path
            switch (url) {
                case "/get":
                case "get": {

                    // Enforce a single blazegraph endpoint
                    if(requestParams.has("endpoint")) {
                        LOGGER.info("Enforcing a single Blazegraph endpoint: {}", requestParams.getString("endpoint"));
                        
                        this.enforcedEndpoint = new ConfigEndpoint(
                            "ENFORCED", 
                            requestParams.getString("endpoint"), 
                            null, 
                            null,
                            EndpointType.BLAZEGRAPH
                        );
                    }

                    // Run main GET logic
                    getRoute(requestParams, response);
                }
                break;

                case "/status":
                case "status": {
                    // Return status
                    statusRoute(response);
                }
                break;

                default: {
                    LOGGER.info("Detected an unknown request route...");
                    response.setStatus(Response.Status.NOT_IMPLEMENTED.getStatusCode());
                    response.getWriter().write("{\"description\":\"Unknown route, only '/get' and '/status' are permitted.\"}");
                }
                break;
            }
        } 

        response.getWriter().flush();
        LOGGER.info("Call finished, response object's writer has been flushed.");
    }

    /**
     * Initiate logic required to process a request on the "/get" route.
     * 
     * @param requestParams HTTP request parameters
     * @param response HTTp response
     * 
     * @throws IOException
     */
    protected void getRoute(JSONObject requestParams, HttpServletResponse response) throws IOException {
        LOGGER.info("Detected request to get meta and timeseries data.");
        this.runLogic(requestParams, response);
    }

    /**
     * Run logic for the "/status" route.
     * 
     * @param response HTTO response
     * 
     * @throws IOException
     */
    protected void statusRoute(HttpServletResponse response) throws IOException {
        LOGGER.info("Detected request to get agent status...");

        if(CONFIG != null && FeatureInfoAgent.VALID) {
            response.setStatus(Response.Status.OK.getStatusCode());
            response.getWriter().write("{\"description\":\"Ready to serve.\"}");
        } else {
            response.setStatus(Response.Status.INTERNAL_SERVER_ERROR.getStatusCode());
            response.getWriter().write("{\"description\":\"Could not initialise agent instance!\"}");
        }
    }

    /**
     * Checks the incoming JSON request for validity.
     * 
     * @param requestParams JSON request parameters.
     * 
     * @return request validity.
     * @throws BadRequestException if request is malformed.
     */
    @Override
    public boolean validateInput(JSONObject requestParams) throws BadRequestException {
        // Check that there's an iri
        if (requestParams.isNull("iri")) {
            LOGGER.warn("Could not find the required 'iri' field.");
            return false;
        }
        return true;
    }

    /**
     * Runs the main agent logic for the /get route (on valid request)
     * 
     * @param requestParams JSONObject of request parameters
     * @param response HTTP response
     * 
     * @throws IOException
     */
    protected void runLogic(JSONObject requestParams, HttpServletResponse response) throws IOException {
        // Check if request is valid
        boolean validRequest = validateInput(requestParams);
        if(!validRequest) {
            response.setStatus(Response.Status.BAD_REQUEST.getStatusCode());
            response.getWriter().write("{\"description\":\"Bad request, no 'iri' parameter.\"}");
            return;
        }

        // Get the request parameters
        String iri = requestParams.optString("iri");
        LOGGER.info("Incoming 'iri' is {}", iri);

        try {
            // Determine the class match
            String classMatch = this.getClass(iri, response);
            
            if(classMatch != null) {
                // Get the metadata
                JSONArray metaArray = this.getMetadata(iri, classMatch, response);

                // Get the timeseries
                JSONArray timeArray = this.getTimeseries(iri, classMatch, response);

                // Combine into single response
                JSONObject result = new JSONObject();
                if(metaArray != null) result.put("meta", metaArray);
                if(timeArray != null) result.put("time", timeArray);

                // Return result
                response.setStatus(Response.Status.OK.getStatusCode());
                response.getWriter().write(result.toString());

                LOGGER.info("JSON data has been written to the response object.");
            }

        } catch(Exception exception) {
            LOGGER.error("An unexpected exception has occured, please see the log file!", exception);
            
            response.setStatus(Response.Status.INTERNAL_SERVER_ERROR.getStatusCode());
            response.getWriter().write("{\"description\":\"" + exception.getMessage() + "\"}");
        }
    }

    /**
     * Determine the class match for the input IRI.
     * 
     * @param iri asset IRI
     * @param response servlet response
     * 
     * @return name of matching class
     */
    private final String getClass(String iri, HttpServletResponse response) throws IOException {
        // Get Blazegraph endpoints
        List<ConfigEndpoint> endpoints = (this.enforcedEndpoint != null) ? Arrays.asList(this.enforcedEndpoint) : CONFIG.getBlazegraphEndpoints();
        LOGGER.debug("Running class queries against following endpoints via federation...");

        // Build class handler
        ClassHandler handler = new ClassHandler(iri, endpoints);

        // Construct clients
        RemoteStoreClient rsClient = new RemoteStoreClient();

        // Set the username and password for access (assumes all endpoints within same stack!)
        if(endpoints.get(0).username() != null && !endpoints.get(0).username().isEmpty()) {
            rsClient.setUser(endpoints.get(0).username());
            rsClient.setPassword(endpoints.get(0).password());

            LOGGER.info("Creating a RemoteStoreClient with username: {}", rsClient.getUser());
        }

        handler.setClient((RS_CLIENT_OVER != null) ? RS_CLIENT_OVER : rsClient);

        // Determine the class match
        try {
            String classMatch = handler.getClassMatch();
            LOGGER.info("Discovered class match is: {}", classMatch);

            if(classMatch == null) {
                LOGGER.info("Null class match!");

                response.setStatus(Response.Status.INTERNAL_SERVER_ERROR.getStatusCode());
                response.getWriter().write("{\"description\":\"Internal error occurred, could not query to determine classes.\"}");
                return null;
            } else if(classMatch.isEmpty()) {
                LOGGER.info("Empty class match!");

                response.setStatus(Response.Status.NO_CONTENT.getStatusCode());
                response.getWriter().write("{\"description\":\"Queries sent, but no classes could be determined.\"}");
                return null;
            }
           
            return classMatch;

        } catch(Exception exception) {
            LOGGER.error("Exception occurred when determining class match!", exception);

            response.setStatus(Response.Status.INTERNAL_SERVER_ERROR.getStatusCode());
            response.getWriter().write("{\"description\":\"Could not contact endpoints to determine class names.\"}");
        }
        return null;
    }

    /**
     * Queries for KG metadata.
     * 
     * @param iri asset IRI
     * @param classMatch name of matching class
     * @param response server response object
     * 
     * @returns metadata
     */
    protected JSONArray getMetadata(String iri, String classMatch, HttpServletResponse response) throws Exception {
        // Get Blazegraph endpoints
        List<ConfigEndpoint> endpoints = (this.enforcedEndpoint != null) ? Arrays.asList(this.enforcedEndpoint) : CONFIG.getBlazegraphEndpoints();

        // Build metadata handler
        MetaHandler handler = new MetaHandler(iri, classMatch, endpoints);

        // Construct clients
        RemoteStoreClient rsClient = new RemoteStoreClient();

        // Set the username and password for access (assumes all endpoints within same stack!)
        if(endpoints.get(0).username() != null && !endpoints.get(0).username().isEmpty()) {
            rsClient.setUser(endpoints.get(0).username());
            rsClient.setPassword(endpoints.get(0).password());

            LOGGER.info("Creating a RemoteStoreClient with username: {}", rsClient.getUser());
        }

        handler.setClient((RS_CLIENT_OVER != null) ? RS_CLIENT_OVER : rsClient);

        // Run queries
        LOGGER.info("Running query to gather metadata...");
        JSONArray result = handler.getData(response);

        if(result == null) {
            LOGGER.warn("Result from metadata query was null!");
        } else {
            LOGGER.info("Have result, contains {} entries.", result.length());
        }
        return result;
    }

    /**
     * Queries for timeseries data.
     * 
     * @param iri asset IRI
     * @param classMatch name of matching class
     * @param response server response object
     * 
     * @returns metadata
     */
    protected JSONArray getTimeseries(String iri, String classMatch, HttpServletResponse response) throws Exception {
        // Get postgres endpoint details
        Optional<ConfigEndpoint> postEndpoint = CONFIG.getPostgresEndpoint();
        if(!postEndpoint.isPresent()) {
            response.setStatus(Response.Status.INTERNAL_SERVER_ERROR.getStatusCode());
            response.getWriter().write("{\"description\":\"Could not determine the PostgreSQL endpoint.\"}");
            return null;
        }
        
        // Construct clients
        RemoteStoreClient rsClient = new RemoteStoreClient();
        TimeSeriesClient<Instant> tsClient = new TimeSeriesClient<>(
            rsClient, 
            Instant.class
        );

        // Get Blazegraph endpoints
        List<ConfigEndpoint> endpoints = (this.enforcedEndpoint != null) ? Arrays.asList(this.enforcedEndpoint) : CONFIG.getBlazegraphEndpoints();

        // Build RBD client
        String dbName = CONFIG.getDatabaseName(classMatch);
        if(dbName == null) {
            LOGGER.warn("No PostgreSQL database name registered for the class: {}", classMatch);
            LOGGER.warn("Skipping timeseries queries.");
            return null;
        }

        String dbURL = CONFIG.generatePostgresURL(dbName);
        LOGGER.info("Establishing connection to RBD for timeseries...");
        LOGGER.info("     Using URL: {}", dbURL);
        LOGGER.info("     Using Username: {}", postEndpoint.get().username());

        RemoteRDBStoreClient rdbClient = new RemoteRDBStoreClient(
            dbURL,
            postEndpoint.get().username(),
            postEndpoint.get().password()
        );

        // Build timeseries handler
        TimeHandler handler = new TimeHandler(iri, classMatch, endpoints);
        handler.setClients(
            (RS_CLIENT_OVER != null) ? RS_CLIENT_OVER : rsClient,
            (RDB_CLIENT_OVER != null) ? RDB_CLIENT_OVER : rdbClient,
            (TS_CLIENT_OVER != null) ? TS_CLIENT_OVER : tsClient
        );

        // Run queries and return timeseries JSON
        LOGGER.info("Running query to gather timeseries...");
        JSONArray result =  handler.getData(response);

        if(result == null) {
            LOGGER.warn("...result from timeseries query was null!");
        } else {
            LOGGER.info("...have result, contains {} entries.", result.length());
        }
        return result;
    }

    /**
     * Check that the request is valid and the agent is in a valid state.
     * 
     * @param response HTTP response
     */
    private final boolean check(HttpServletResponse response) throws IOException {
        // Check if in valid state
        if(CONFIG == null || !VALID)  {
            LOGGER.error("FeatureInfoAgent could not start in a valid state.");

            response.setStatus(Response.Status.INTERNAL_SERVER_ERROR.getStatusCode());
            response.getWriter().write("{\"description\":\"Could not initialise agent instance!\"}");
            return false;
        }
        return true;
    }

}
// End of class.
