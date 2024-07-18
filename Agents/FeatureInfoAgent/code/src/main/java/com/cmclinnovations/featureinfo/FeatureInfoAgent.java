package com.cmclinnovations.featureinfo;

import java.io.IOException;
import java.text.SimpleDateFormat;
import java.time.Instant;
import java.time.LocalTime;
import java.util.Date;
import java.util.regex.Pattern;

import javax.servlet.ServletContext;
import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.ws.rs.core.Response;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONObject;
import org.springframework.stereotype.Controller;
import com.fasterxml.jackson.databind.ObjectMapper;

import com.cmclinnovations.featureinfo.config.ConfigStore;
import com.cmclinnovations.featureinfo.objects.Request;
import com.cmclinnovations.featureinfo.utils.TimeSeriesCreator;

import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
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
@WebServlet(urlPatterns = {"/get", "/status", "/refresh", "/make-time-series"})
public class FeatureInfoAgent extends JPSAgent {

    /**
     * Cached servlet context.
     */
    public static ServletContext CONTEXT;

    /**
     * Logger for reporting info/errors.
     */
    private static final Logger LOGGER = LogManager.getLogger(FeatureInfoAgent.class);
    
    /**
     * Reads and stores configuration details.
     */
    private final ConfigStore configStore = new ConfigStore();

    /**
     * Is the FeatureInfoAgent in a valid state.
     */
    private boolean valid = true;

    /**
     * Manager class to run information gathering logic.
     */
    private QueryManager queryManager;

    /**
     * Object mapper used for Jackson serialisation and deserialiation.
     */
    private static final ObjectMapper OBJECT_MAPPER = new ObjectMapper();
    
    /**
     * Perform required setup.
     *
     * @throws ServletException if in invalid state
     */
    @Override
    public synchronized void init() throws ServletException {
        try {
            super.init();
            configStore.loadDetails();
            FeatureInfoAgent.CONTEXT = this.getServletContext();
        } catch(Exception exception) {
            this.valid = false;
            LOGGER.error("Could not initialise a valid FeatureInfoAgent instance!", exception);
        }
    }

    /**
     * Returns the information gathering instance.
     * 
     * @return QueryManager instance.
     */
    public QueryManager getQueryManager() {
        if(this.queryManager == null) {
            this.queryManager = new QueryManager(this.configStore);

            RemoteStoreClient kgClient = new RemoteStoreClient();
            TimeSeriesClient<Instant> tsClient = new TimeSeriesClient<>(kgClient, Instant.class);
            this.queryManager.setClients(kgClient, tsClient);
        }
        return this.queryManager;
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
        // Log time and date
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
                    // Run main GET logic
                     // Re-scan endpoints and reload configuration
                    try {
                        getRoute(requestParams, response);
                    } catch(Exception exception) {
                        LOGGER.error("Could not run /get route.", exception);
                        response.setStatus(Response.Status.INTERNAL_SERVER_ERROR.getStatusCode());
                        response.getWriter().write("{\"description\":\"Could run /get route successfully!\"}");
                    }
                }
                break;

                case "/refresh":
                case "refresh" : {
                    // Re-scan endpoints and reload configuration
                    try {
                        refreshRoute(requestParams, response);
                    } catch(Exception exception) {
                        LOGGER.error("Could not run /refresh route.", exception);
                        response.setStatus(Response.Status.INTERNAL_SERVER_ERROR.getStatusCode());
                        response.getWriter().write("{\"description\":\"Could not clear cached data structures!\"}");
                    }
                }
                break;

                case "/status":
                case "status": {
                    // Return status
                    statusRoute(response);
                }
                break;

                case "/make-time-series": {
                    // Undocumented route to generate sample time series data.
                    // Not to be used outside of very specific testing cases.
                    makeTimeSeries(response);
                }
                break;

                default: {
                    // Something else
                    LOGGER.info("Detected an unknown request route...");
                    response.setStatus(Response.Status.NOT_IMPLEMENTED.getStatusCode());
                    response.getWriter().write("{\"description\":\"Unknown route, only '/get', '/refresh', and '/status' are permitted.\"}");
                }
                break;
            }
        } 

        response.setContentType("text/json");
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
        LOGGER.info("Detected request to get meta and times series data.");

        if(!this.valid) {
            response.setStatus(Response.Status.INTERNAL_SERVER_ERROR.getStatusCode());
            response.getWriter().write("{\"description\":\"Agent is in invalid state, please check server logs.\"}");
            return;
        } 

        // Check for a valid request
        if(!this.getQueryManager().checkRequest(requestParams)) {
            response.setStatus(Response.Status.BAD_REQUEST.getStatusCode());
            response.getWriter().write("{\"description\":\"Request is missing required parameters, please check documentation.\"}");
            return;
        }

        // Run information gathering logic
        Request request = OBJECT_MAPPER.readValue(requestParams.toString(),Request.class);
        JSONObject result = this.getQueryManager().processRequest(request, response);
        if(result != null) {
            response.setStatus(Response.Status.OK.getStatusCode());
            response.getWriter().write(result.toString(2));
        }
    }

    /**
     * Run logic for the "/status" route.
     * 
     * @param response HTTP response.
     * 
     * @throws IOException if response cannot be written to.
     */
    protected void statusRoute(HttpServletResponse response) throws IOException {
        LOGGER.info("Detected request to get agent status...");

        if(this.valid) {
            response.setStatus(Response.Status.OK.getStatusCode());
            response.getWriter().write("{\"description\":\"Ready to serve.\"}");
        } else {
            response.setStatus(Response.Status.INTERNAL_SERVER_ERROR.getStatusCode());
            response.getWriter().write("{\"description\":\"Could not initialise a valid FeatureInfoAgent instance!\"}");
        }
    }

    /**
     * Run logic for the "/refresh" route, rescanning Blazegraph for available
     * endpoints and reloading the agent's configuration file.
     * 
     * @param requestParams JSONObject of request parameters.
     * @param response HTTP response.
     * 
     * @throws Exception if refresh does not succeed.
     */
    protected void refreshRoute(JSONObject requestParams, HttpServletResponse response) throws Exception {
        LOGGER.info("Detected request to refresh cached information...");

        if(!this.valid) {
            response.setStatus(Response.Status.INTERNAL_SERVER_ERROR.getStatusCode());
            response.getWriter().write("{\"description\":\"Agent is in invalid state, please check server logs.\"}");
            return;
        } 

        // Force refresh of configuration
        this.configStore.loadDetails();

        // Respond
        response.setStatus(Response.Status.OK.getStatusCode());
        response.getWriter().write(String.format("""
            {
                "description": "Cached endpoints and configurations have been refreshed.",
                "completed-at": "%s"

            }
            """, LocalTime.now().toString()));
    }

    /**
     * Check that the agent is in a valid state.
     * 
     * @param response HTTP response.
     */
    private final boolean check(HttpServletResponse response) throws IOException {
        if(!valid)  {
            response.setStatus(Response.Status.INTERNAL_SERVER_ERROR.getStatusCode());
            response.getWriter().write("{\"description\":\"Could not initialise a valid FeatureInfoAgent instance!\"}");
            return false;
        }
        return true;
    }

    /**
     * Check that the agent is in a valid state.
     * 
     * @param response HTTP response.
     */
    private final void makeTimeSeries(HttpServletResponse response) throws IOException {
        LOGGER.info("Detected request to generate sample time series data...");

        if(!this.valid) {
            response.setStatus(Response.Status.INTERNAL_SERVER_ERROR.getStatusCode());
            response.getWriter().write("{\"description\":\"Agent is in invalid state, please check server logs.\"}");
            return;
        } 

        // Make time series data
        TimeSeriesCreator creator = new TimeSeriesCreator(this.configStore);
        creator.createClients("sample-data", "postgres");
        creator.addSampleData();

        response.setStatus(Response.Status.OK.getStatusCode());
        response.getWriter().write(String.format("""
            {
                "description": "Have attempted to generate new time series in 'sample-data' namespace and 'castles' database.",
                "completed-at": "%s"

            }
            """, LocalTime.now().toString()));
        LOGGER.info("Generation complete at: {}", LocalTime.now().toString());
    }

}
// End of class.
