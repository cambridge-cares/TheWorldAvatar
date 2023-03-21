package uk.ac.cam.cares.jps.agent.ifc2ontobim;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONObject;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.ttlparser.StringUtils;
import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import java.nio.file.Paths;
import java.util.Set;

/**
 * This class acts as the entry point of the compiled war, and coordinates the two components (IfcOwlConverterAgent and
 * OntoBimAgent) to produce a TTL file with ontoBIM instances converted from an IFC model input.
 *
 * @author qhouyee
 */
@WebServlet(urlPatterns = {"/convert", "/status"})
public class Ifc2OntoBIMAgent extends JPSAgent {
    private static final Logger LOGGER = LogManager.getLogger(Ifc2OntoBIMAgent.class);
    // Agent starts off in valid state, and will be invalid when running into exceptions
    private static boolean VALID = true;
    private static final String INVALID_ROUTE_ERROR_MSG = "Invalid request type! Route ";
    private static final String IFCOWL_CONVERSION_ERROR_MSG = "Failed to convert to IfcOwl schema. ";
    private static final String KEY_BASEURI = "uri";
    private static final String KEY_ENDPOINT = "endpoint";
    private static final String ttlDir = Paths.get(System.getProperty("user.dir"), "data").toString();
    private static String endpoint;
    private static final String IFCOWL_API_URl = "http://ifcowlconverter:8080/ifcowlconverter/";

    /**
     * Perform required setup.
     */
    @Override
    public synchronized void init() {
        try {
            super.init();
            // Ensure logging are properly working
            LOGGER.debug("This is a test DEBUG message");
            LOGGER.info("This is a test INFO message");
            LOGGER.warn("This is a test WARN message");
            LOGGER.error("This is a test ERROR message");
            LOGGER.fatal("This is a test FATAL message");
        } catch (Exception exception) {
            Ifc2OntoBIMAgent.VALID = false;
            LOGGER.error("Could not initialise an agent instance!", exception);
        }
    }

    /**
     * An overloaded method to process all the different HTTP (GET/POST/PULL..) requests.
     * Do note all requests to JPS agents are processed similarly and will only return response objects.
     *
     * @return A response to the request called as a JSON Object.
     */
    @Override
    public JSONObject processRequestParameters(JSONObject requestParams, HttpServletRequest request) {
        return processRequestParameters(requestParams);
    }

    /**
     * A method that process all the different HTTP (GET/POST/PULL..) requests.
     * This will validate the incoming request type and parameters against their route options.
     *
     * @return A response to the request called as a JSON Object.
     */
    @Override
    public JSONObject processRequestParameters(JSONObject requestParams) {
        JSONObject jsonMessage = new JSONObject();
        // Retrieve the request type and route
        String requestType = requestParams.get("method").toString();
        String route = requestParams.get("requestUrl").toString();
        route = StringUtils.getStringAfterLastCharacterOccurrence(route, StringUtils.SLASH);
        LOGGER.info("Passing request to Ifc2OntoBIM Agent...");
        // Run logic based on request path
        switch (route) {
            case "convert":
                if (validateInput(requestParams)) {
                    // Process request parameters
                    String baseURI = requestParams.getString(KEY_BASEURI);
                    endpoint = requestParams.has(KEY_ENDPOINT) ? requestParams.getString(KEY_ENDPOINT) : "";
                    String[] args = (!baseURI.equals("default")) ? new String[]{baseURI} : new String[]{"default"};
                    jsonMessage = this.runAgent(args);
                } else if (!requestType.equals("POST")) {
                    LOGGER.fatal(INVALID_ROUTE_ERROR_MSG + route + " can only accept POST request.");
                    jsonMessage.put("Result", INVALID_ROUTE_ERROR_MSG + route + " can only accept POST request.");
                } else {
                    LOGGER.fatal("Request parameters are not defined correctly.");
                    jsonMessage.put("Result", "Request parameters are not defined correctly.");
                }
                break;
            case "status":
                if (requestType.equals("GET")) {
                    jsonMessage = statusRoute();
                } else {
                    LOGGER.fatal(INVALID_ROUTE_ERROR_MSG + route + " can only accept GET request.");
                    jsonMessage.put("Result", INVALID_ROUTE_ERROR_MSG + route + " can only accept GET request.");
                }
                break;
        }
        return jsonMessage;
    }

    /**
     * Validates the request parameter.
     *
     * @return true or false depending on valid parameter status.
     */
    @Override
    public boolean validateInput(JSONObject requestParams) {
        boolean validate;
        if (requestParams.isEmpty()) {
            validate = false;
        } else {
            validate = requestParams.has(KEY_BASEURI);
            if (validate) {
                String baseURI = requestParams.getString(KEY_BASEURI);
                // Base URI passed must either be default or a valid URL (starts with http/https and ends with / or #)
                validate = baseURI.equals("default") ||
                        (baseURI.startsWith("http://www.") || baseURI.startsWith("https://www.")) && (baseURI.endsWith("/") || baseURI.endsWith("#"));
                // When there is an optional endpoint specified, verify it is correct
                if (requestParams.has(KEY_ENDPOINT)) {
                    String endpoint = requestParams.getString(KEY_ENDPOINT);
                    validate = endpoint.startsWith("http") && endpoint.contains("/blazegraph/namespace") && endpoint.endsWith("/sparql");
                }
            }
        }
        return validate;
    }

    /**
     * Run logic for the "/status" route that indicates the agent's current status.
     *
     * @return A response to the request called as a JSON Object.
     */
    protected JSONObject statusRoute() {
        JSONObject response = new JSONObject();
        LOGGER.info("Detected request to get agent status...");
        if (Ifc2OntoBIMAgent.VALID) {
            response.put("Result", "Agent is ready to receive requests.");
        } else {
            response.put("Result", "Agent could not be initialise!");
        }
        return response;
    }

    /**
     * Run logic for the "/run" route that runs the agent's tasks. Args can be used to set flags following the IFC2RDF options.
     *
     * @return A response to the request called as a JSON Object.
     */
    protected JSONObject runAgent(String[] args) {
        JSONObject response = new JSONObject();
        // Convert the IFC files in the target directory to TTL using IfcOwl Schema
        LOGGER.info("Sending POST request to IfcOwlConverterAgent...");
        try {
            String inputJson = "{\"" + KEY_BASEURI + "\":\"" + args[0] + "\"}";
            AccessClient.sendPostRequest(IFCOWL_API_URl, inputJson);
        } catch (Exception e) {
            LOGGER.fatal(IFCOWL_CONVERSION_ERROR_MSG + e.getMessage());
            throw new JPSRuntimeException(IFCOWL_CONVERSION_ERROR_MSG + e.getMessage());
        }
        LOGGER.info("All IFC files have been successfully converted to IfcOwl instances.");
        OntoBimConverter bimConverter = new OntoBimConverter();
        // Generate a set of ttl files  in target directory
        Set<String> ttlFileList = bimConverter.listTTLFiles(ttlDir);
        // Validate file availability
        if (ttlFileList.size() == 0) {
            LOGGER.info("No TTL file detected! Please place at least 1 IFC file input.");
            response.put("Result", "No TTL file detected! Please place at least 1 IFC file input.");
            return response;
        } else if (ttlFileList.size() > 1 && !endpoint.isEmpty()) {
            LOGGER.info("More than one TTL file detected! Files will not be uploaded to " + endpoint);
            response.put("Result", "More than one TTL file detected! Files will not be uploaded to " + endpoint);
        }

        if (ttlFileList.size() != 0) {
            // Convert each TTL file with IFCOwl instances to ontoBIM instances
            for (String ttlFile : ttlFileList) {
                LOGGER.info("Preparing to convert IFCOwl to OntoBIM schema for TTL file: " + ttlFile);
                bimConverter.convertOntoBIM(ttlFile);
                LOGGER.info(ttlFile + " has been successfully converted!");
                response.accumulate("Result", ttlFile + " has been successfully converted!");

                // Load to an endpoint if specified
                if (!endpoint.isEmpty()) {
                    if (ttlFileList.size() == 1) {
                        AccessClient.uploadTTL(endpoint, ttlFile);
                        LOGGER.info(ttlFile + " has been uploaded to " + endpoint);
                        response.accumulate("Result", ttlFile + " has been uploaded to " + endpoint);
                    }
                }
            }
        }
        LOGGER.info("All ttl files have been generated in OntoBIM. Please check the directory for the files at :");
        response.accumulate("Result", "All ttl files have been generated in OntoBIM. Please check the directory.");
        return response;
    }
}