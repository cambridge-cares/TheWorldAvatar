package uk.ac.cam.cares.jps.agent.ifc2ontobim;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONObject;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.utils.NamespaceMapper;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.utils.StringUtils;
import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.TimeUnit;

/**
 * This class acts as the entry point of the compiled war, and coordinates the two components (IfcOwlConverterAgent and
 * OntoBimAgent) to produce a TTL file with ontoBIM instances converted from an IFC model input.
 *
 * @author qhouyee
 */
@WebServlet(urlPatterns = {"/convert", "/convert-no-geom", "/status"})
public class Ifc2OntoBIMAgent extends JPSAgent {
    private static final Logger LOGGER = LogManager.getLogger(Ifc2OntoBIMAgent.class);
    // Agent starts off in valid state, and will be invalid when running into exceptions
    private static boolean VALID = true;
    private static final String INVALID_ROUTE_ERROR_MSG = "Invalid request type! Route ";
    private static final String IFCOWL_CONVERSION_ERROR_MSG = "Failed to convert to IfcOwl schema. ";
    private static final String KEY_BASEURI = "uri";
    private static final String KEY_IS_IFCOWL = "isIfcOwl";
    private static final String ttlDir = Paths.get(System.getProperty("user.dir"), "data").toString();

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
        long startTime = System.nanoTime(); // Start timing agent runtime
        // Run logic based on request path
        switch (route) {
            case "convert":
                if (validateInput(requestParams)) {
                    // Process request parameters
                    String baseURI = requestParams.getString(KEY_BASEURI);
                    // Return false if the request does not have this parameter. Otherwise, return the parameter's value
                    Boolean isIfcOwl = requestParams.has(KEY_IS_IFCOWL) && requestParams.getBoolean(KEY_IS_IFCOWL);
                    String[] args = (!baseURI.equals("default")) ? new String[]{baseURI} : new String[]{"default"};
                    jsonMessage = this.runAgent(args, true, isIfcOwl);
                } else if (!requestType.equals("POST")) {
                    LOGGER.fatal(INVALID_ROUTE_ERROR_MSG + route + " can only accept POST request.");
                    jsonMessage.put("Result", INVALID_ROUTE_ERROR_MSG + route + " can only accept POST request.");
                } else {
                    LOGGER.fatal("Request parameters are not defined correctly.");
                    jsonMessage.put("Result", "Request parameters are not defined correctly.");
                }
                break;
            case "convert-no-geom":
                if (validateInput(requestParams)) {
                    // Process request parameters
                    String baseURI = requestParams.getString(KEY_BASEURI);
                    // Return false if the request does not have this parameter. Otherwise, return the parameter's value
                    Boolean isIfcOwl = requestParams.has(KEY_IS_IFCOWL) && requestParams.getBoolean(KEY_IS_IFCOWL);
                    String[] args = (!baseURI.equals("default")) ? new String[]{baseURI} : new String[]{"default"};
                    jsonMessage = this.runAgent(args, false, isIfcOwl);
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
        // Total agent run time in nanoseconds
        long duration = System.nanoTime() - startTime;
        // If it can be converted to second, return run time in seconds
        if (TimeUnit.NANOSECONDS.toSeconds(duration)>0){
            jsonMessage.accumulate("Runtime", TimeUnit.NANOSECONDS.toSeconds(duration) + "s");
        } else {
            // Else return as nanoseconds
            jsonMessage.accumulate("Runtime", duration + "ns");
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
    protected JSONObject runAgent(String[] args, Boolean isGeomRequired, Boolean isIfcOwl) {
        JSONObject response = new JSONObject();
        Map<String, String> config = AccessClient.retrieveClientProperties();
        // Boolean indicates if there is already a preexisting TTL file. If there is one, do not generate another TTL from the IfcOwlConverter agent.
        if (!isIfcOwl) {
            // Convert the IFC files in the target directory to TTL using IfcOwl Schema
            LOGGER.info("Sending POST request to IfcOwlConverterAgent...");
            try {
                NamespaceMapper.setBaseNameSpace(args[0]);
                String inputJson = "{\"" + KEY_BASEURI + "\":\"" + args[0] + "\"}";
                AccessClient.sendPostRequest(config.get(AccessClient.IFC_OWL_CONVERTER_API), inputJson);
            } catch (Exception e) {
                LOGGER.fatal(IFCOWL_CONVERSION_ERROR_MSG + e.getMessage());
                throw new JPSRuntimeException(IFCOWL_CONVERSION_ERROR_MSG + e.getMessage());
            }
            LOGGER.info("All IFC files have been successfully instantiated as IfcOwl instances.");
        }
        // Generate a set of ttl files  in target directory
        Set<String> ttlFileList = AccessClient.listTTLFiles(ttlDir);
        // Validate file availability
        if (ttlFileList.size() == 0) {
            LOGGER.info("No TTL file detected! Please place at least 1 IFC file input.");
            response.put("Result", "No TTL file detected! Please place at least 1 IFC file input.");
            return response;
        } else if (ttlFileList.size() > 1) {
            LOGGER.info("More than one TTL file detected! Files cannot be converted or uploaded.");
            response.put("Result", "More than one TTL file detected! Files cannot be converted or uploaded.");
            return response;
        }
        // Only one TTL file containing the IFCOwl instances should exist
        // Retrieve the endpoint and start instantiation using the OntoBIM schema
        String endpoint = config.get(AccessClient.UPDATE_ENDPOINT);
        OntoBimConverter bimConverter = new OntoBimConverter();
        for (String ttlFile : ttlFileList) {
            LOGGER.info("Preparing to convert IFCOwl to OntoBIM schema for TTL file: " + ttlFile);
            List<Path> tempFilePaths = bimConverter.convertOntoBIM(ttlFile, isGeomRequired);
            LOGGER.info("Uploading statements to " + endpoint);
            AccessClient.uploadStatements(endpoint, tempFilePaths);
            AccessClient.cleanUp(ttlFile, tempFilePaths);
            String ifc = StringUtils.getStringAfterLastCharacterOccurrence(ttlFile, StringUtils.SLASH);
            ifc = StringUtils.getStringBeforeLastCharacterOccurrence(ifc, ".");
            LOGGER.info(ifc + ".ifc has been successfully instantiated and uploaded to " + endpoint);
            response.accumulate("Result", ifc + ".ifc has been successfully instantiated and uploaded to " + endpoint);
        }
        return response;
    }
}