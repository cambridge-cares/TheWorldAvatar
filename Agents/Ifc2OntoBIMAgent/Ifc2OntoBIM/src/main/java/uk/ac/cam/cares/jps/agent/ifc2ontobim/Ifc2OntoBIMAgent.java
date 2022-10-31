package uk.ac.cam.cares.jps.agent.ifc2ontobim;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONObject;
import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.BadRequestException;
import java.util.Set;

/**
 * This class acts as the entry point of the compiled jar, and coordinates the two components (IfcOwlConverter and OntoBimAgent)
 * to produce a TTL file with ontoBIM instances converted from an IFC model input.
 *
 * @author qhouyee
 */
@WebServlet(urlPatterns = {"/retrieve"})
public class Ifc2OntoBIMAgent extends JPSAgent {
    private static final Logger LOGGER = LogManager.getLogger(Ifc2OntoBIMAgent.class);
    private static final String IFCOWL_CONVERSION_ERROR_MSG = "Failed to convert to IfcOwl schema. Read error for more information: ";
    private static final String KEY_BASEURI = "uri";

    @Override
    public JSONObject processRequestParameters(JSONObject requestParams, HttpServletRequest request) {
        return processRequestParameters(requestParams);
    }

    @Override
    public JSONObject processRequestParameters(JSONObject requestParams) {
        JSONObject jsonMessage = new JSONObject();
        if (validateInput(requestParams)) {
            LOGGER.info("Passing request to Ifc2OntoBIM Agent..");
            String baseURI = requestParams.getString(KEY_BASEURI);
            String[] args = (!baseURI.equals("default")) ? new String[]{"--baseURI", baseURI} : new String []{};
            jsonMessage = this.runAgent(args);
            LOGGER.info("All ttl files have been generated in OntoBIM. Please check the directory for the files at :");
            jsonMessage.accumulate("Result", "All ttl files have been generated in OntoBIM. Please check the directory.");
        } else {
            LOGGER.fatal("Request parameters are not defined correctly.");
            jsonMessage.put("Result", "Request parameters are not defined correctly.");
        }
        return jsonMessage;
    }

    @Override
    public boolean validateInput(JSONObject requestParams) throws BadRequestException {
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

    // Args can be used to set flags following the IFC2RDF options
    public JSONObject runAgent(String[] args) {
        JSONObject jsonMessage = new JSONObject();
        // Convert the IFC files in the target directory to TTL using IfcOwl Schema
        IfcOwlConverter ifcConverter= new IfcOwlConverter(args);
        LOGGER.info("IfcOwl converter object have been initialised");
        try {
            ifcConverter.parse2TTL();
        } catch (Exception e) {
            LOGGER.fatal(IFCOWL_CONVERSION_ERROR_MSG + e);
            throw new JPSRuntimeException(IFCOWL_CONVERSION_ERROR_MSG + e);
        }
        LOGGER.info("All IFC files have been successfully converted to IfcOwl instances.");

        // Generate a set of ttl files  in target directory
        Set<String> ttlFileList = ifcConverter.listTTLFiles();

        OntoBimConverter bimConverter;
        // Convert each TTL file with IFCOwl instances to ontoBIM instances
        for (String ttlFile: ttlFileList){
            LOGGER.info("Preparing to convert IFCOwl to OntoBIM schema for TTL file: " + ttlFile);
            bimConverter = new OntoBimConverter();
            bimConverter.convertOntoBIM(ttlFile);
            LOGGER.info(ttlFile + " has been successfully converted!");
            jsonMessage.accumulate("Result", ttlFile + " has been successfully converted!");
        }
        return jsonMessage;
    }
}
