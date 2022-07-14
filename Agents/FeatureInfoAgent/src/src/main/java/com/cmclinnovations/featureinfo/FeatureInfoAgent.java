package com.cmclinnovations.featureinfo;


import java.text.SimpleDateFormat;
import java.util.Date;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.BadRequestException;
import javax.ws.rs.InternalServerErrorException;
import javax.ws.rs.ServerErrorException;
import javax.ws.rs.core.Response;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONObject;
import org.springframework.stereotype.Controller;

import uk.ac.cam.cares.jps.base.agent.JPSAgent;

/**
 * This agent expects a HTTP request containing a JSON string with the "iri" and "endpoint" for an individual
 * feature. This information is then used (in conjunction with hardcoded SPARQL queries) to gather data on
 * that feature and return it as a JSON object.
 * 
 * Note: this version of the agent is a VERY rough implementation and should not be used outside of very specific
 * use cases. It currently relies on hardcoded SPARQL queries, and will not work out-of-the box for any old data sets.
 * Future improvements will make this more generic, at which point the agent could be used more widely.
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
     * Is the FeatureInfoAgent in a valid state.
     */
    private static boolean VALID = true;

    /**
     * Perform required setup.
     *
     * @throws ServletException if in invalid state
     */
    @Override
    public void init() throws ServletException {
        super.init();

        // Test the logging
        LOGGER.debug("This is a test DEBUG message");
        LOGGER.info("This is a test INFO message");
        LOGGER.warn("This is a test WARN message");
        LOGGER.error("This is a test ERROR message");
        LOGGER.fatal("This is a test FATAL message");
        System.out.println("This is a test SYSTEM.OUT message");
    }

    /**
     * Processes HTTP requests with originating details.
     *
     * @param requestParams Request parameters in a JSONObject.
     * @param request HTTP Servlet Request.
     *
     * @return response in JSON format.
     */
    @Override
    public JSONObject processRequestParameters(JSONObject requestParams) {
        return new JSONObject();
    }

    /**
     * Processes HTTP requests.
     *
     * @param requestParams Request parameters as a JSONObject.
     *
     * @return response in JSON format.
     */
    @Override
    public JSONObject processRequestParameters(JSONObject requestParams, HttpServletRequest request) {
        SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss.SSSS");
        String datetime = dateFormat.format(new Date());
        LOGGER.info("Request received at: " + datetime);
        LOGGER.info("Request contents are: " + requestParams);

        // Check if in valid state
        if(!VALID) {
            throw new InternalServerErrorException("FeatureInfoAgent could not start in a valid state.");
        }

        // Get the URL path
        String url = request.getRequestURI();
        url = url.substring(url.lastIndexOf("/"), url.length());
        if (url.contains("?")) url = url.split("?")[0];

        // Run logic based on request path
        switch (url) {
            case "/get":
            case "get":
                LOGGER.info("Detected request to get meta and time data...");

                // Check if request is valid
                try {
                    validateInput(requestParams);
                } catch (BadRequestException brExcep) {
                    throw brExcep;
                }

                try {
                    // Get and return all available information
                    Manager grabber = new Manager(
                        requestParams.getString("iri"),
                        requestParams.getString("endpoint"),
                        this.getServletContext()
                    );
                    grabber.readProperties();

                    return grabber.grabAll();
                } catch(Exception excep) {
                    LOGGER.error(excep);
                    throw new ServerErrorException(excep.getMessage(), Response.Status.INTERNAL_SERVER_ERROR, excep);
                }

            case "/status":
            case "status":
                LOGGER.info("Detected request to get agent status...");

                // Send response
                JSONObject response = new JSONObject();
                response.put("status", Integer.toString(200));
                response.put("description", "Ready for requests.");
                return response;

            default:
                return new JSONObject();
        }
    }

    /**
     * Checks the incoming JSON request for validity.
     * 
     * @param requestParams JSON request parameters.
     * 
     * @return request validity.
     * 
     * @throws BadRequestException if request is malformed.
     */
    @Override
    public boolean validateInput(JSONObject requestParams) throws BadRequestException {
        // Check that there's an iri
        if (requestParams.isNull("iri")) {
            LOGGER.error("Could not find the 'iri' field.");
            throw new BadRequestException("Request does not have required 'iri' field.");
        }

        // Check that there's an endpoint
        if (requestParams.isNull("endpoint")) {
            LOGGER.error("Could not find the 'endpoint' field.");
            throw new BadRequestException("Request does not have required 'endpoint' field.");
        }

        return true;
    }

}
// End of class.
