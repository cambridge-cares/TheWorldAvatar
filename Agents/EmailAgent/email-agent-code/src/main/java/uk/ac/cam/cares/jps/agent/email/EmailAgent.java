package uk.ac.cam.cares.jps.agent.email;

import static uk.ac.cam.cares.jps.agent.email.EmailAgentConfiguration.KEY_WHITE_IPS;
import static uk.ac.cam.cares.jps.agent.email.EmailAgentConfiguration.KEY_WHITE_ONLY;
import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.Arrays;
import java.util.Date;
import java.util.HashSet;
import java.util.Set;
import javax.servlet.ServletException;
import javax.servlet.UnavailableException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.BadRequestException;
import org.apache.log4j.ConsoleAppender;
import org.apache.log4j.Level;
import org.apache.log4j.PatternLayout;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Controller;
import uk.ac.cam.cares.jps.base.agent.JPSAgent;

/**
 * TODO - Document
 *
 * @author Michael Hillman
 */
@Controller
@WebServlet(urlPatterns = {"/email", "/test"})
public class EmailAgent extends JPSAgent {

    /**
     * Error logging.
     */
    private static final Logger LOGGER = LoggerFactory.getLogger(EmailAgent.class);

    /**
     * Is the EmailAgent in a valid state.
     */
    private boolean validState = true;

    static {
        org.apache.log4j.Logger root = org.apache.log4j.Logger.getRootLogger();
        root.setLevel(Level.DEBUG);
        root.addAppender(new ConsoleAppender(
                new PatternLayout("%d{HH:mm:ss.SSS} (%c{1})[%t] %p - %m%n")
        ));

        System.out.println("LOG4J has been initialised.");
    }

    public EmailAgent() {
        System.out.println("An EmailAgent instance has been initialised?");
    }
    
    /**
     * Perform required setup.
     *
     * @throws ServletException if in invalid state
     */
    @Override
    public void init() throws ServletException {
        super.init();

        // Read the properties file
        try {
            EmailAgentConfiguration.readProperties();
        } catch (IOException ioException) {
            LOGGER.error("Could not read properties!", ioException);
            validState = false;

            throw new UnavailableException("EmailAgent is not in valid state, could not read properties.");
        }
    }

    @Override
    public JSONObject processRequestParameters(JSONObject requestParams) {
        System.out.println("WRONG METHOD");
        JSONObject response = new JSONObject();
        response.put("status", "500");
        response.put("description", "Has the wrong method been called?");
        return response;
    }

    /**
     * Processes HTTP requests.
     *
     * @param requestParams Request parameters in a JSONObject
     * @param request HTTP Servlet Request
     *
     * @return result JSON
     */
    @Override
    public JSONObject processRequestParameters(JSONObject requestParams, HttpServletRequest request) {

        String path = request.getServletPath();
        if (path.contains("test")) {
            JSONObject response = new JSONObject();
            response.put("status", "200");
            response.put("description", "This was a test?");
            return response;
        }

        SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss.SSSS");
        String datetime = dateFormat.format(new Date());
        LOGGER.info("Request received at: " + datetime);

        // Check if this is an email request or a ping to determine availability
        if (!requestParams.isNull("ping")) {
            return processPingRequest(request);
        }

        // Check validity
        boolean validInput = validateInput(requestParams);
        boolean validSource = validateRequest(request);

        if (validInput && validSource) {
            return processEmailRequest(requestParams, request);
        } else {
            // Should already haev triggered a BadRequestException, but just in case
            LOGGER.warn("Bad request detected, throwing BadRequestException.");
            throw new BadRequestException("Invalid inputs, or untrusted source, cannot process request.");
        }
    }

    /**
     * Processes the request to check servlet availability.
     *
     * @param request HTTP Servlet Request
     *
     * @return result JSON
     */
    private JSONObject processPingRequest(HttpServletRequest request) {
        LOGGER.info("Determined as availability request, checking...");

        // Is it from a valid source
        boolean validSource = validateRequest(request);

        // Return status (200 is good)
        int status = 200;
        String description = "Ready and available for requests.";

        if (!validState) {
            // Server is in invalid state
            status = 500;
            description = "Internal server error, cannot process requests.";
            LOGGER.warn("Servlet not in valid state, returning status 500.");

        } else if (!validSource) {
            // Non-permitted source of request.
            status = 403;
            description = "Unauthorised source (not on white-list), cannot process requests.";
            LOGGER.warn("Unauthorised request source, returning status 403.");

        } else {
            LOGGER.info("Approved request, returning status 200.");
        }

        // Send response
        JSONObject response = new JSONObject();
        response.put("status", Integer.toString(status));
        response.put("description", description);
        return response;
    }

    /**
     * Processes the request to send an email notification.
     *
     * @param requestParams Request parameters in a JSONObject
     * @param request HTTP Servlet Request
     *
     * @return result JSON
     */
    private JSONObject processEmailRequest(JSONObject requestParams, HttpServletRequest request) {
        LOGGER.info("Determined as email request, submitting...");

        // Attempt to send email
        return EmailHandler.submitEmail(
                requestParams.get("subject").toString(),
                requestParams.get("body").toString()
        );
    }

    /**
     * Validate the input JSON contents.
     *
     * @param requestParams Request parameters as a JSONObject
     * @return validity status
     *
     * @throws BadRequestException
     */
    @Override
    public boolean validateInput(JSONObject requestParams) throws BadRequestException {
        if (!validState) {
            LOGGER.error("EmailAgent is in invalid state, cannot validate inputs.");
            return false;
        }

        // Check that there's a subject
        if (requestParams.isNull("subject")) {
            throw new BadRequestException("Request does not have required 'subject' field.");
        }

        // Check that there's a body
        if (requestParams.isNull("body")) {
            throw new BadRequestException("Request does not have required 'body' field.");
        }

        return true;
    }

    /**
     * Validates that the request has come from a trusted source.
     *
     * @param request HTTP request
     *
     * @return validity status
     */
    private boolean validateRequest(HttpServletRequest request) {
        if (!validState) {
            LOGGER.error("EmailAgent is in invalid state, cannot validate request.");
            return false;
        }

        // Determine if the white list is enabled
        String whitelistProperty = EmailAgentConfiguration.getProperty(KEY_WHITE_ONLY);
        boolean whitelistOn = Boolean.parseBoolean(whitelistProperty);

        if (whitelistOn) {
            // Get the allowed source IPs
            String[] allowedIPs = EmailAgentConfiguration.getPropertyAsArray(KEY_WHITE_IPS, ",");
            Set<String> allowedList = new HashSet<>(Arrays.asList(allowedIPs));

            // Get the source IP
            String sourceIP = request.getHeader("X-FORWARDED-FOR");
            if (sourceIP == null) {
                sourceIP = request.getRemoteAddr();
            }

            // Allow local calls
            if (sourceIP.contains("localhost")) return true;
            if (sourceIP.contains("127.0.0.1")) return true;

            LOGGER.info("Request IP(s) reported as: " + sourceIP);

            // Source may be multiple IPs if client was using a proxy
            if (sourceIP.contains(",")) {
                String[] sourceIPs = sourceIP.split(",");

                // How many of those IPs are in the whitelist?
                int matches = 0;
                for (String ip : sourceIPs) {
                    if (allowedList.contains(ip.trim())) matches++;
                }

                // If none are, return invalid
                if (matches == 0) return false;

            } else {
                if (!allowedList.contains(sourceIP.trim())) return false;
            }
        }

        return true;
    }

}
// End of class.
