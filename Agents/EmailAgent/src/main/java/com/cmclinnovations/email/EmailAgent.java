package com.cmclinnovations.email;

import static com.cmclinnovations.email.EmailAgentConfiguration.KEY_WHITE_IPS;
import static com.cmclinnovations.email.EmailAgentConfiguration.KEY_WHITE_ONLY;
import java.io.IOException;
import java.util.Arrays;
import java.util.Date;
import java.util.HashSet;
import java.util.Set;
import javax.servlet.ServletException;
import javax.servlet.UnavailableException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.BadRequestException;
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
@WebServlet(urlPatterns = {"/email"})
public class EmailAgent extends JPSAgent {

    /**
     * Error logging.
     */
    private static final Logger LOGGER = LoggerFactory.getLogger(EmailAgent.class);

    /**
     * Is the EmailAgent in a valid state.
     */
    private boolean validState = true;

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
        LOGGER.info("Request received at: " + new Date().toString());

        // Check validity
        boolean validInput = validateInput(requestParams);
        boolean validSource = validateRequest(request);

        if (validInput && validSource) {
            // Attempt to send email
            return EmailHandler.submitEmail(
                    requestParams.get("subject").toString(),
                    requestParams.get("body").toString()
            );
        } else {
            // Should already haev triggered a BadRequestException, but just in case
            throw new BadRequestException("Invalid inputs, or untrusted source, cannot process request.");
        }
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
        Object subject = requestParams.get("subject");
        if (subject == null) {
            throw new BadRequestException("Request does not have required 'subject' field.");
        }

        // Check that there's a body
        Object body = requestParams.get("body");
        if (body == null) {
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
    public boolean validateRequest(HttpServletRequest request) {
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
            Set<String> allowedList = new HashSet<String>(Arrays.asList(allowedIPs));

            // Get the source IP
            String sourceIP = request.getHeader("X-FORWARDED-FOR");
            if (sourceIP == null) {
                sourceIP = request.getRemoteAddr();
            }

            // Allow local calls
            if (sourceIP.contains("localhost")) return true;

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
