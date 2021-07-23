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
            System.out.println("ERROR: Could not read properties!");
            ioException.printStackTrace(System.out);
            validState = false;

            throw new UnavailableException("EmailAgent is not in valid state, could not read properties.");
        }
    }

    @Override
    public JSONObject processRequestParameters(JSONObject requestParams) {
        System.out.println("processRequestParameters(1)");
        System.out.println(requestParams.toString());

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
        System.out.println("processRequestParameters(2)");
        System.out.println(requestParams.toString());

        SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss.SSSS");
        String datetime = dateFormat.format(new Date());
        System.out.println("INFO: Request received at: " + datetime);

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
            System.out.println("WARN: Bad request detected, throwing BadRequestException.");
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
        System.out.println("INFO: Determined as availability request, checking...");

        // Is it from a valid source
        boolean validSource = validateRequest(request);

        // Return status (200 is good)
        int status = 200;
        String description = "Ready and available for requests.";

        if (!validState) {
            // Server is in invalid state
            status = 500;
            description = "Internal server error, cannot process requests.";
            System.out.println("WARN: Servlet not in valid state, returning status 500.");

        } else if (!validSource) {
            // Non-permitted source of request.
            status = 403;
            description = "Unauthorised source (not on white-list), cannot process requests.";
            System.out.println("WARN: Unauthorised request source, returning status 403.");

        } else {
            System.out.println("INFO: Approved request, returning status 200.");
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
        System.out.println("INFO: Determined as email request, submitting...");

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
            System.out.println("ERROR: EmailAgent is in invalid state, cannot validate inputs.");
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
            System.out.println("ERROR: EmailAgent is in invalid state, cannot validate request.");
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
            
            // For testing, good to know even in production
            System.out.println("INFO: Request IP(s) reported as: " + sourceIP);
             
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
            
            System.out.println("INFO: Matching IP found, approving request.");
        }

        return true;
    }

}
// End of class.
