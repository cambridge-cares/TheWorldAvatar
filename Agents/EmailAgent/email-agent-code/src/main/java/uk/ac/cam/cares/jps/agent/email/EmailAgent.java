package uk.ac.cam.cares.jps.agent.email;

import static uk.ac.cam.cares.jps.agent.email.EmailAgentConfiguration.KEY_WHITE_IPS;
import static uk.ac.cam.cares.jps.agent.email.EmailAgentConfiguration.KEY_WHITE_ONLY;
import java.io.IOException;
import java.net.InetAddress;
import java.net.UnknownHostException;
import java.text.SimpleDateFormat;
import java.util.Arrays;
import java.util.Date;
import java.util.HashSet;
import java.util.Set;
import java.util.regex.Pattern;
import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.BadRequestException;
import org.json.JSONObject;
import org.springframework.stereotype.Controller;
import uk.ac.cam.cares.jps.base.agent.JPSAgent;

/**
 * This EmailAgent uses the JPS Asynchronous Watcher framework and listens for incoming HTTP
 * requests. Once a valid HTTP request has been received (and the originating source is approved),
 * then the contents are forward onto an SMTP server (of the developer's choosing) to be dispatched
 * as an email.
 *
 * A class (EmailSender) has been provided within the JPS Base Library to facilitate this. If the
 * EmailAgent cannot be reached (or the request is not approved), then the contents are written to a
 * local log file instead.
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
            validState = false;

            // Cannot throw UnavailableException here unless we're using Java EE
            throw new IllegalStateException("EmailAgent is not in valid state, could not read properties.", ioException);
        }
    }

    @Override
    public JSONObject processRequestParameters(JSONObject requestParams) {
        return new JSONObject();
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

        // Determine if the white list is enabled (default if not set should be false)
        boolean whitelistOn = Boolean.parseBoolean(EmailAgentConfiguration.getProperty(KEY_WHITE_ONLY));

        if (whitelistOn) {
            // Get the allowed source IPs
            String[] allowedIPs = EmailAgentConfiguration.getPropertyAsArray(KEY_WHITE_IPS, ",");
            if (allowedIPs == null) return true;

            Set<String> allowedList = new HashSet<>(Arrays.asList(allowedIPs));

            // Get the source IP
            String sourceIP = request.getHeader("X-FORWARDED-FOR");
            if (sourceIP == null) {
                sourceIP = request.getRemoteAddr();
            }

            // For testing, good to know even in production
            System.out.println("INFO: Request IP(s) reported as: " + sourceIP);

            // Source may be multiple IPs if client was using a proxy
            String[] sourceIPs = sourceIP.split(",");
            for (String ip : sourceIPs) {
                // Allow local requests.
                if (isLocalIP(ip.trim())) return true;

                // Allow if it matches at least one whitelisted ip
                if (allowedList.contains(ip.trim())) return true;
            }

            return false;
        }
        return true;
    }

    /**
     * Returns true if the input IP should be considered a local IP.
     *
     * @param ipString IP Address
     */
    private boolean isLocalIP(String ipString) {
        try {
            InetAddress address = InetAddress.getByName(ipString);

            return address.isLinkLocalAddress()
                    || address.isLoopbackAddress()
                    || address.isSiteLocalAddress();

        } catch (UnknownHostException exception) {
            System.out.println("ERROR: Value '" + ipString + "' is not a valid IP address.");
            return false;
        }
    }
}
// End of class.
