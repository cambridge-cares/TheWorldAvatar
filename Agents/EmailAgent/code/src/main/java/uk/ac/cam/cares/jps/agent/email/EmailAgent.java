package uk.ac.cam.cares.jps.agent.email;

import static uk.ac.cam.cares.jps.agent.email.Config.KEY_WHITE_IPS;
import static uk.ac.cam.cares.jps.agent.email.Config.KEY_WHITE_ONLY;

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
import javax.servlet.http.HttpServletResponse;
import javax.ws.rs.core.Response;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONObject;
import org.springframework.stereotype.Controller;

import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;

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
 * @author Michael Hillman (mdhillman<@>mdhillman@cmclinnovations.com)
 */
@Controller
@WebServlet(urlPatterns = {"/get", "/send", "/ping", "/status"})
public class EmailAgent extends JPSAgent {

    /**
     * Logger for error output.
     */
    private static final Logger LOGGER = LogManager.getLogger(EmailAgent.class);

    /**
     * Config object.
     */
    private Config config;

    /**
     * Handles transmission of emails.
     */
    private Handler handler;

    /**
     * Is the EmailAgent in a valid state.
     */
    private boolean validState = true;

    /**
     * Constructor.
     */
    public EmailAgent() {
        // Empty
    }

    /**
     * Constructor, with instance overrides
     * 
     * @param config configuration object.
     */
    public EmailAgent(Config config, Handler handler) {
        this.config = config;
        this.handler = handler;
    }

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

        try {
            if(config == null) {
                config = new Config();
                config.readProperties();
                LOGGER.debug("EmailAgent has been initialised.");
            }

            if(handler == null) {
                handler = new Handler();
                handler.setConfig(config);
            }

        } catch (IOException ioException) {
            LOGGER.error("Could not read the required properties file!");
            validState = false;

            // Cannot throw UnavailableException here unless we're using Java EE
            throw new IllegalStateException("EmailAgent is not in valid state, could not read properties.", ioException);
        }
    }

    /**
     * Processes incoming HTTP GET request.
     * 
     * Note that as the functionality of the agent is to request a job is performed, this should really be done
     * by using POST requests; however the JPS agent framework isn't configured well enough to handle this.
     *
     * @param request HTTP servlet request.
     * @param response HTTP serlet response.
     */
    @Override
    public void doGet(HttpServletRequest request, HttpServletResponse response) {
        response.setContentType("text/json");

        // Log presence of request
        SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss.SSSS");
        String datetime = dateFormat.format(new Date());
        LOGGER.info("Request received at: {}", datetime);

        // Get the requested route
        String url = request.getRequestURI();
        url = url.substring(url.lastIndexOf("/"), url.length());
        if (url.contains("?")) url = url.split(Pattern.quote("?"))[0];

        try {
            // Run logic depending on submitted route
            switch(url) {
                // Run main logic
                case "/get":
                case "/send":
                    this.sendRoute(request, response);
                break;

                // Simple status check
                case "/ping": 
                case "/status":
                    this.statusRoute(request, response);
                break;

                // Anything else
                default:
                    LOGGER.info("Detected an unknown request route.");
                    response.setStatus(Response.Status.NOT_IMPLEMENTED.getStatusCode());
                    response.getWriter().write("{\"description\":\"Unknown route, only '/get' and '/status' are permitted.\"}");
                break;
            }

            response.getWriter().flush();
            LOGGER.info("Call finished, response object's writer has been flushed.");
       
        } catch(IOException ioException) {
            LOGGER.error("Internal server error processing GET request!", ioException);
        }
    }


    /**
     * Run logic for the "/status" route.
     * 
     * @param request original HTTP request
     * @param response HTTP response
     * 
     * @throws IOException
     */
    protected void statusRoute(HttpServletRequest request, HttpServletResponse response) throws IOException {
        LOGGER.info("Detected request to get agent status, returning state.");

        if(validState) {
            // Is it from a valid source
            boolean validSource = validateRequest(request);

            if(!validSource) {
                response.setStatus(Response.Status.FORBIDDEN.getStatusCode());
                response.getWriter().write("{\"description\":\"Unauthorised source IP.\"}");
                LOGGER.info("State was 'Unauthorised source.'");

            } else {
                response.setStatus(Response.Status.OK.getStatusCode());
                response.getWriter().write("{\"description\":\"Ready to serve.\"}");
                LOGGER.info("State was 'Ready to serve.'");
            }
           
        } else {
            response.setStatus(Response.Status.INTERNAL_SERVER_ERROR.getStatusCode());
            response.getWriter().write("{\"description\":\"Could not initialise a valid agent instance!\"}");
        }
    }

    /**
     * Run logic for the "/main" route.
     * 
     * @param request original HTTP request
     * @param response HTTP response
     * 
     * @throws IOException
     */
    protected void sendRoute(HttpServletRequest request, HttpServletResponse response) throws IOException {
        // Parse request as JSON
        JSONObject requestParams = new JSONObject(request.getParameter("query"));

        // Check validity of request parameters
        if(!check(requestParams, response)) return;

        // Check validity of source IP
        if(!validateRequest(request)) {
            LOGGER.info("Unauthorised source IP, will not send email.");
            response.setStatus(Response.Status.FORBIDDEN.getStatusCode());
            response.getWriter().write("{\"description\":\"Unauthorised source IP.\"}");

        } else {
            // Attempt to send email
            LOGGER.info("Submitting email request to remote SMTP server.");
            
            handler.submitEmail(
                requestParams.getString("subject"),
                requestParams.getString("body"),
                response
            );

            response.setStatus(Response.Status.OK.getStatusCode());
            response.getWriter().write("{\"description\":\"Request forwarded to remote SMTP server for delivery.\"}");
        }
    }

    /**
     * Check that the request is valid and the agent is in a valid state.
     * 
     * @param requestParams parameters of request
     * @param response HTTP response
     */
    private final boolean check(JSONObject requestParams, HttpServletResponse response) throws IOException {
        // Check if in valid state
        if(!validState)  {
            LOGGER.error("FeatureInfoAgent could not start in a valid state.");

            response.setStatus(Response.Status.INTERNAL_SERVER_ERROR.getStatusCode());
            response.getWriter().write("{\"description\":\"Could not initialise agent instance!\"}");
            return false;
        }

        // Check if valid request parameters
        if(requestParams.has("subject") && requestParams.has("body")) {
            // Valid for a get/send request
            return true;
        } else {
            response.setStatus(Response.Status.BAD_REQUEST.getStatusCode());
            response.getWriter().write("{\"description\":\"Must specify 'subject' and 'body' parameters!\"}");
            return false;
        }
    }

    /**
     * Validates that the request has come from a trusted source.
     *
     * @param request HTTP request
     *
     * @return validity status
     */
    private boolean validateRequest(HttpServletRequest request) {
        // Determine if the white list is enabled (default if not set should be false)
        boolean whitelistOn = Boolean.parseBoolean(config.getProperty(KEY_WHITE_ONLY));

        if (whitelistOn) {
            // Get the allowed source IPs
            String[] allowedIPs = config.getPropertyAsArray(KEY_WHITE_IPS, ",");
            if (allowedIPs == null) return true;

            Set<String> allowedList = new HashSet<>(Arrays.asList(allowedIPs));

            // Get the source IP
            String sourceIP = request.getHeader("X-FORWARDED-FOR");
            if (sourceIP == null) {
                sourceIP = request.getRemoteAddr();
            }

            // For testing, good to know even in production
            LOGGER.info("Request IP(s) reported as: {}", sourceIP);

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
            LOGGER.error("Value '{}' is not a valid IP address.", ipString);
            return false;
        }
    }
}
// End of class.
