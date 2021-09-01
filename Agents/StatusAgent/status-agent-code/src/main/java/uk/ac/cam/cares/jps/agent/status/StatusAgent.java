package uk.ac.cam.cares.jps.agent.status;

import java.text.SimpleDateFormat;
import java.util.Date;
import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.BadRequestException;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONObject;
import org.springframework.stereotype.Controller;
import uk.ac.cam.cares.jps.base.agent.JPSAgent;

/**
 * The StatusAgent provides a number of availability tests to check that KG endpoints (and other
 * agents) are available at their expected URLs, and contain the expected data. These tests are run
 * on a regular schedule, and the results viewed via a provided webpage. Manual execution of the
 * tests can also be triggered via the webpage and HTTP requests.
 *
 * @author Michael Hillman
 */
@Controller
@WebServlet(urlPatterns = {StatusAgent.DASHBOARD_URL, StatusAgent.LIST_URL, StatusAgent.SUBMISSION_URL})
public class StatusAgent extends JPSAgent {

    /**
     * URL path for status dashboard (webpage).
     */
    public static final String DASHBOARD_URL = "/dashboard";

    /**
     * URL path for list of available tests.
     */
    public static final String LIST_URL = "/tests";

    /**
     * URL path to trigger/submit a test.
     */
    public static final String SUBMISSION_URL = "/submit";

    /**
     * Logger for error output.
     */
    private static final Logger LOGGER = LogManager.getLogger(StatusAgent.class);

    /**
     * Is the agent in a valid state.
     */
    private boolean validState = true;
    
    /**
     * TestHandler instance.
     */
    public static final TestHandler HANDLER = new TestHandler();

    /**
     * Perform required setup.
     *
     * @throws ServletException if in invalid state
     */
    @Override
    public void init() throws ServletException {
        super.init();

        LOGGER.debug("This is a test DEBUG message");
        LOGGER.info("This is a test INFO message");
        LOGGER.warn("This is a test WARN message");
        LOGGER.error("This is a test ERROR message");
        LOGGER.fatal("This is a test FATAL message");
        System.out.println("This is a test SYSTEM.OUT message");
    }

    @Override
    protected String getResponseBody(HttpServletRequest request) {
        // TODO: Check if request should have HTML or JSON response
        
        return "";
    }

    @Override
    protected String getResponseBody(HttpServletRequest request, JSONObject requestParams) {
        return "";
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
        SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss.SSSS");
        String datetime = dateFormat.format(new Date());
        LOGGER.info("Request received at: " + datetime);

        // TODO: This should only respond to requests that require a JSON response.
        return null;
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
//        if (!validState) {
//            LOGGER.error("EmailAgent is in invalid state, cannot validate inputs.");
//            return false;
//        }
//
//        // Check that there's a subject
//        if (requestParams.isNull("subject")) {
//            throw new BadRequestException("Request does not have required 'subject' field.");
//        }
//
//        // Check that there's a body
//        if (requestParams.isNull("body")) {
//            throw new BadRequestException("Request does not have required 'body' field.");
//        }

        return true;
    }
}
// End of class.
