package uk.ac.cam.cares.jps.agent.fumehood;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONObject;
import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.regex.Pattern;

@WebServlet(urlPatterns = {"/retrieve"})
public class FHSashAndOccupancyAgent extends JPSAgent {
    private static final Logger LOGGER = LogManager.getLogger(FHSashAndOccupancyAgent.class);


    public static final String PARAMETERS_VALIDATION_ERROR_MSG = "Unable to validate request sent to the agent.";
    public static final String EMPTY_PARAMETER_ERROR_MSG = "Empty Request.";
    public static final String AGENT_Construction_ERROR_MSG = "The Agent could not be constructed.";

    /**
     * Servlet init.
     *
     * @throws ServletException
     */
    @Override
    public void init() throws ServletException {
        super.init();
        LOGGER.debug("This is a debug message.");
        LOGGER.info("This is an info message.");
        LOGGER.warn("This is a warn message.");
        LOGGER.error("This is an error message.");
        LOGGER.fatal("This is a fatal message.");
    }

}
