package uk.ac.cam.cares.jps.agent.status.process;

import java.io.IOException;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.ws.rs.BadRequestException;
import javax.ws.rs.core.Response;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONObject;
import uk.ac.cam.cares.jps.agent.status.TestHandler;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;

/**
 * Interface for all classes that act on requests and return JSON content.
 *
 * @author Michael Hillman
 */
public abstract class StatusRequest {

    /**
     * Logger for error output.
     */
    protected final Logger LOGGER = LogManager.getLogger(getClass());

    /**
     * HTTP request.
     */
    protected final HttpServletRequest request;

    /**
     * HTTP response.
     */
    protected final HttpServletResponse response;

    /**
     * Current test handler instance.
     */
    protected final TestHandler handler;

    /**
     * Initialise a new StatusRequest instance.
     *
     * @param request HTTP request.
     * @param response HTTP response.
     * @param handler Test handler instance.
     */
    public StatusRequest(HttpServletRequest request, HttpServletResponse response, TestHandler handler) {
        this.request = request;
        this.response = response;
        this.handler = handler;
    }

    /**
     * Parse and return the JSON content of the request.
     *
     * @return JSON content in request.
     */
    protected JSONObject parseRequestJSON() {
        return AgentCaller.readJsonParameter(request);
    }

    /**
     *
     * @param result
     */
    protected void writeJSONResponse(JSONObject result, HttpServletResponse response) {
        try {
            response.getWriter().write(result.toString());
        } catch (Exception exception) {
            LOGGER.error("Exception when writing response to request!", exception);
            response.setStatus(Response.Status.SERVICE_UNAVAILABLE.getStatusCode());
        }
    }

    /**
     * Validate the request.
     * 
     * 
     * @throws BadRequestException 
     */
    public abstract void validateRequest() throws BadRequestException;
    
    /**
     * Act on the request.
     */
    public abstract void processRequest() throws ServletException, IOException;
}

// End of interface.
