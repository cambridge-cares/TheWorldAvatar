package uk.ac.cam.cares.derivation.asynexample;

import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.BadRequestException;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONObject;
import org.springframework.stereotype.Controller;
import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

/**
 * Example JPS Agent that receives a request with two numbers, adds them together, then returns the
 * result in a JSON format.
 *
 * @author Owen Parry {@literal <oparry@cmclinnovations.com>}
 */
@Controller
@WebServlet(urlPatterns = {AdditionAgent.API_PATTERN})
public class AdditionAgent extends JPSAgent {

    // Logger for reporting info/errors
    private static final Logger LOGGER = LogManager.getLogger(AdditionAgent.class);

    // Expected request keys
    private static final String FIRST_PARAM_KEY = "a";
    private static final String SECOND_PARAM_KEY = "b";
    private static final String RESULT_KEY = "c";

    // Responses
    private static final String BAD_REQUEST_MSG_KEY = "bad_request";
    private static final String REQUEST_RECEIVED_MSG = "Request received.";
    private static final String INVALID_REQUEST_MSG = "Invalid request.";

    // Request URL
    static final String API_PATTERN = "/api/v1";

    /**
     * Processes HTTP requests with originating details.
     *
     * @param requestParams Request parameters in a JSONObject.
     * @param request HTTP Servlet Request.
     *
     * @return response in JSON format.
     */
    @Override
    public JSONObject processRequestParameters(JSONObject requestParams, HttpServletRequest request) {
        return processRequestParameters(requestParams);
    }

    /**
     * Processes HTTP requests.
     *
     * @param requestParams Request parameters as a JSONObject.
     *
     * @return response in JSON format.
     */
    @Override
    public JSONObject processRequestParameters(JSONObject requestParams) {
        boolean isValidInput;

        LOGGER.info(REQUEST_RECEIVED_MSG);

        try {
            isValidInput = validateInput(requestParams);
        } catch (BadRequestException e) {
            return requestParams.put(BAD_REQUEST_MSG_KEY, e.getMessage());
        }
        if (isValidInput) {
            LOGGER.debug("Keys are " + requestParams.keySet().toString());

            Double resultVal = requestParams.getDouble(FIRST_PARAM_KEY) + requestParams.getDouble(SECOND_PARAM_KEY);
            JSONObject result = new JSONObject();
            result.put(RESULT_KEY, resultVal);
            return result;
        } else {
            LOGGER.error(INVALID_REQUEST_MSG);
            throw new JPSRuntimeException(INVALID_REQUEST_MSG);
        }
    }

    /**
     * Checks the incoming JSON request for validity.
     * 
     * @param requestParams JSOn request parameters.
     * 
     * @return request validity.
     * 
     * @throws BadRequestException if request is malformed.
     */
    @Override
    public boolean validateInput(JSONObject requestParams) throws BadRequestException {
        boolean valid = true;
        //requestParams.has(FIRST_PARAM_KEY) && requestParams.has(SECOND_PARAM_KEY);
        
        return valid;
    }

}
// End of class.
