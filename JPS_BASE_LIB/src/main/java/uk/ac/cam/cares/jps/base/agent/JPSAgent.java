package uk.ac.cam.cares.jps.base.agent;

import org.json.JSONObject;
import uk.ac.cam.cares.jps.base.interfaces.JPSAgentInterface;
import uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet;

import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.BadRequestException;

/**
 * Base class for JPS Agent instances, the methods listed here should be overridden by the 
 * concrete class as required.
 * 
 * @author Arkadiusz Chadzynski
 */
public class JPSAgent extends JPSHttpServlet implements JPSAgentInterface {

    /**
	 * Serialization identifier.
	 */
	private static final long serialVersionUID = 1L;

    /**
     * Process the JSON contents of a new HTTP request and return a response in JSON form.
     * 
     * @param requestParams incoming JSON content.
     * 
     * @return resulting JSON content.
     */
	@Override
    public JSONObject processRequestParameters(JSONObject requestParams) {
    	validateInput(requestParams);
    	return new JSONObject();
    }

    /**
     * Process the JSON contents of a new HTTP request, and the request details itself, and return a response in JSON form.
     * 
     * @param requestParams incoming JSON content.
     * @param request originating HTTP request.
     * 
     * @return resulting JSON content.
     */
    @Override
    public JSONObject processRequestParameters(JSONObject requestParams, HttpServletRequest request) {
    	validateInput(requestParams);
    	return new JSONObject();
    }

    /**
     * Validate the JSON contents of the request.
     * 
     * @param requestParams incoming JSON content.
     * 
     * @return validity of content.
     * 
     * @throws BadRequestException if request if malformed/incomplete.
     */
    @Override
    public boolean validateInput(JSONObject requestParams) throws BadRequestException {
        if (requestParams.isEmpty()) {
            throw new BadRequestException("Empty request!");
        }
        return true;
    }

}
// End of class.