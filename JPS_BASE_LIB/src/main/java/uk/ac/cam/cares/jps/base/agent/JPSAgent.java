package uk.ac.cam.cares.jps.base.agent;

import org.json.JSONObject;
import uk.ac.cam.cares.jps.base.interfaces.JPSAgentInterface;
import uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet;

import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.BadRequestException;

public class JPSAgent extends JPSHttpServlet implements JPSAgentInterface {

    @Override
    public JSONObject processRequestParameters(JSONObject requestParams) {
            validateInput(requestParams);
            return requestParams;
    }

    @Override
    public JSONObject processRequestParameters(JSONObject requestParams, HttpServletRequest request) {
        validateInput(requestParams);
        return requestParams;
    }

    @Override
    public boolean validateInput(JSONObject requestParams) throws BadRequestException {
        if (requestParams.isEmpty()) {
            throw new BadRequestException();
        }
        return true;
    }

}
