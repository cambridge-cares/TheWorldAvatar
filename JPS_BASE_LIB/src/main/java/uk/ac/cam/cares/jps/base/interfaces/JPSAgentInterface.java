package uk.ac.cam.cares.jps.base.interfaces;

import org.json.JSONObject;

import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.BadRequestException;

public interface JPSAgentInterface {

    /**
     * Shall implement logic transforming requestParams to responseParam
     *
     * @param requestParams
     * @return response body
     */
    JSONObject processRequestParameters(JSONObject requestParams);


    /**
     * Shall implement logic transforming requestParams to responseParams. Has access to full request.
     *
     * @param requestParams
     * @param request
     * @return response body
     */
    JSONObject processRequestParameters(JSONObject requestParams, HttpServletRequest request);

    /**
     * Shall implement input validation logic.
     *
     * @return validation confirmation
     * @throws BadRequestException
     */
    boolean validateInput(JSONObject requestParams) throws BadRequestException;

}
