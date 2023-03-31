package uk.ac.cam.cares.jps.base.interfaces;

import org.json.JSONArray;
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

    /**
     * Shall implement logic to perform a sparql query on a resource in the KG.
     * @param targetResourceID
     * @param sparqlQuery
     * @return
     */
    JSONArray queryStore(String targetResourceID, String sparqlQuery);
    
    /**
     * Shall implement logic to perform a sparql update on a resource in the KG.
     * @param targetResourceID
     * @param sparqlUpdate
     */
    void updateStore(String targetResourceID, String sparqlUpdate);

    /**
     * Shall implement logic to call another agent with a HTTP GET request.
     * @param agentID
     * @param jsonParameters
     * @return
     */
    String callAgentWithGet(String agentID, JSONObject jsonParameters);
    
    /**
     * Shall implement logic to call another agent with a HTTP POST request
     * with JSON request parameters and JSON response.
     * @param agentID
     * @param jsonParameters
     * @return
     */
    String callAgentWithPost(String agentID, JSONObject jsonParameters);
    
    /**
     * Shall implement logic to call another agent with a HTTP POST request.
     * @param agentID
     * @param body
     * @param contentType
     * @param accept
     * @return
     */
    String callAgentWithPost(String agentID, String body, String contentType, String accept) ;
}
