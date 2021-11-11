package uk.ac.cam.cares.jps.base.interfaces;

import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.agent.JPSAgent;


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
    String query(String targetResourceID, String sparqlQuery);
    
    /**
     * Shall implement logic to perform a sparql update on a resource in the KG.
     * @param targetResourceID
     * @param sparqlUpdate
     */
    void update(String targetResourceID, String sparqlUpdate);
    
    /**
     * Shall implement logic to monitor the derivation that isDerivedUsing the instantiated agent.
     * @param agentIRI
     */
    void monitorDerivation(String agentIRI, JPSAgent agent);
}
