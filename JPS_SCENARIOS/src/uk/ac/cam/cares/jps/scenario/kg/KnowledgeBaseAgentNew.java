package uk.ac.cam.cares.jps.scenario.kg;

import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.BadRequestException;

import org.json.JSONException;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.config.JPSConstants;
import uk.ac.cam.cares.jps.base.interfaces.KnowledgeBaseClientInterface;
import uk.ac.cam.cares.jps.base.query.KGRouter;
import uk.ac.cam.cares.jps.base.util.InputValidator;
import uk.ac.cam.cares.jps.base.util.MiscUtil;

@WebServlet(urlPatterns = {"/kb-new/*"})
public class KnowledgeBaseAgentNew extends JPSAgent{
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	@Override
	public JSONObject processRequestParameters(JSONObject requestParams) {
		JSONObject result = processRequestParameters(requestParams,null);
		return result;
	}
	@Override
    public JSONObject processRequestParameters(JSONObject requestParams, HttpServletRequest request) {	
		if (!validateInput(requestParams)) {
			throw new JSONException("KBAgent: Input parameters not found.\n ");
		}
		System.out.println("KBA: JSONPARAMS: " + requestParams.toString());
		return main(requestParams);
		
		}
	public JSONObject main(JSONObject requestParams) {
		boolean isUpdateOperation  = false;
		boolean  isQueryOperation=false;
		String sparqlquery = MiscUtil.optNullKey(requestParams, JPSConstants.QUERY_SPARQL_QUERY);
		String sparqlupdate = MiscUtil.optNullKey(requestParams,  JPSConstants.QUERY_SPARQL_UPDATE);
		if (sparqlquery != null) isQueryOperation = true;
		else if (sparqlupdate != null) isUpdateOperation = true;
		String targetResourceIRIOrPath = requestParams.getString(JPSConstants.SCENARIO_RESOURCE);
		KnowledgeBaseClientInterface kbClient = KGRouter.getKnowledgeBaseClient(targetResourceIRIOrPath, isQueryOperation,isUpdateOperation);
		String result = kbClient.execute(sparqlquery);
		return new JSONObject().put("result",result);
	}
	@Override
	public boolean validateInput(JSONObject requestParams) throws BadRequestException {
	    if (requestParams.isEmpty()) {
	        throw new BadRequestException();
	    }
	    try {
	        boolean q = InputValidator.checkURLpattern(requestParams.getString(JPSConstants.REQUESTURL));
	        String method = MiscUtil.optNullKey(requestParams,JPSConstants.METHOD);
	        if (method == null) {
	        	return false;
	        }
	        return q;
	    }catch (JSONException ex) {
	    	ex.printStackTrace();
	    	return false;
	    }
	}
}
