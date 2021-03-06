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

@WebServlet(urlPatterns = {"/kb/*"})
public class KnowledgeBaseAgentNew extends JPSAgent{
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	@Override
	public JSONObject processRequestParameters(JSONObject requestParams) {
		if (!validateInput(requestParams)) {
			throw new BadRequestException();
		}
		return main(requestParams);
		
		}
	public JSONObject main(JSONObject requestParams) {
		boolean isUpdateOperation  = false;
		boolean  isQueryOperation=false;
		JSONObject JSONresult = new JSONObject();
		String sparqlquery = MiscUtil.optNullKey(requestParams, JPSConstants.QUERY_SPARQL_QUERY);
		String sparqlupdate = MiscUtil.optNullKey(requestParams,  JPSConstants.QUERY_SPARQL_UPDATE);
		if (sparqlquery != null) isQueryOperation = true;
		else if (sparqlupdate != null) isUpdateOperation = true;
		String targetResourceIRIOrPath = requestParams.getString(JPSConstants.TARGETIRI);
		KnowledgeBaseClientInterface kbClient = KGRouter.getKnowledgeBaseClient(targetResourceIRIOrPath, isQueryOperation,isUpdateOperation);
		if (isQueryOperation) { 
			String result = kbClient.execute(sparqlquery);
			JSONresult.put("results",result);
			}
		else if (isUpdateOperation) {
			//perform update
			kbClient.setQuery(sparqlupdate);
			kbClient.executeUpdate();
		}else { //perform post/creation of new file
//			String newFilePath = MiscUtil.optNullKey(requestParams,  JPSConstants.SCENARIO_RESOURCE);
//			kbClient.setPath(null, newFilePath);
//			
//			kbClient.end();
			
		}
		return JSONresult; 
	}
	@Override
	public boolean validateInput(JSONObject requestParams) throws BadRequestException {
	    if (requestParams.isEmpty()) {
	        throw new BadRequestException();
	    }
	    try {
	    	String iriOrPath = requestParams.getString(JPSConstants.TARGETIRI);
	        boolean q = InputValidator.checkIfURLpattern(iriOrPath);
	        boolean v = InputValidator.checkIfFilePath(iriOrPath);
	        String sparqlquery = MiscUtil.optNullKey(requestParams, JPSConstants.QUERY_SPARQL_QUERY);
			String sparqlupdate = MiscUtil.optNullKey(requestParams,  JPSConstants.QUERY_SPARQL_UPDATE);
			if ((sparqlquery == null && sparqlupdate == null )||// if both are empty
					(sparqlquery != null && sparqlupdate != null)) { //or if both are filled. 
				return false;
			}else if (sparqlquery != null) {
				if (InputValidator.checkIfValidQuery(sparqlquery )!= true){
					return false;
					}
			}else if (sparqlupdate!= null) {
				if (InputValidator.checkIfValidUpdate(sparqlupdate)!= true){
					return false;
					}
			}
	        
	        return( q || v);
	    }catch (JSONException ex) {
	    	return false;
	    }
	}
	
}
