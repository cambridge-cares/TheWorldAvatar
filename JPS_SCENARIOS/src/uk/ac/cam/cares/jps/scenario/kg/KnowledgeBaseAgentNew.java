package uk.ac.cam.cares.jps.scenario.kg;


import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.BadRequestException;

import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.client.methods.HttpPut;
import org.json.JSONException;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.config.JPSConstants;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.interfaces.KnowledgeBaseClientInterface;
import uk.ac.cam.cares.jps.base.query.KGRouter;
import uk.ac.cam.cares.jps.base.query.KnowledgeBaseClient;
import uk.ac.cam.cares.jps.base.util.InputValidator;
import uk.ac.cam.cares.jps.base.util.MiscUtil;
import uk.ac.cam.cares.jps.scenario.kb.KnowledgeBaseAbstract;
import uk.ac.cam.cares.jps.scenario.kb.KnowledgeBaseManager;

@WebServlet(urlPatterns = {"/kb/*"})
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
		System.out.println("JSON PARAMS" + requestParams.toString());
		if (!validateInput(requestParams)) {
			throw new JSONException("KnowledgeBaseAgent: Input parameters not found.\n");
		}
		
		boolean isUpdateOperation  = false;
		boolean  isQueryOperation=false;
		JSONObject JSONresult = new JSONObject();
		String sparqlquery = MiscUtil.optNullKey(requestParams, JPSConstants.QUERY_SPARQL_QUERY);
		String sparqlupdate = MiscUtil.optNullKey(requestParams,  JPSConstants.QUERY_SPARQL_UPDATE);
		if (sparqlquery != null) isQueryOperation = true;
		else if (sparqlupdate != null) isUpdateOperation = true;
		
		String method = MiscUtil.optNullKey(requestParams, JPSConstants.METHOD);
        String accept = MiscUtil.optNullKey(requestParams, JPSConstants.HEADERS);
        
		String body = MiscUtil.optNullKey(requestParams, JPSConstants.CONTENT);
		String contentType = MiscUtil.optNullKey(requestParams, JPSConstants.CONTENTTYPE);
		String requestUrl = MiscUtil.optNullKey(requestParams, JPSConstants.REQUESTURL);
		String path = MiscUtil.optNullKey(requestParams, JPSConstants.PATH);
		String paramResourceUrl= MiscUtil.optNullKey(requestParams,JPSConstants.SCENARIO_RESOURCE);		
	    String paramDatasetUrl = MiscUtil.optNullKey(requestParams, JPSConstants.SCENARIO_DATASET);
	    
		String datasetUrl = KnowledgeBaseManager.getDatasetUrl(requestUrl);
		String resourceUrl = getResourceUrl(datasetUrl, requestUrl, paramResourceUrl);
	
		String targetResourceIRIOrPath = requestParams.getString(JPSConstants.TARGETIRI); //TODO check this is always present
		KnowledgeBaseClientInterface kbClient = KGRouter.getKnowledgeBaseClient(targetResourceIRIOrPath, isQueryOperation,isUpdateOperation);
		
		String result = null;
		logInputParams(method, requestUrl, path, paramDatasetUrl, paramResourceUrl, contentType, sparqlquery, sparqlupdate, false);
		switch (method) {
			case HttpGet.METHOD_NAME:
				if (isQueryOperation) { 
					 result = kbClient.execute(sparqlquery);
					JSONresult.put("result",result);
				}else if (isUpdateOperation) { //TODO: update is a POST operation
					//perform update
					kbClient.setQuery(sparqlupdate);
					kbClient.executeUpdate();
				}else {//get
					result = kbClient.get(resourceUrl, accept); //TODO check this
					JSONresult.put("result",result);
				}
			    break;
		  case HttpPost.METHOD_NAME:
				if (!isUpdateOperation) {
					throw new JPSRuntimeException("parameter " + JPSConstants.QUERY_SPARQL_UPDATE + " is missing");
				}
				//perform update
				kbClient.setQuery(sparqlupdate);
				kbClient.executeUpdate();
				break;
		  case HttpPut.METHOD_NAME:
				if (!isQueryOperation && !isUpdateOperation) {
					throw new JPSRuntimeException("parameter " + JPSConstants.QUERY_SPARQL_UPDATE + " is not allowed");
				}    			
				kbClient.put(resourceUrl, body, contentType); //TODO check this
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
	
	protected void logInputParams(String httpVerb, String requestUrl, String path, String datasetUrl, String resourceUrl, String contentType, String sparqlquery, String sparqlupdate, boolean hasErrorOccured) {
		StringBuffer b = new StringBuffer(httpVerb);
		b.append(" with requestedUrl=").append(requestUrl);
		b.append(", path=").append(path);
		b.append(", datasetUrl=").append(datasetUrl);
		b.append(", resourceUrl=").append(resourceUrl);
		b.append(", contentType=").append(contentType);
		if (hasErrorOccured) {
			b.append(", sparqlquery=" + sparqlquery);
			b.append(", sparqlupdate=" + sparqlupdate);
			logger.error(b.toString());
		} else {
			if (sparqlquery != null) {
				int i = sparqlquery.toLowerCase().indexOf("select");
				if (i > 0) {
					sparqlquery = sparqlquery.substring(i);
				}
				if (sparqlquery.length() > 150) {
					sparqlquery = sparqlquery.substring(0, 150);
				}
			}
			if (sparqlupdate != null) {
				int i = sparqlupdate.toLowerCase().indexOf("select");
				if (i > 0) {
					sparqlupdate = sparqlupdate.substring(i);
				}
				if (sparqlupdate.length() > 150) {
					sparqlupdate = sparqlupdate.substring(0, 150);
				}
			}
			b.append(", sparqlquery (short)=" + sparqlquery);
			b.append(", sparqlupdate (short)=" + sparqlupdate);
			logger.info(b.toString());
		}
	}
	
	public String getResourceUrl(String datasetUrl, String requestUrl, String parameterUrl) {

		// Example: datasetUrl = http://www.thw.com/jps/data/test
		
		if (requestUrl.equals(datasetUrl)) {
			
			if ((parameterUrl == null) || parameterUrl.isEmpty()) {
				return null;
			} else {
				// case 2: indirect query
				return KnowledgeBaseClient.cutHashFragment(parameterUrl);
			}
			
		} else {
			if ((parameterUrl == null) || parameterUrl.isEmpty()) {
				// case 3: direct query
				return requestUrl;
			}
		}
		
		String message = "A URL was given by the query parameter " + JPSConstants.SCENARIO_RESOURCE 
				+ ". This is not allowed since the requested URL does not define a dataset URL."
				+ " parameter URL = " + parameterUrl + ", requested URL=" + requestUrl;
		throw new JPSRuntimeException(message);
	}
}
