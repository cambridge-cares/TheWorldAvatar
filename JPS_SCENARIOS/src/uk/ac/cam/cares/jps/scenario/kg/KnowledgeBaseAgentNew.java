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

@WebServlet(urlPatterns = {"/kb/*"})
public class KnowledgeBaseAgentNew extends JPSAgent{

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
		
		JSONObject JSONresult = new JSONObject();
		String method = MiscUtil.optNullKey(requestParams, JPSConstants.METHOD);
		
		switch (method) {
			case HttpGet.METHOD_NAME:	
				JSONresult = get(requestParams);
			    break;
			case HttpPost.METHOD_NAME:
				post(requestParams);
				break;
			case HttpPut.METHOD_NAME:
				put(requestParams);
				break;
			}		
	    return JSONresult;
	}
		
	/**
	 * Perform http get: sparql query or get all triples from graph
	 * @param requestParams
	 * @return
	 */
	public JSONObject get(JSONObject requestParams) {
		
		String sparqlquery = MiscUtil.optNullKey(requestParams, JPSConstants.QUERY_SPARQL_QUERY);
		String requestUrl = MiscUtil.optNullKey(requestParams, JPSConstants.REQUESTURL);
		String datasetUrl = getDatasetUrl(requestUrl);
		String paramResourceUrl= MiscUtil.optNullKey(requestParams,JPSConstants.SCENARIO_RESOURCE);
		String resourceUrl = getResourceUrl(datasetUrl, requestUrl, paramResourceUrl);
		String accept = MiscUtil.optNullKey(requestParams, JPSConstants.HEADERS);
		String contentType = MiscUtil.optNullKey(requestParams, JPSConstants.CONTENTTYPE);
		String path = MiscUtil.optNullKey(requestParams, JPSConstants.PATH);		
	    String paramDatasetUrl = MiscUtil.optNullKey(requestParams, JPSConstants.SCENARIO_DATASET);		
	    String targetResourceIRIOrPath = requestParams.getString(JPSConstants.TARGETIRI);
		
		try {
			logInputParams("GET", requestUrl, path, paramDatasetUrl, paramResourceUrl, targetResourceIRIOrPath, contentType, sparqlquery, false);
			
			KnowledgeBaseClientInterface kbClient = KGRouter.getKnowledgeBaseClient(targetResourceIRIOrPath, true, false);
			
			JSONObject JSONresult = new JSONObject();
			String result = null;
			if (sparqlquery != null) { 
				//query
				result = kbClient.execute(sparqlquery);
				JSONresult.put("result",result);
			}else {		//TODO: defaulting to this could be dangerous for large triple store
				//get
				result = kbClient.get(resourceUrl, accept);
				JSONresult.put("result",result);
			}
			return JSONresult;
		
		} catch (RuntimeException e) {
			logInputParams("GET", requestUrl, path, paramDatasetUrl, paramResourceUrl, targetResourceIRIOrPath, contentType, sparqlquery, true);
			throw new JPSRuntimeException(e);
		}
	}
	
	/**
	 * Perform http put: insert triples
	 * @param requestParams
	 */
	public void put(JSONObject requestParams) {
		
		String requestUrl = MiscUtil.optNullKey(requestParams, JPSConstants.REQUESTURL);
		String paramResourceUrl= MiscUtil.optNullKey(requestParams,JPSConstants.SCENARIO_RESOURCE);
		String datasetUrl = getDatasetUrl(requestUrl);
		String resourceUrl = getResourceUrl(datasetUrl, requestUrl, paramResourceUrl);
		String path = MiscUtil.optNullKey(requestParams, JPSConstants.PATH);		
		String body = MiscUtil.optNullKey(requestParams, JPSConstants.CONTENT);
		String contentType = MiscUtil.optNullKey(requestParams, JPSConstants.CONTENTTYPE);
	    String paramDatasetUrl = MiscUtil.optNullKey(requestParams, JPSConstants.SCENARIO_DATASET);	
	    String targetResourceIRIOrPath = requestParams.getString(JPSConstants.TARGETIRI);
	    
		try {
			logInputParams("PUT", requestUrl, path, paramDatasetUrl, paramResourceUrl, targetResourceIRIOrPath, contentType, null, false);
			
			//TODO check target or datasetUrl for this
			KnowledgeBaseClientInterface kbClient = KGRouter.getKnowledgeBaseClient(datasetUrl, false, true);
			
			kbClient.insert(resourceUrl, body, contentType);
		} catch (RuntimeException e) {
			logInputParams("PUT", requestUrl, path, paramDatasetUrl, paramResourceUrl, targetResourceIRIOrPath, contentType, null, true);
			throw new JPSRuntimeException(e);
		}
	}
	
	/**
	 * Perform http post: sparql update
	 * @param requestParams
	 */
	public void post(JSONObject requestParams) {	
		
		//TODO UPdate in the body or header?
		String sparqlupdate = MiscUtil.optNullKey(requestParams, JPSConstants.QUERY_SPARQL_UPDATE);
		String body = MiscUtil.optNullKey(requestParams, JPSConstants.CONTENT);
		
		String path = MiscUtil.optNullKey(requestParams, JPSConstants.PATH);		
		String requestUrl = MiscUtil.optNullKey(requestParams, JPSConstants.REQUESTURL);
		String paramResourceUrl= MiscUtil.optNullKey(requestParams,JPSConstants.SCENARIO_RESOURCE);
		String paramDatasetUrl = MiscUtil.optNullKey(requestParams, JPSConstants.SCENARIO_DATASET);	
		String contentType = MiscUtil.optNullKey(requestParams, JPSConstants.CONTENTTYPE);
		String targetResourceIRIOrPath = requestParams.getString(JPSConstants.TARGETIRI);
		
		try {
			logInputParams("POST", requestUrl, path, paramDatasetUrl, paramResourceUrl, targetResourceIRIOrPath, contentType, null, false);
			
			//TODO check target or datasetUrl for this
			KnowledgeBaseClientInterface kbClient = KGRouter.getKnowledgeBaseClient(targetResourceIRIOrPath, false, true);
			
			if (sparqlupdate!=null) {
				//perform update
				kbClient.executeUpdate(sparqlupdate);
			}else {
				throw new JPSRuntimeException("parameter " + JPSConstants.QUERY_SPARQL_UPDATE + " is missing");
			}
		} catch (RuntimeException e) {
			logInputParams("POST", requestUrl, path, paramDatasetUrl, paramResourceUrl, targetResourceIRIOrPath, contentType, null, true);
			throw new JPSRuntimeException(e);
		}
	}
	
	@Override
	public boolean validateInput(JSONObject requestParams) throws BadRequestException {	    
		
	    if (requestParams.isEmpty()) {
	        throw new BadRequestException();
	    }
	    try {
	    	
	    	String method = MiscUtil.optNullKey(requestParams,JPSConstants.METHOD);
	        if (method == null) {
	        	return false;
	        }
	    	
	        boolean q = InputValidator.checkIfURLpattern(requestParams.getString(JPSConstants.REQUESTURL));
	        if(!q) {return false;};
	    	
	        
	        String sparqlquery = MiscUtil.optNullKey(requestParams, JPSConstants.QUERY_SPARQL_QUERY);
			String sparqlupdate = MiscUtil.optNullKey(requestParams,  JPSConstants.QUERY_SPARQL_UPDATE);
			if (sparqlquery != null && sparqlupdate != null) { //both query and update are filled. 
				return false;
			}else {
				if (sparqlquery != null) { 
					if (InputValidator.checkIfValidQuery(sparqlquery)!= true){
						return false;
					}
				}
				if (sparqlupdate == null) {
					if (InputValidator.checkIfValidUpdate(sparqlupdate)!= true){
						return false;
					}
				}
			}
			
			String iriOrPath = requestParams.getString(JPSConstants.TARGETIRI);
	        boolean isURL = InputValidator.checkIfURLpattern(iriOrPath);
	        boolean isPath = InputValidator.checkIfFilePath(iriOrPath);
	        return( isURL || isPath);
	        
	    }catch (JSONException ex) {
	    	return false;
	    }
	}
	
	protected void logInputParams(String httpVerb, String requestUrl, String path, String datasetUrl, String resourceUrl, String targetResourceIRIOrPath, String contentType, String sparql, boolean hasErrorOccured) {
		StringBuffer b = new StringBuffer(httpVerb);
		b.append(" with requestedUrl=").append(requestUrl);
		b.append(", path=").append(path);
		b.append(", datasetUrl=").append(datasetUrl);
		b.append(", resourceUrl=").append(resourceUrl);
		b.append(", targetResourceIRIOrPath=").append(targetResourceIRIOrPath);
		b.append(", contentType=").append(contentType);
		if (hasErrorOccured) {
			b.append(", sparql=" + sparql);
			logger.error(b.toString());
		} else {
			if (sparql != null) {
				int i = sparql.toLowerCase().indexOf("select");
				if (i > 0) {
					sparql = sparql.substring(i);
				}
				if (sparql.length() > 150) {
					sparql = sparql.substring(0, 150);
				}
			}
			b.append(", sparql (short)=" + sparql);
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
	
	public static String getDatasetUrl(String requestUrl) {
		String jps = "/" + JPSConstants.KNOWLEDGE_BASE_JPS + "/";
		int i = requestUrl.indexOf(jps) + jps.length();
		String rest = requestUrl.substring(i);
		if (rest.startsWith("data/")) {
			i += "data/".length();
		}  else if (rest.startsWith("dataset/")) {
			i += "dataset/".length();
		} else if (rest.startsWith("kb/")) {
			i += "kb/".length();
		} else if (rest.startsWith("scenario/")) {
			i += "scenario/".length();
		}
		String datasetUrl = requestUrl.substring(0, i);
		rest = requestUrl.substring(i);
		i = rest.indexOf("/");
		if (i >= 0) {
			datasetUrl += rest.substring(0, i);
		} else {
			datasetUrl = requestUrl;
		}
		return datasetUrl;
	}
}
