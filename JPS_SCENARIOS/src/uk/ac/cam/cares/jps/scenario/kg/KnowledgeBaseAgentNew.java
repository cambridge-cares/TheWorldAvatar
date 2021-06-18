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
	
	
	public KnowledgeBaseClientInterface getStoreClient(JSONObject requestParams, boolean isQueryOperation, boolean isUpdateOperation) {
		String targetResourceIRIOrPath = requestParams.getString(JPSConstants.TARGETIRI); //TODO check this is always present
		KnowledgeBaseClientInterface kbClient = KGRouter.getKnowledgeBaseClient(targetResourceIRIOrPath, isQueryOperation,isUpdateOperation);
		return kbClient;
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
		
		//////////////
		//TODO try catch with exception and logInputParams
		String contentType = MiscUtil.optNullKey(requestParams, JPSConstants.CONTENTTYPE);
		String path = MiscUtil.optNullKey(requestParams, JPSConstants.PATH);		
	    String paramDatasetUrl = MiscUtil.optNullKey(requestParams, JPSConstants.SCENARIO_DATASET);		
		logInputParams("GET", requestUrl, path, paramDatasetUrl, paramResourceUrl, contentType, sparqlquery, null, false);
		//////////////////
		
		KnowledgeBaseClientInterface kbClient = getStoreClient(requestParams, true, false);
		
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
	}
	
	/**
	 * Perform http put: insert triples
	 * @param requestParams
	 */
	public void put(JSONObject requestParams) {
		
		KnowledgeBaseClientInterface kbClient = getStoreClient(requestParams, false, true);
		
		String requestUrl = MiscUtil.optNullKey(requestParams, JPSConstants.REQUESTURL);
		String paramResourceUrl= MiscUtil.optNullKey(requestParams,JPSConstants.SCENARIO_RESOURCE);
		String datasetUrl = getDatasetUrl(requestUrl);
		String resourceUrl = getResourceUrl(datasetUrl, requestUrl, paramResourceUrl);
		
		String body = MiscUtil.optNullKey(requestParams, JPSConstants.CONTENT);
		String contentType = MiscUtil.optNullKey(requestParams, JPSConstants.CONTENTTYPE);
	    			
		kbClient.insert(resourceUrl, body, contentType);
	}
	
	/**
	 * Perform http post: sparql update
	 * @param requestParams
	 */
	public void post(JSONObject requestParams) {	
		
		String sparqlupdate = MiscUtil.optNullKey(requestParams, JPSConstants.QUERY_SPARQL_UPDATE);
		
		KnowledgeBaseClientInterface kbClient = getStoreClient(requestParams, false, true);
		
		if (sparqlupdate!=null) {
			//perform update
			kbClient.executeUpdate(sparqlupdate);
		}else {
			throw new JPSRuntimeException("parameter " + JPSConstants.QUERY_SPARQL_UPDATE + " is missing");
		}
	}
	
	//TODO validate all parameters
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
