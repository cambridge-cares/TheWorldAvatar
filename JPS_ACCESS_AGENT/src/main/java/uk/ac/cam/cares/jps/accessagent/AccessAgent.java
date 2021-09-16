package uk.ac.cam.cares.jps.accessagent;

import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.BadRequestException;

import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.client.methods.HttpPut;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONException;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.config.JPSConstants;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.interfaces.StoreClientInterface;
import uk.ac.cam.cares.jps.base.query.StoreRouter;
import uk.ac.cam.cares.jps.base.util.InputValidator;
import uk.ac.cam.cares.jps.base.util.MiscUtil;

@WebServlet(urlPatterns = {"/access"})
public class AccessAgent extends JPSAgent{

	private static final long serialVersionUID = 1L;
	
	/**
     * Logger for error output.
     */
    private static final Logger LOGGER = LogManager.getLogger(AccessAgent.class);
	
	@Override
	public JSONObject processRequestParameters(JSONObject requestParams) {
		JSONObject result = processRequestParameters(requestParams,null);
		return result;
	}

	@Override
	public JSONObject processRequestParameters(JSONObject requestParams, HttpServletRequest request) {
		System.out.println("JSON PARAMS" + requestParams.toString());
		if (!validateInput(requestParams)) {
			throw new JSONException("AccessAgent: Input parameters not found.\n");
		}
		
		JSONObject JSONresult = new JSONObject();
		String method = MiscUtil.optNullKey(requestParams, JPSConstants.METHOD);
		System.out.println("METHOD: "+ method);
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
	 * Perform HTTP GET. This will be either a SPARQL query or "get" all triples (from specified graph).
	 * @param requestParams
	 * @return
	 */
	public JSONObject get(JSONObject requestParams) {
		
		String sparqlquery = MiscUtil.optNullKey(requestParams, JPSConstants.QUERY_SPARQL_QUERY);
		String sparqlupdate = MiscUtil.optNullKey(requestParams, JPSConstants.QUERY_SPARQL_UPDATE);
		String accept = MiscUtil.optNullKey(requestParams, JPSConstants.HEADERS);		
	    String targetIRI = requestParams.getString(JPSConstants.TARGETIRI);
	    String graphIRI = MiscUtil.optNullKey(requestParams, JPSConstants.TARGETGRAPH);
	    
	    if(sparqlupdate != null) {
	    	throw new JPSRuntimeException("parameter " + JPSConstants.QUERY_SPARQL_UPDATE + " is not allowed");
	    }
	    
		try {
			logInputParams(requestParams, sparqlquery, false);
			
			StoreClientInterface kbClient = getStoreClient(targetIRI, true, false);
			
			JSONObject JSONresult = new JSONObject();
			String result = null;
			if (sparqlquery != null) { 
				//query
				result = kbClient.execute(sparqlquery);
				JSONresult.put("result",result);
			}else {	
				//get
				result = kbClient.get(graphIRI, accept);
				JSONresult.put("result",result);
			}
			return JSONresult;
		
		} catch (RuntimeException e) {
			logInputParams(requestParams, sparqlquery, true);
			throw new JPSRuntimeException(e);
		}
	}
	
	/**
	 * Perform HTTP PUT. Insert triples into store.
	 * @param requestParams
	 */
	public void put(JSONObject requestParams) {
		
		String sparqlquery = MiscUtil.optNullKey(requestParams, JPSConstants.QUERY_SPARQL_QUERY);
		String sparqlupdate = MiscUtil.optNullKey(requestParams, JPSConstants.QUERY_SPARQL_UPDATE);
		String body = MiscUtil.optNullKey(requestParams, JPSConstants.CONTENT);
		String contentType = MiscUtil.optNullKey(requestParams, JPSConstants.CONTENTTYPE);	
	    String targetIRI = requestParams.getString(JPSConstants.TARGETIRI);
	    String graphIRI = MiscUtil.optNullKey(requestParams, JPSConstants.TARGETGRAPH);
	    
	    if(sparqlquery!=null && sparqlupdate!=null) {
	    	throw new JPSRuntimeException("parameters " + JPSConstants.QUERY_SPARQL_QUERY + " and " 
	    									+ JPSConstants.QUERY_SPARQL_UPDATE + " are not allowed");
	    }
	    
		try {
			logInputParams(requestParams, null, false);
			
			StoreClientInterface kbClient = getStoreClient(targetIRI, false, true);
			
			kbClient.insert(graphIRI, body, contentType);
		} catch (RuntimeException e) {
			logInputParams(requestParams, null, true);
			throw new JPSRuntimeException(e);
		}
	}
	
	/**
	 * Perform HTTP POST. This will perform a SPARQL update on the store. 
	 * @param requestParams
	 */
	public void post(JSONObject requestParams) {	
		
		String sparqlquery = MiscUtil.optNullKey(requestParams, JPSConstants.QUERY_SPARQL_QUERY);
		String sparqlupdate = MiscUtil.optNullKey(requestParams, JPSConstants.QUERY_SPARQL_UPDATE);
		String targetIRI = requestParams.getString(JPSConstants.TARGETIRI);
		
		if(sparqlquery != null) {
			throw new JPSRuntimeException("parameter " + JPSConstants.QUERY_SPARQL_QUERY + " is not allowed");
		}
		
		try {
			logInputParams(requestParams, sparqlupdate, false);
			
			StoreClientInterface kbClient = getStoreClient(targetIRI, false, true);
			
			if (sparqlupdate!=null) {
				kbClient.executeUpdate(sparqlupdate);
			}else {
				throw new JPSRuntimeException("parameter " + JPSConstants.QUERY_SPARQL_UPDATE + " is missing");
			}
		} catch (RuntimeException e) {
			logInputParams(requestParams, sparqlupdate, true);
			throw new JPSRuntimeException(e);
		}
	}
	
	/**
	 * Instantiate a store client using StoreRouter
	 * @param targetIRI
	 * @param isQuery
	 * @param isUpdate
	 * @return
	 */
	public StoreClientInterface getStoreClient(String targetIRI, boolean isQuery, boolean isUpdate) {		
		try {
			return StoreRouter.getStoreClient(targetIRI, isQuery, isUpdate);
		}catch (RuntimeException e) {
			LOGGER.error("Failed to instantiate StoreClient");
			throw new JPSRuntimeException("Failed to instantiate StoreClient");
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
	    	
	        String targetiri = requestParams.getString(JPSConstants.TARGETIRI);
	        boolean v = InputValidator.checkIfURLpattern(targetiri);
	        if(!v) {return false;}
	        
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
				if (sparqlupdate != null) {
					if (InputValidator.checkIfValidUpdate(sparqlupdate)!= true){
						return false;
					}
				}
			}
			
			return true;
	        
	    }catch (JSONException ex) {
	    	return false;
	    }
	}
	
	protected void logInputParams(JSONObject requestParams, String sparql, boolean hasErrorOccured) {
		
		String method = MiscUtil.optNullKey(requestParams, JPSConstants.METHOD);
		String path = MiscUtil.optNullKey(requestParams, JPSConstants.PATH);		
		String requestUrl = MiscUtil.optNullKey(requestParams, JPSConstants.REQUESTURL);	
		String contentType = MiscUtil.optNullKey(requestParams, JPSConstants.CONTENTTYPE);
		String targetIRI = requestParams.getString(JPSConstants.TARGETIRI);
		String graphIRI = MiscUtil.optNullKey(requestParams, JPSConstants.TARGETGRAPH);
		
		StringBuffer b = new StringBuffer(method);
		b.append(" with requestedUrl=").append(requestUrl);
		b.append(", path=").append(path);
		b.append(", contentType=").append(contentType);
		b.append(", targetiri=").append(targetIRI);
		b.append(", targetgraph=").append(graphIRI);
		if (hasErrorOccured) {
			b.append(", sparql=" + sparql);
			LOGGER.error(b.toString());
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
			LOGGER.info(b.toString());
		}
	}
}
