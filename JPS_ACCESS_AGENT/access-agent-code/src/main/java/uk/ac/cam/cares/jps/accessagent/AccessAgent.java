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
import uk.ac.cam.cares.jps.base.discovery.MediaType;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.interfaces.TripleStoreClientInterface;
import uk.ac.cam.cares.jps.base.query.StoreRouter;
import uk.ac.cam.cares.jps.base.util.InputValidator;
import uk.ac.cam.cares.jps.base.util.MiscUtil;

/**
 * The purpose of the AccessAgent servlet is to handle HTTP requests to perform SPARQL query 
 * and update operations on RDF resources in the knowledge graph. The agent will also 
 * perform requests to "get" and "insert" entire graphs. 
 * This agent extends the JPSAgent framework and can be called using methods in the 
 * AccessAgentCaller class in jps_base_lib.
 *  
 * <p> All requests must provide a "targetresourceiri" {@link JPSConstants#TARGETIRI} and
 * use one of the following HTTP methods with request parameters: 
 * 	<p>HTTP GET: get the entire graph 
 * 		<br> (optional) target graph {@link JPSConstants#TARGETGRAPH}
 * 		<br> (optional) accept {@link JPSConstants#HEADERS}, see {@link MediaType}
 * 	<p>HTTP POST: perform a SPARQL update
 * 		<br> sparql update {@link JPSConstants#QUERY_SPARQL_UPDATE}
 * <p>HTTP PUT: "insert" graph  
 * 		<br> rdf content {@link JPSConstants#CONTENT}
 * 		<br> content type {@link JPSConstants#CONTENTTYPE}
 * 		<br> target graph {@link JPSConstants#TARGETGRAPH}
 * 
 * @author csl37
 *
 */
@WebServlet(urlPatterns = {AccessAgent.ACCESS_URL, AccessAgent.CLEAR_CACHE_URL})
public class AccessAgent extends JPSAgent{

	private static final long serialVersionUID = 1L;
	
	public static final String ACCESS_URL = "/access";
	public static final String CLEAR_CACHE_URL = "/clearcache";
		
	/**
     * Logger for error output.
     */
    private static final Logger LOGGER = LogManager.getLogger(AccessAgent.class);
	    
    
    @Override
    public JSONObject processRequestParameters(JSONObject requestParams) {
    	//Do nothing
        return new JSONObject();
    }
    
	@Override
	public JSONObject processRequestParameters(JSONObject requestParams, HttpServletRequest request) {
		
		String method = MiscUtil.optNullKey(requestParams, JPSConstants.METHOD);
		
		// Clear cache
		if(request.getServletPath().equals(CLEAR_CACHE_URL)) {
			if(method.equals(HttpGet.METHOD_NAME)) {
				return clearCache();
			}else {
				throw new JPSRuntimeException("AccessAgent: Input parameters not valid.\n");
			}
		
		// SPARQL
		}else {
		
			if (!validateInput(requestParams)) {
				throw new JPSRuntimeException("AccessAgent: Input parameters not valid.\n");
			}			
			
			JSONObject JSONresult = new JSONObject();
				
			switch (method) {
				case HttpGet.METHOD_NAME:	
					JSONresult = performGet(requestParams);
				    break;
				case HttpPost.METHOD_NAME:
					JSONresult = performPost(requestParams);
					break;
				case HttpPut.METHOD_NAME:
					performPut(requestParams);
					break;
				}		
		    return JSONresult;
		}
	}
	
	/**
	 * Clear StoreRouter cache
	 * @return
	 */
	public JSONObject clearCache() {
		StoreRouter.getInstance().clearCache();
		JSONObject JSONresult = new JSONObject().put(JPSConstants.RESULT_KEY, "Cache cleared.");
		return JSONresult;
	}
	
	@Override
	public boolean validateInput(JSONObject requestParams) throws BadRequestException {	    
		
	    if (requestParams.isEmpty()) {
	        throw new BadRequestException();
	    }
	    try {
	    	
	    	//GET, PUT or POST
	    	String method = MiscUtil.optNullKey(requestParams,JPSConstants.METHOD);
	        if (!method.equals(HttpGet.METHOD_NAME) && !method.equals(HttpPut.METHOD_NAME) && !method.equals(HttpPost.METHOD_NAME) ) {
	        	LOGGER.error("Invalid input parameters: Not HTTP GET, PUT or POST!");
	        	return false;
	        }
	    	
	        //targetResourceRequired
	        String targetiri = MiscUtil.optNullKey(requestParams,JPSConstants.TARGETIRI);
	        if (targetiri == null) {
	        	LOGGER.error("Invalid input parameters: targetResourceID not provided!");
	        	return false;
	        }
	        
	        boolean q = InputValidator.checkIfURLpattern(requestParams.getString(JPSConstants.REQUESTURL));
	        if(!q) {return false;};
	    	
	        //valid SPARQL query or update, not both
	        String sparqlquery = MiscUtil.optNullKey(requestParams, JPSConstants.QUERY_SPARQL_QUERY);
			String sparqlupdate = MiscUtil.optNullKey(requestParams,  JPSConstants.QUERY_SPARQL_UPDATE);
			if (sparqlquery != null && sparqlupdate != null) { //both query and update are filled.
				LOGGER.error("Invalid input parameters: Must be either SPARQL query or update. Not both!");
				return false;
			}else {
				if (sparqlquery != null) { 
					if (InputValidator.checkIfValidQuery(sparqlquery)!= true){
						LOGGER.error("Invalid input parameters: Invalid SPARQL query!");
						return false;
					}
				}
				if (sparqlupdate != null) {
					if (InputValidator.checkIfValidUpdate(sparqlupdate)!= true){
						LOGGER.error("Invalid input parameters: Invalid SPARQL update!");
						return false;
					}
				}
			}
			
			return true;
	        
	    }catch (JSONException ex) {
	    	return false;
	    }
	}
	
	 /**
	 * Perform HTTP GET. This will "get" all triples (from specified graph).
	 * @param requestParams
	 * @return
	 */
	public JSONObject performGet(JSONObject requestParams) {
		
		String sparqlquery = MiscUtil.optNullKey(requestParams, JPSConstants.QUERY_SPARQL_QUERY);
		String sparqlupdate = MiscUtil.optNullKey(requestParams, JPSConstants.QUERY_SPARQL_UPDATE);
		String accept = MiscUtil.optNullKey(requestParams, JPSConstants.HEADERS);		
	    String targetIRI = requestParams.getString(JPSConstants.TARGETIRI);
	    String graphIRI = MiscUtil.optNullKey(requestParams, JPSConstants.TARGETGRAPH);
	    	    
	    if(sparqlquery!=null || sparqlupdate!=null) {
	    	throw new JPSRuntimeException("parameters " + JPSConstants.QUERY_SPARQL_QUERY + " and " 
	    									+ JPSConstants.QUERY_SPARQL_UPDATE + " are not allowed");
	    }
	    
		try {
			logInputParams(requestParams, sparqlquery, false);
			
			TripleStoreClientInterface kbClient = getStoreClient(targetIRI, true, false);
			
			JSONObject JSONresult = new JSONObject();
			String result = null;
			
			//get
			result = kbClient.get(graphIRI, accept);
			JSONresult.put(JPSConstants.RESULT_KEY,result);
		
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
	public void performPut(JSONObject requestParams) {
		
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

			TripleStoreClientInterface kbClient = getStoreClient(targetIRI, false, true);
			
			kbClient.insert(graphIRI, body, contentType);
		} catch (RuntimeException e) {
			logInputParams(requestParams, null, true);
			throw new JPSRuntimeException(e);
		}
	}
	
	/**
	 * Perform HTTP POST. This will perform a SPARQL update on the store. 
	 * @param requestParams
	 * @return 
	 */
	public JSONObject performPost(JSONObject requestParams) {	
		
		String sparqlquery = MiscUtil.optNullKey(requestParams, JPSConstants.QUERY_SPARQL_QUERY);
		String sparqlupdate = MiscUtil.optNullKey(requestParams, JPSConstants.QUERY_SPARQL_UPDATE);
		String targetIRI = requestParams.getString(JPSConstants.TARGETIRI);
			
		JSONObject JSONresult = new JSONObject();
		String result = null;
		
		try {

			if (sparqlupdate!=null) {
				//update
				logInputParams(requestParams, sparqlupdate, false);
				TripleStoreClientInterface kbClient = getStoreClient(targetIRI, false, true);
				LOGGER.info("Store client instantiated for update endpoint: "+kbClient.getUpdateEndpoint());
				LOGGER.info("Performing SPARQL update.");
				kbClient.executeUpdate(sparqlupdate);
				//TODO change this
				JSONresult.put(JPSConstants.RESULT_KEY,"Update completed!");
			}else if(sparqlquery!=null){
				//query
				logInputParams(requestParams, sparqlquery, false);
				TripleStoreClientInterface kbClient = getStoreClient(targetIRI, true, false);
				LOGGER.info("Store client instantiated for query endpoint: "+kbClient.getQueryEndpoint());
				LOGGER.info("Performing SPARQL query.");
				result = kbClient.execute(sparqlquery);
				JSONresult.put(JPSConstants.RESULT_KEY,result);
			}else {
				throw new JPSRuntimeException("SPARQL query or update is missing");
			}
			
			return JSONresult;
			
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
	public TripleStoreClientInterface getStoreClient(String targetIRI, boolean isQuery, boolean isUpdate) {
		try {
			TripleStoreClientInterface storeClient = StoreRouter.getStoreClient(targetIRI, isQuery, isUpdate);
			if (storeClient == null) {
				throw new RuntimeException();
			}
			return storeClient;
		}catch (RuntimeException e) {
			LOGGER.error("Failed to instantiate StoreClient");
			throw new JPSRuntimeException("Failed to instantiate StoreClient");
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
