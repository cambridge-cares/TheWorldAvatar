package uk.ac.cam.cares.jps.accessagent;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.config.JPSConstants;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.interfaces.StoreClientInterface;
import uk.ac.cam.cares.jps.base.query.StoreRouter;
import uk.ac.cam.cares.jps.base.util.MiscUtil;

/**
 * The purpose of this class is to execute store access requests (SPARQL query and update) for the Access Agent 
 * @author csl37
 *
 */
public class StoreAccessHandler {

	/**
     * Logger for error output.
     */
    private static final Logger LOGGER = LogManager.getLogger(StoreAccessHandler.class);
	
    /**
	 * Perform HTTP GET. This will "get" all triples (from specified graph).
	 * @param requestParams
	 * @return
	 */
	public JSONObject perfromGet(JSONObject requestParams) {
		
		String sparqlquery = MiscUtil.optNullKey(requestParams, JPSConstants.QUERY_SPARQL_QUERY);
		String sparqlupdate = MiscUtil.optNullKey(requestParams, JPSConstants.QUERY_SPARQL_UPDATE);
		String accept = MiscUtil.optNullKey(requestParams, JPSConstants.HEADERS);		
	    String targetIRI = requestParams.getString(JPSConstants.TARGETIRI);
	    String graphIRI = MiscUtil.optNullKey(requestParams, JPSConstants.TARGETGRAPH);
	    	    
	    if(sparqlquery!=null && sparqlupdate!=null) {
	    	throw new JPSRuntimeException("parameters " + JPSConstants.QUERY_SPARQL_QUERY + " and " 
	    									+ JPSConstants.QUERY_SPARQL_UPDATE + " are not allowed");
	    }
	    
		try {
			logInputParams(requestParams, sparqlquery, false);
			
			StoreClientInterface kbClient = getStoreClient(targetIRI, true, false);
			
			JSONObject JSONresult = new JSONObject();
			String result = null;
			
			//get
			result = kbClient.get(graphIRI, accept);
			JSONresult.put("result",result);
		
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
	 * @return 
	 */
	public JSONObject perfromPost(JSONObject requestParams) {	
		
		String sparqlquery = MiscUtil.optNullKey(requestParams, JPSConstants.QUERY_SPARQL_QUERY);
		String sparqlupdate = MiscUtil.optNullKey(requestParams, JPSConstants.QUERY_SPARQL_UPDATE);
		String targetIRI = requestParams.getString(JPSConstants.TARGETIRI);
			
		JSONObject JSONresult = new JSONObject();
		String result = null;
		
		try {

			if (sparqlupdate!=null) {
				//update
				logInputParams(requestParams, sparqlupdate, false);
				StoreClientInterface kbClient = getStoreClient(targetIRI, false, true);
				LOGGER.info("Store client instantiated for update endpoint: "+kbClient.getUpdateEndpoint());
				LOGGER.info("Performing SPARQL update.");
				kbClient.executeUpdate(sparqlupdate);
				//TODO change this
				JSONresult.put("result","Update completed!");
			}else if(sparqlquery!=null){
				//query
				logInputParams(requestParams, sparqlquery, false);
				StoreClientInterface kbClient = getStoreClient(targetIRI, true, false);
				LOGGER.info("Store client instantiated for query endpoint: "+kbClient.getQueryEndpoint());
				LOGGER.info("Performing SPARQL query.");
				result = kbClient.execute(sparqlquery);
				JSONresult.put("result",result);
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
	public StoreClientInterface getStoreClient(String targetIRI, boolean isQuery, boolean isUpdate) {
		try {
			StoreClientInterface storeClient = StoreRouter.getStoreClient(targetIRI, isQuery, isUpdate);
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
