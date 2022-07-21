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
import uk.ac.cam.cares.jps.base.util.InputValidator;
import uk.ac.cam.cares.jps.base.util.MiscUtil;

/**
 * The purpose of the AccessAgent servlet is to handle HTTP requests to perform SPARQL query 
 * and update operations on RDF resources in the knowledge graph. The agent will also 
 * perform requests to "get" and "insert" entire graphs. Requests are executed by the 
 * StoreAccessHandler class. 
 * This agent extends the JPSAgent framework and can be called using methods in the 
 * AccessAgentCaller class in jps_base_lib.
 *  
 * <p> All requests must provide a "targetresourceiri" {@link JPSConstants.TARGETIRI} and 
 * use one of the following HTTP methods with request parameters: 
 * 	<p>HTTP GET: perform a SPARQL query or "get" (if no sparql query is provided) 
 * 		<br> (for query operation) sparql query {@link JPSConstants.QUERY_SPARQL_QUERY}
 * 		<br> (for get operation, optional) target graph {@link JPSConstants.TARGETGRAPH}
 * 		<br> (for get operation, optional) accept {@link JPSConstants.HEADERS}, see {@link MediaType}
 * 	<p>HTTP POST: perform a SPARQL update
 * 		<br> sparql update {@link JPSConstants.QUERY_SPARQL_UPDATE}
 * <p>HTTP PUT: "insert" graph  
 * 		<br> rdf content {@link JPSConstants.CONTENT}
 * 		<br> content type {@link JPSConstants.CONTENTTYPE}
 * 		<br> target graph {@link JPSConstants.TARGETGRAPH}
 * 
 * @author csl37
 *
 */
@WebServlet(urlPatterns = {AccessAgent.ACCESS_URL})
public class AccessAgent extends JPSAgent{

	private static final long serialVersionUID = 1L;
	
	public static final String ACCESS_URL = "/access";
		
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
		
		if (!validateInput(requestParams)) {
			throw new JSONException("AccessAgent: Input parameters not valid.\n");
		}
		
		String method = MiscUtil.optNullKey(requestParams, JPSConstants.METHOD);
		
		StoreAccessHandler storeAccessHandler = new StoreAccessHandler();
		JSONObject JSONresult = new JSONObject();
		
		LOGGER.info("Initialising StoreAccessHandler to perform "+method+" request.");
		
		switch (method) {
			case HttpGet.METHOD_NAME:	
				JSONresult = storeAccessHandler.perfromGet(requestParams);
			    break;
			case HttpPost.METHOD_NAME:
				JSONresult = storeAccessHandler.perfromPost(requestParams);
				break;
			case HttpPut.METHOD_NAME:
				storeAccessHandler.performPut(requestParams);
				break;
			}		
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
	
	
}
