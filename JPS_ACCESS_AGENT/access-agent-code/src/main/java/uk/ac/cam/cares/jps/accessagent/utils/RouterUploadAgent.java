package uk.ac.cam.cares.jps.accessagent.utils;

import javax.servlet.annotation.WebServlet;
import javax.ws.rs.BadRequestException;

import org.apache.http.client.methods.HttpPost;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.config.JPSConstants;
import uk.ac.cam.cares.jps.base.interfaces.StoreClientInterface;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.query.StoreRouter;
import uk.ac.cam.cares.jps.base.util.MiscUtil;

/**
 * This is a servlet to upload routing information to the store router.
 * See 
 * {@link uk.ac.cam.cares.jps.accessagent.utils.RoutingUploaderTool RoutingUploaderTool} 
 * for details 
 *  
 * @author csl37
 *
 */
@WebServlet(urlPatterns = {RouterUploadAgent.UPLOAD_URL})
public class RouterUploadAgent  extends JPSAgent{

	private static final long serialVersionUID = 1L;

	public static final String UPLOAD_URL = "/upload";
	
	/**
     * Logger for error output.
     */
    private static final Logger LOGGER = LogManager.getLogger(RouterUploadAgent.class);
	    
    @Override
    public JSONObject processRequestParameters(JSONObject requestParams) {

    	if (!validateInput(requestParams)) {
			throw new JSONException("RouterUploadAgent: Input parameters not valid.\n");
		}
    	
    	String body = MiscUtil.optNullKey(requestParams,JPSConstants.CONTENT);
    	JSONArray array = new JSONArray(body);  
    	
    	//providing a storerouter endpoint is optional 
    	String routerEndpoint = MiscUtil.optNullKey(requestParams,"routerEndpoint");
    	if(routerEndpoint == null) {
    		routerEndpoint = StoreRouter.storeRouterEndpoint;    		
    	}
    	StoreClientInterface storeClient = getStoreClient(routerEndpoint);
    	
    	LOGGER.info("Uploading to: "+routerEndpoint);
    	int nUploaded = uploadTriples(array, storeClient);
        
        return new JSONObject().put(JPSConstants.RESULT_KEY, Integer.toString(nUploaded)+" endpoint(s) uploaded.");
    }
    
    public StoreClientInterface getStoreClient(String endpoint) {
    	return new RemoteStoreClient(endpoint, endpoint);
    }
    
    public int uploadTriples(JSONArray array, StoreClientInterface storeClient) {
    	RouterUploadTool uploaderTool = new RouterUploadTool();
    	return uploaderTool.uploadRoutingData(array, storeClient);
    }
    
    @Override
	public boolean validateInput(JSONObject requestParams) throws BadRequestException {	    
		
	    if (requestParams.isEmpty()) {
	        throw new BadRequestException();
	    }
	    try {
	    	
	    	//POST only
	    	String method = MiscUtil.optNullKey(requestParams,JPSConstants.METHOD);
	        if (!method.equals(HttpPost.METHOD_NAME)) {
        		LOGGER.error("Method is: "+method+". Only "+HttpPost.METHOD_NAME+" accepted.");
        		return false;
        	}
	    	
	        //JSONObject
	        String body = MiscUtil.optNullKey(requestParams,JPSConstants.CONTENT);
	        JSONArray array = null;
	        try {
	        	array = new JSONArray(body);
	        }catch(JSONException ex) {
	        	LOGGER.error("Content is not a JSONArray.");
	        	return false;
	        }
	    	
	        int size = array.length();
	        if (size < 1) {return false;}
	        
	    	for (int i = 0; i < size; i++) {
	    		JSONObject obj = array.getJSONObject(i); 
	        	if(!obj.has(RouterUploadTool.LABEL)) {
	        		LOGGER.error("LABEL missing");
	        		return false;
	        	}
	        	if(!obj.has(RouterUploadTool.QUERY_ENDPOINT)) {
	        		LOGGER.error("QUERY_ENDPOINT missing");
	        		return false;
	        	}
	        	if(!obj.has(RouterUploadTool.UPDATE_ENDPOINT)) {
	        		LOGGER.error("UPDATE_ENDPOINT missing");
	        		return false;
	        	}
	    	}
	    	
			return true;
	        
	    }catch (JSONException ex) {
	    	return false;
	    }
	}
    
    

}
