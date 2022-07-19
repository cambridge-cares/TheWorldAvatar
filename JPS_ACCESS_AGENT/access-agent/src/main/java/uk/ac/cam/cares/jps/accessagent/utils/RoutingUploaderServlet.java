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
@WebServlet(urlPatterns = {RoutingUploaderServlet.UPLOAD_URL})
public class RoutingUploaderServlet  extends JPSAgent{

	private static final long serialVersionUID = 1L;

	public static final String UPLOAD_URL = "/upload";
	
	/**
     * Logger for error output.
     */
    private static final Logger LOGGER = LogManager.getLogger(RoutingUploaderServlet.class);
	    
    @Override
    public JSONObject processRequestParameters(JSONObject requestParams) {

    	validateInput(requestParams);
    	
    	JSONArray array = requestParams.getJSONArray(JPSConstants.CONTENT);  
    	
    	//providing a storerouter endpoint is optional 
    	String routerEndpoint = MiscUtil.optNullKey(requestParams,"routerEndpoint");
    	if(routerEndpoint == null) {
    		routerEndpoint = StoreRouter.STOREROUTER_ENDPOINT;    		
    	}
    	RemoteStoreClient storeClient = new RemoteStoreClient(routerEndpoint,routerEndpoint);   	  	    	
    	
    	LOGGER.info("Uploading to: "+routerEndpoint);
    	RoutingUploaderTool uploaderTool = new RoutingUploaderTool();
    	int nUploaded = uploaderTool.uploadRoutingData(array, storeClient);
        
        return new JSONObject().put("Result:", Integer.toString(nUploaded)+" endpoint(s) uploaded.");
    }
    
    @Override
	public boolean validateInput(JSONObject requestParams) throws BadRequestException {	    
		
	    if (requestParams.isEmpty()) {
	        throw new BadRequestException();
	    }
	    try {
	    	
	    	//POST only
	    	String method = MiscUtil.optNullKey(requestParams,JPSConstants.METHOD);
	        if (method != HttpPost.METHOD_NAME) {
        		LOGGER.error("Not HTTP POST");
        		return false;
        	}
	    	
	        //JSONObject
	        JSONArray array = requestParams.getJSONArray(JPSConstants.CONTENT);    	    	
	    	
	        int size = array.length();
	        if (size < 1) {return false;}
	        
	    	for (int i = 0; i < size; i++) {
	    		JSONObject obj = array.getJSONObject(i); 
	        	if(!obj.has(RoutingUploaderTool.LABEL)) {
	        		LOGGER.error("LABEL missing");
	        		return false;
	        	}
	        	if(!obj.has(RoutingUploaderTool.QUERY_ENDPOINT)) {
	        		LOGGER.error("QUERY_ENDPOINT missing");
	        		return false;
	        	}
	        	if(!obj.has(RoutingUploaderTool.UPDATE_ENDPOINT)) {
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
