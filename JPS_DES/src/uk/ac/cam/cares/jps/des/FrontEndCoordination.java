package uk.ac.cam.cares.jps.des;

import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import org.json.JSONObject;
import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;

@WebServlet(urlPatterns = { "/showDESResult"})

public class FrontEndCoordination  extends JPSAgent{

	private static final long serialVersionUID = 1L;

	@Override
	public JSONObject processRequestParameters(JSONObject requestParams) {
	    requestParams = processRequestParameters(requestParams, null);
	    return requestParams;
	}
    @Override
    public JSONObject processRequestParameters(JSONObject requestParams,HttpServletRequest request) {
    	 	JSONObject responseParams = requestParams;
	    	String v = AgentCaller.executeGetWithJsonParameter("JPS_DES/GetBlock", requestParams.toString());
 			System.gc();
 			responseParams = new JSONObject(v);
 	    		 
 			
    	return responseParams;
    }
   
    

    
}
