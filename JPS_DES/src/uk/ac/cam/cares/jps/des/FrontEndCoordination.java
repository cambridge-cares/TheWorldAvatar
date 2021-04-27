package uk.ac.cam.cares.jps.des;

import javax.servlet.annotation.WebServlet;
import javax.ws.rs.BadRequestException;

import org.json.JSONObject;
import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;

@WebServlet(urlPatterns = { "/showDESResult"})

public class FrontEndCoordination  extends JPSAgent{

	private static final long serialVersionUID = 1L;

	@Override
	public JSONObject processRequestParameters(JSONObject requestParams) {
		if (!validateInput(requestParams)) {
			throw new BadRequestException("FrontEndCoordination: Input parameters are non-empty.\n");
		}
    	JSONObject responseParams = requestParams;
    	String v = AgentCaller.executeGetWithJsonParameter("JPS_DES/GetBlock", requestParams.toString());
		responseParams = new JSONObject(v);		
    	return responseParams;
    }
    
	@Override
    public boolean validateInput(JSONObject requestParams) throws BadRequestException {
		if (!requestParams.isEmpty()) {
            return true;
        }//Even if there are no resources available here, key values are sent
		//via AgentCaller/put in requestURL, path and so on. 
        return false;
	}
   
    

    
}
