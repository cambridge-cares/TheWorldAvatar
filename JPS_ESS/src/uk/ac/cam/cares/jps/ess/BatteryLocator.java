package uk.ac.cam.cares.jps.ess;

import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.BadRequestException;

import org.json.JSONException;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet;
import uk.ac.cam.cares.jps.base.util.InputValidator;

@WebServlet(urlPatterns = { "/LocateBattery" })
public class BatteryLocator extends JPSAgent {
	
    @Override
    protected void setLogger() {
        logger = LoggerFactory.getLogger(BatteryLocator.class);
    }
    Logger logger = LoggerFactory.getLogger(BatteryLocator.class);
    
    @Override
	public JSONObject processRequestParameters(JSONObject requestParams) {
	    requestParams = processRequestParameters(requestParams, null);
	    return requestParams;
	}
    @Override
   	public JSONObject processRequestParameters(JSONObject requestParams, HttpServletRequest request) {
    	if (!validateInput(requestParams)) {
			throw new JSONException ("BatteryLocatorAgent: Input parameters not found.\n");
		}
    	
		//run the opf
		String result = AgentCaller.executeGetWithJsonParameter("JPS_POWSYS/ENAgent/startsimulationOPF", requestParams.toString());
    	
		String resultStartLocator = AgentCaller.executeGetWithJsonParameter("JPS_ESS/CreateBattery", requestParams.toString());
    	
		
		return new JSONObject(resultStartLocator);
    }
    @Override
    public boolean validateInput(JSONObject requestParams) throws BadRequestException {
    	if (requestParams.isEmpty()) {
            throw new BadRequestException();
        }
        try {
	        String storageFormat = requestParams.getString("storage");
	        boolean q = InputValidator.checkIfValidIRI(storageFormat);
	        String ENIRI = requestParams.getString("electricalnetwork");
	        boolean v = InputValidator.checkIfValidIRI(ENIRI);
	        
	        return q&v;
        }catch (Exception ex) {
        	return false;
        }
    }
}
