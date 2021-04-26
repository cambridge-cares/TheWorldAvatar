package uk.ac.cam.cares.jps.ess;

import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.BadRequestException;

import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.util.InputValidator;

@WebServlet(urlPatterns = { "/LocateBattery" })
/** CoordinationAgent for selecting based on total power loss
 * executes ENAgent followed by Battery Entity Creator
 *
 */
public class BatteryLocator extends JPSAgent {
	
	private static final long serialVersionUID = 1L;

	@Override
    protected void setLogger() {
        logger = LoggerFactory.getLogger(BatteryLocator.class);
    }
    
    @Override
	public JSONObject processRequestParameters(JSONObject requestParams) {
	    if (!validateInput(requestParams)) {
			throw new BadRequestException("BatteryLocatorAgent: Input parameters not found.\n");
		}
    	// runs simulation with ENAgent
		AgentCaller.executeGetWithJsonParameter("JPS_POWSYS/ENAgent/startsimulationOPF", requestParams.toString());
    	//Creates battery instances using BatteryEntityCreator
		String resultStartLocator = AgentCaller.executeGetWithJsonParameter("JPS_ESS/CreateBattery", requestParams.toString());
    	//returns a list of battery instances IRI
		
		return new JSONObject(resultStartLocator);
    }
    
    @Override
    public boolean validateInput(JSONObject requestParams) throws BadRequestException {
    	if (requestParams.isEmpty()) {
            return false;
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
