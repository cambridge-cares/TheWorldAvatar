package uk.ac.cam.cares.jps.virtualsensor.agents;

import javax.servlet.annotation.WebServlet;
import javax.ws.rs.BadRequestException;

import org.json.JSONArray;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.region.Scope;
import uk.ac.cam.cares.jps.virtualsensor.sparql.ShipSparql;

@WebServlet(urlPatterns = {"/ShipDataAgent"})
public class ShipDataAgent extends JPSAgent {
    /**
     * 
     */
    private static final long serialVersionUID = 1L;
    
    @Override
    public JSONObject processRequestParameters(JSONObject requestParams) {
    	JSONObject response = new JSONObject();
    	if (validateInput(requestParams)) {
	        Scope sc = new Scope(requestParams);
	        response = ShipSparql.GetShipIriWithinScope(sc);
    	}
        return response;
    }

    @Override
    public boolean validateInput(JSONObject requestParams) {
    	boolean valid = false;
    	try {
    		new Scope(requestParams);
    		valid = true;
    	} catch (Exception e) {
    		throw new BadRequestException();
    	}
    	return valid;
    }
}
