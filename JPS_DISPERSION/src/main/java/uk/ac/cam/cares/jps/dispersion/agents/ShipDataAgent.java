package uk.ac.cam.cares.jps.dispersion.agents;

import javax.servlet.annotation.WebServlet;
import javax.ws.rs.BadRequestException;

import org.json.JSONArray;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.region.Scope;
import uk.ac.cam.cares.jps.dispersion.sparql.ShipSparql;

@WebServlet(urlPatterns = {"/ShipDataAgent"})
public class ShipDataAgent extends JPSAgent {
    /**
     * 
     */
    private static final long serialVersionUID = 1L;
    private static final String keyCollection = "collection";
    private static final String keyItems = "items";
    
    @Override
    public JSONObject processRequestParameters(JSONObject requestParams) {
    	JSONObject response = new JSONObject();
    	if (validateInput(requestParams)) {
	        Scope sc = new Scope(requestParams);
	        ShipSparql ss = new ShipSparql();
	        JSONArray result = ss.queryShipWithinScope(sc);
	        JSONObject items = new JSONObject();
	        items.put(keyItems, result);
	        response.put(keyCollection, items);
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
