package uk.ac.cam.cares.jps.virtualsensor.agents;

import java.net.URL;

import javax.servlet.annotation.WebServlet;
import javax.ws.rs.BadRequestException;

import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.virtualsensor.objects.Chimney;
import uk.ac.cam.cares.jps.virtualsensor.sparql.ShipSparql;

@WebServlet("/ShipAgent")
public class ShipAgent extends JPSAgent{
	@Override
    public JSONObject processRequestParameters(JSONObject requestParams) {
		JSONObject response = null;
		
		if (validateInput(requestParams)) {
			// only call SpeedLoadMapAgent, may extend to include SRM agent in the future
			JSONObject result= new JSONObject(AgentCaller.executeGetWithJsonParameter("JPS_VIRTUALSENSOR/SpeedLoadMapAgent", requestParams.toString()));
		    Chimney chim = new Chimney(result);
		    ShipSparql.UpdateShipChimney(requestParams.getString(ShipSparql.shipKey), chim);
		    response = requestParams;
		}
		return response;
	}
	
	@Override
	public boolean validateInput(JSONObject requestParams) {
    	boolean valid = false;
    	try {
    		// ensure ship IRI is valid
    		new URL(requestParams.getString(ShipSparql.shipKey)).toURI();
    		valid = true;
    	} catch (Exception e) {
    		throw new BadRequestException(e);
    	}
    	return valid;
    }
}
