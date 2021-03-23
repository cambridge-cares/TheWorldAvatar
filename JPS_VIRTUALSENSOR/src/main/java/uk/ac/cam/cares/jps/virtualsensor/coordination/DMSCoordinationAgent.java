package uk.ac.cam.cares.jps.virtualsensor.coordination;

import java.net.URL;
import java.util.concurrent.CompletableFuture;

import javax.servlet.annotation.WebServlet;
import javax.ws.rs.BadRequestException;

import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.virtualsensor.sparql.DispSimSparql;
import uk.ac.cam.cares.jps.virtualsensor.sparql.ShipSparql;

@WebServlet("/DMSCoordinationAgent")
public class DMSCoordinationAgent extends JPSAgent {

	Logger logger = LoggerFactory.getLogger(DMSCoordinationAgent.class);
	
	@Override
	public JSONObject processRequestParameters(JSONObject requestParams) {
		JSONObject response = new JSONObject();
		
		if (validateInput(requestParams)) {
			String sim_iri = requestParams.getString(DispSimSparql.SimKey);
			AgentCaller.executeGetWithJsonParameter("JPS_VIRTUALSENSOR/WeatherAgent", requestParams.toString());
	
			logger.info("calling ship data agent = " + requestParams.toString());
			AgentCaller.executeGetWithJsonParameter("JPS_VIRTUALSENSOR/ShipDataAgent", requestParams.toString());
	
			String[] shipIRI = DispSimSparql.GetEmissionSources(sim_iri);
			
			if (shipIRI.length != 0) {
				RunShipAsync(shipIRI);
				String result = AgentCaller.executeGetWithURLAndJSON(DispSimSparql.GetServiceURL(sim_iri), requestParams.toString());
				response.put("folder", new JSONObject(result).getString("folder"));
			}
		}
		return response;
	}
	
	@Override
    public boolean validateInput(JSONObject requestParams) {
    	boolean valid = false;
    	try {
    		new URL(requestParams.getString(DispSimSparql.SimKey)).toURI();
    		valid = true;
    	} catch (Exception e) {
    		throw new BadRequestException();
    	}
    	return valid;
    }
	
	private void RunShipAsync(String[] shipIRI) {
		CompletableFuture<String> getAsync = null;

		for (int i = 0; i < shipIRI.length; i++) {
			logger.info("Ship AGENT called: " + i);
			JSONObject shiprequest = new JSONObject();
			shiprequest.put(ShipSparql.shipKey, shipIRI[i]);

			getAsync = CompletableFuture
					.supplyAsync(() -> execute("/JPS_VIRTUALSENSOR/ShipAgent", shiprequest.toString()));
		}
        getAsync.join(); // ensure all calculations are completed before proceeding
	}
}
