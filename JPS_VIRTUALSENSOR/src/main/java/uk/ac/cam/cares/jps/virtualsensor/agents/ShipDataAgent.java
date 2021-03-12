package uk.ac.cam.cares.jps.virtualsensor.agents;

import java.net.URL;

import javax.servlet.annotation.WebServlet;
import javax.ws.rs.BadRequestException;

import org.json.JSONArray;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.region.Scope;
import uk.ac.cam.cares.jps.virtualsensor.sparql.DispSimSparql;
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
    		String sim_iri = requestParams.getString(DispSimSparql.SimKey);
	        Scope sc = DispSimSparql.GetScope(sim_iri);
	        String[] shipIRI = ShipSparql.GetShipIriWithinScope(sc);
	        DispSimSparql.AddEmissionSources(sim_iri, shipIRI);
	        response.put("shipDataAgent", "success");
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
}
