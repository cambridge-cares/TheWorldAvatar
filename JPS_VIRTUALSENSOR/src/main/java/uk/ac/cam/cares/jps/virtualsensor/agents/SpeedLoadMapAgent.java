package uk.ac.cam.cares.jps.virtualsensor.agents;

import java.io.IOException;
import java.net.URISyntaxException;
import java.net.URL;
import javax.servlet.annotation.WebServlet;
import javax.ws.rs.BadRequestException;

import org.apache.http.client.methods.CloseableHttpResponse;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.utils.URIBuilder;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClients;
import org.apache.http.util.EntityUtils;
import org.json.JSONObject;
import org.json.JSONTokener;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.virtualsensor.objects.Ship;
import uk.ac.cam.cares.jps.virtualsensor.sparql.ShipSparql;

@WebServlet("/SpeedLoadMapAgent")
public class SpeedLoadMapAgent extends JPSAgent {
	/**
	 * Takes in one ship IRI as input, queries the ship properties (type and speed for now).
	 * These parameters are then passed to the speed load map agent to generate emissions.
	 * Reference to convert ship speed into engine speed:
	 * http://betterboat.com/average-boat-speed/ assume fastest medium boat 
     * max speed= 25knot max rpm= 2500 rpm torque=constant=250Nm then 1knot=100 rpm rpm=
	 * https://www.marineinsight.com/shipping-news/worlds-fastest-ship-built-tasmania-christened-argentinas-president/->fastest=58.1 knot
	 * knot*2500/58.1 roughly 1 ship 33 kg/h 1 boat= 1.1338650741577147e-05*3600 = 0.041
	 * kg/h NO2 (comparison of NO2
	 * https://pdfs.semanticscholar.org/1bd2/52f2ae1ede131d0ef84ee21c84a73fb6b374.pdf) 
	 * 1 boat mass flux=0.0192143028723584 kg/s 
	 */
	private Logger logger = LogManager.getLogger(SpeedLoadMapAgent.class);
	@Override
    public JSONObject processRequestParameters(JSONObject requestParams) {
		JSONObject response = new JSONObject();
		
		if (validateInput(requestParams)) {
			Ship ship = new Ship(requestParams.getString(ShipSparql.shipKey),false);

			double valuecalc=ship.getSpeed()*2500/58.1;
			if(valuecalc>2500) {
				valuecalc=2500;
			}
			String type=ship.getType().toLowerCase();
			
			JSONObject in= new JSONObject();
			JSONObject speedob= new JSONObject();		
			speedob.put("value", valuecalc); //600-2500
			speedob.put("unit", "RPM");
			JSONObject torob= new JSONObject();
			torob.put("value", 250); //50-550 range
			torob.put("unit", "Nm");
			in.put("speed", speedob);
			in.put("torque", torob);
	
		    response = crankUpRealShipModel(type, getSurogateValues(valuecalc, 250));
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
	
	private JSONObject getSurogateValues(double speed_rpm, double torque_nm) {
		JSONObject result;
		try {
			URIBuilder url = new URIBuilder("http://python-service:5000/getEmissions");
			url.addParameter("speed", String.valueOf(speed_rpm));
			url.addParameter("torque", String.valueOf(torque_nm));

			HttpGet httpGet = new HttpGet(url.build());
			CloseableHttpClient httpclient = HttpClients.createDefault();
			CloseableHttpResponse pyresponse = httpclient.execute(httpGet);
			result = new JSONObject(new JSONTokener(pyresponse.getEntity().getContent()));
		} catch (URISyntaxException | IOException e) {
			logger.error(e.getMessage());
			throw new JPSRuntimeException(e);
		}

		return result;
	}
	
	private JSONObject crankUpRealShipModel(String type, JSONObject json) {
		// these scaling factors are purely to make the results fall within the reasonable range
		for(int gas=0;gas<json.getJSONArray("pollutants").length();gas++) {
			JSONObject pollutantmass=json.getJSONArray("pollutants").getJSONObject(gas);
			Double oldvaluemixmass= pollutantmass.getDouble("value");
			if(type.contains("cargo")) {
				pollutantmass.put("value",oldvaluemixmass*322);	
			}
			else if(type.contains("tanker")) {
				pollutantmass.put("value",oldvaluemixmass*430);	
				
			}
			else if(type.contains("container")) {
				pollutantmass.put("value",oldvaluemixmass*580);	
			}
			else if(type.contains("passenger")) {
				pollutantmass.put("value",oldvaluemixmass*697);	
			}
			else  {
				pollutantmass.put("value",oldvaluemixmass*300);	
			}
		}
		
		
		
		for(int part=0;part<json.getJSONArray("particle").length();part++) {
			JSONObject particlemass=json.getJSONArray("particle").getJSONObject(part).getJSONObject("emission_rate");
			Double oldvaluemixmass= particlemass.getDouble("value");
			JSONObject particleD=json.getJSONArray("particle").getJSONObject(part).getJSONObject("diameter");
			Double oldvaluemixD= particleD.getDouble("value");
			particleD.put("value",(double)Math.round(oldvaluemixD * 1000d) / 1000d);
			if(type.contains("cargo")) {
				
				particlemass.put("value",oldvaluemixmass*322);
			}
			else if(type.contains("tanker")) {
				particlemass.put("value",oldvaluemixmass*430);	
				
			}
			else if(type.contains("container")) {
				particlemass.put("value",oldvaluemixmass*580);	
			}
			else if(type.contains("passenger")) {
				particlemass.put("value",oldvaluemixmass*697);	
			}
			else  {
				particlemass.put("value",oldvaluemixmass*300);	
			}
		}
		
		
		
		JSONObject mixturemass=json.getJSONObject("mixture").getJSONObject("massflux");
		Double oldvaluemixmass= mixturemass.getDouble("value");
		if(type.contains("cargo")) {
			mixturemass.put("value",oldvaluemixmass*322);	
		}
		else if(type.contains("tanker")) {
			mixturemass.put("value",oldvaluemixmass*430);	
			
		}
		else if(type.contains("container")) {
			mixturemass.put("value",oldvaluemixmass*580);	
		}
		else if(type.contains("passenger")) {
			mixturemass.put("value",oldvaluemixmass*697);	
		}
		else  {
			mixturemass.put("value",oldvaluemixmass*300);	
		}
		return json;
	}
}
