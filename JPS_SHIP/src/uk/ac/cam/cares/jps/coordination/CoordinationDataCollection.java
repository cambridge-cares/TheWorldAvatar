package uk.ac.cam.cares.jps.coordination;

import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.ship.HKUWeatherRetriever;

public class CoordinationDataCollection {
	
	public static void retrieveHKWeather () {
		//JSONObject jo = new JSONObject();
		//jo.put("electricalnetwork", ELECTRICAL_NETWORK);
		//String resultStart = AgentCaller.executeGetWithJsonParameter("JPS_HKUWeatherAgent/getdata", "no parameter needed");
		
		HKUWeatherRetriever.readWritedata();
		System.out.println(" finished reading writing data");
	}
	
	public static void retrieveHKPollution () {
		//JSONObject jo = new JSONObject();
		//jo.put("electricalnetwork", ELECTRICAL_NETWORK);
		//String resultStart = AgentCaller.executeGetWithJsonParameter("JPS_HKUWeatherAgent/getdata", "no parameter needed");
		
		//HKUPollutionRetriever.readWritedata();
		System.out.println(" finished reading writing data");
	}
	
	public static void main (String args[]) {
		
		retrieveHKWeather();
		
		//retrieveHKPollution();
		
		//retrieveShipdata();
		
		JSONObject jo = new JSONObject();
		
		JSONObject upcorn = new JSONObject();
		upcorn.put("upperx", "12708200.45");
		upcorn.put("uppery", "2546850.028");
		JSONObject lowcorn = new JSONObject();
		lowcorn.put("lowerx", "12706630.262");
		lowcorn.put("lowery", "2545539.172");
		JSONObject joregion = new JSONObject();
		joregion.put("srsname","EPSG:3857");
		joregion.put("lowercorner",lowcorn);
		joregion.put("uppercorner",upcorn);
		
		
		
		jo.put("agent", "http://www.theworldavatar.com/kb/agents/Service__ComposedADMS.owl#Service");
		jo.put("region", joregion);
		jo.put("location", "Hong Kong");
		jo.put("reactionmechanism", "http://www.theworldavatar.com/kb/ontokin/Reduced_PRF_ERC_particle.owl#ReactionMechanism_184144363244001");
		
		String resultStart = AgentCaller.executeGetWithJsonParameter("JPS_SHIP/ADMSCoordinationAgentForShipWithoutComposition",jo.toString());
		System.out.println("it is executed");
	}
	

}
