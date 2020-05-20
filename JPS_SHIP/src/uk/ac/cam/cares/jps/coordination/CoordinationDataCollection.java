package uk.ac.cam.cares.jps.coordination;

import java.io.IOException;

import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.scenario.BucketHelper;
import uk.ac.cam.cares.jps.base.scenario.JPSContext;
import uk.ac.cam.cares.jps.ship.HKUPollutionRetriever;
import uk.ac.cam.cares.jps.ship.HKUWeatherRetriever;

@WebServlet("/CollectorCoordination")
public class CoordinationDataCollection extends HttpServlet {
	private static final long serialVersionUID = 1L;
		
	public void executeSGData(JSONObject jo){
		JSONObject upcorn = new JSONObject();
		upcorn.put("upperx", "11564077.989");
		upcorn.put("uppery", "143305.896");
		JSONObject lowcorn = new JSONObject();
		lowcorn.put("lowerx", "11560879.832");
		lowcorn.put("lowery", "140107.739");
		JSONObject joregion = new JSONObject();
		joregion.put("srsname","EPSG:3857");
		joregion.put("lowercorner",lowcorn);
		joregion.put("uppercorner",upcorn);
		jo.put("region", joregion);
		

		
		//SG episode
//		JSONObject upcorn = new JSONObject();
//		upcorn.put("upperx", "11572101.89");
//		upcorn.put("uppery", "151860.32");
//		JSONObject lowcorn = new JSONObject();
//		lowcorn.put("lowerx", "11552101.832");
//		lowcorn.put("lowery", "131707.739");
//		JSONObject joregion = new JSONObject();
//		joregion.put("srsname","EPSG:3857");
//		joregion.put("lowercorner",lowcorn);
//		joregion.put("uppercorner",upcorn);
		callAgent(jo);
	}
	
	public void executeHKData(JSONObject jo){
		//HK episode
//		JSONObject upcorn = new JSONObject();
//		upcorn.put("upperx", "12720578.56");
//		upcorn.put("uppery", "2562555.26");
//		JSONObject lowcorn = new JSONObject();
//		lowcorn.put("lowerx", "12694101.21");
//		lowcorn.put("lowery", "2534900.06");
//		JSONObject joregion = new JSONObject();
//		joregion.put("srsname","EPSG:3857");
//		joregion.put("lowercorner",lowcorn);
//		joregion.put("uppercorner",upcorn);
//		jo.put("region", joregion);
	
	
		callAgent(jo);
	}

	
	public void callAgent(JSONObject jo) {
		//jo.put("agent", "http://www.theworldavatar.com/kb/agents/Service__ComposedADMS.owl#Service");
		jo.put("agent", "http://www.theworldavatar.com/kb/agents/Service__ADMS.owl#Service");
//		jo.put("agent", "http://www.theworldavatar.com/kb/agents/Service__Episode.owl#Service");
//		jo.put("reactionmechanism", "http://www.theworldavatar.com/kb/ontokin/Reduced_PRF_ERC_particle.owl#ReactionMechanism_184144363244001");
		jo.put("reactionmechanism", "none");
		jo.put("airStationIRI","http://www.theworldavatar.com/kb/sgp/singapore/AirQualityStation-001.owl#AirQualityStation-001");
//		AgentCaller.executeGetWithJsonParameter("JPS_SHIP/ADMSCoordinationAgentForShipWithoutComposition",jo.toString());
		AgentCaller.executeGetWithJsonParameter("JPS_DISPERSION/DMSCoordinationAgent",jo.toString());
	}

	protected void doGet(HttpServletRequest req, HttpServletResponse res) throws IOException {
		
		JSONObject jo = new JSONObject();
		
		JSONObject inputjo = AgentCaller.readJsonParameter(req);
		String scenarioUrl = null;
		String scenarioName = inputjo.optString("scenarioname");
		if ((scenarioName != null) && !scenarioName.isEmpty()) {
			scenarioUrl = BucketHelper.getScenarioUrl(scenarioName);
			JPSContext.putScenarioUrl(jo, scenarioUrl);
		}
		
		System.out.println("CoordinationDataCollection is called with scenarioUrl = " + scenarioUrl);
		
		
		new HKUWeatherRetriever().readWritedata();
		System.out.println(" finished reading writing data weather");
		new HKUPollutionRetriever().readWritedata();
		System.out.println(" finished reading writing airpollution weather");

		
		//retrieveShipdata();
		executeSGData(jo);
		
		//executeHKData(jo);

		
		System.out.println("it is executed");
	}
	
	

}
