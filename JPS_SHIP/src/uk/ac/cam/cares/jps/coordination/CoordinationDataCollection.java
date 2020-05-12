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
		upcorn.put("upperx", "11563323.926");
		upcorn.put("uppery", "143305.896");
		JSONObject lowcorn = new JSONObject();
		lowcorn.put("lowerx", "11560879.832");
		lowcorn.put("lowery", "140107.739");
		JSONObject joregion = new JSONObject();
		joregion.put("srsname","EPSG:3857");
		joregion.put("lowercorner",lowcorn);
		joregion.put("uppercorner",upcorn);
		jo.put("region", joregion);
		//jo.put("location", "Singapore");
//		jo.put("agent","http://www.theworldavatar.com/kb/agents/Service__ComposedADMS.owl#Service");
		
		callAgent(jo);
	}
	
	public void executeHKData(JSONObject jo){
		JSONObject upcorn = new JSONObject();
		upcorn.put("upperx", "12720831.57");
		upcorn.put("uppery", "2562311.02");
		JSONObject lowcorn = new JSONObject();
		lowcorn.put("lowerx", "12693826.33");
		lowcorn.put("lowery", "2535141.08");
		JSONObject joregion = new JSONObject();
		joregion.put("srsname","EPSG:3857");
		joregion.put("lowercorner",lowcorn);
		joregion.put("uppercorner",upcorn);
		jo.put("region", joregion);
		//jo.put("location", "Hong Kong");
//		jo.put("agent","http://www.theworldavatar.com/kb/agents/Service__ComposedADMS.owl#Service");
		
		callAgent(jo);
	}

	
	public void callAgent(JSONObject jo) {
		//jo.put("agent", "http://www.theworldavatar.com/kb/agents/Service__ComposedADMS.owl#Service");
		jo.put("agent", "http://www.theworldavatar.com/kb/agents/Service__ADMS.owl#Service");
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
		
		executeHKData(jo);

		
		System.out.println("it is executed");
	}
	
	

}
