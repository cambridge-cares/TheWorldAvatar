package uk.ac.cam.cares.jps.coordination;

import java.io.IOException;
import java.util.concurrent.ExecutionException;

import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.region.Region;
import uk.ac.cam.cares.jps.base.scenario.BucketHelper;
import uk.ac.cam.cares.jps.base.scenario.JPSContext;
import uk.ac.cam.cares.jps.ship.HKUPollutionRetriever;
import uk.ac.cam.cares.jps.ship.HKUWeatherRetriever;

@WebServlet("/CollectorCoordination")
public class CoordinationDataCollection extends HttpServlet {
	private static final long serialVersionUID = 1L;
		
	public JSONObject executeSGDataADMS(JSONObject jo) {
		Region.putRegionAndStation(jo, 1);
		jo.put("agent", "http://www.theworldavatar.com/kb/agents/Service__ADMS.owl#Service");
		return jo;
	}
	public JSONObject executeSGDataEPISODE(JSONObject jo) {
		Region.putRegionAndStation(jo, 2);
		jo.put("agent", "http://www.theworldavatar.com/kb/agents/Service__Episode.owl#Service");
		return jo;
	}
	
	public JSONObject executeHKDataADMS(JSONObject jo) {
		Region.putRegionAndStation(jo, 3);
		jo.put("agent", "http://www.theworldavatar.com/kb/agents/Service__ADMS.owl#Service");
		return jo;
	}
	public JSONObject executeHKDataEPISODE(JSONObject jo) throws ExecutionException, InterruptedException {
		Region.putRegionAndStation(jo, 4);
		jo.put("agent", "http://www.theworldavatar.com/kb/agents/Service__Episode.owl#Service");
		return jo;
	}

	
	public void callAgent(JSONObject jo,JSONObject jo2) throws ExecutionException, InterruptedException {
		//jo.put("agent", "http://www.theworldavatar.com/kb/agents/Service__ComposedADMS.owl#Service");
		

//		jo.put("reactionmechanism", "http://www.theworldavatar.com/kb/ontokin/Reduced_PRF_ERC_particle.owl#ReactionMechanism_184144363244001");
		jo.put("reactionmechanism", "none");
		jo2.put("reactionmechanism", "none");

//		AgentCaller.executeGetWithJsonParameter("JPS_SHIP/ADMSCoordinationAgentForShipWithoutComposition",jo.toString());
		AgentCaller.executeGetWithJsonParameter("JPS_DISPERSION/episode/dispersion/coordination",jo2.toString());
		AgentCaller.executeGetWithJsonParameter("JPS_DISPERSION/adms/dispersion/coordination",jo.toString());
		//@TODO Make it separate threads
		/*sample code:*/
//		CompletableFuture<String> asyncEpisode = CompletableFuture.supplyAsync(() ->
//				AgentCaller.executeGetWithJsonParameter("JPS_DISPERSION/episode/dispersion/coordination",jo2.toString()));
//		CompletableFuture<String> asyncAdms = CompletableFuture.supplyAsync(() ->
//				AgentCaller.executeGetWithJsonParameter("JPS_DISPERSION/adms/dispersion/coordination",jo.toString()));
//		asyncEpisode.get();
//		asyncAdms.get();

	}

	public void callAgent(JSONObject jo) {
		// This is only used in CMCL
		jo.put("reactionmechanism", "none");
		AgentCaller.executeGetWithJsonParameter("JPS_DISPERSION/episode/dispersion/coordination",jo.toString());
	}
	
	protected void doGet(HttpServletRequest req, HttpServletResponse res) throws IOException {
		
		JSONObject jo = new JSONObject();
		JSONObject jo2 = new JSONObject();
		JSONObject jo3 = new JSONObject();
		JSONObject jo4 = new JSONObject();
		JSONObject inputjo = AgentCaller.readJsonParameter(req);
		String scenarioUrl = null;
		String scenarioName = inputjo.optString("scenarioname");
		if ((scenarioName != null) && !scenarioName.isEmpty()) {
			scenarioUrl = BucketHelper.getScenarioUrl(scenarioName);
			JPSContext.putScenarioUrl(jo, scenarioUrl);
		}
		
		System.out.println("CoordinationDataCollection is called with scenarioUrl = " + scenarioUrl);
		
		
//		new HKUWeatherRetriever().readWritedata();
//		System.out.println(" finished reading writing data weather");
//		new HKUPollutionRetriever().readWritedata();
//		System.out.println(" finished reading writing airpollution weather");

		
		//retrieveShipdata();
		try {
			if (AgentLocator.isJPSRunningAtCMCL()) {
				JSONObject episode=executeSGDataEPISODE(jo2);
				callAgent(episode);
			} else {
				JSONObject episode=executeSGDataEPISODE(jo2);
				JSONObject adms=executeSGDataADMS(jo);
	//			JSONObject episodeHK=executeHKDataEPISODE(jo4);
	//			JSONObject admsHK=executeHKDataADMS(jo3);
				
				callAgent(adms,episode);
	//			callAgent(admsHK,episodeHK);
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
		//executeHKData(jo);

		
		System.out.println("it is executed");
	}
	
	

}
