package uk.ac.cam.cares.jps.coordination;

import java.io.IOException;
import java.util.concurrent.ExecutionException;

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
		
	public JSONObject executeSGDataADMS(JSONObject jo) throws ExecutionException, InterruptedException {
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
		jo.put("agent", "http://www.theworldavatar.com/kb/agents/Service__ADMS.owl#Service");
		jo.put("airStationIRI","http://www.theworldavatar.com/kb/sgp/singapore/AirQualityStation-001.owl#AirQualityStation-001");

		return jo;
	}
	public JSONObject executeSGDataEPISODE(JSONObject jo) throws ExecutionException, InterruptedException {
	
		//SG episode
		JSONObject upcorn = new JSONObject();
		upcorn.put("upperx", "11572101.89");
		upcorn.put("uppery", "151860.32");
		JSONObject lowcorn = new JSONObject();
		lowcorn.put("lowerx", "11552101.832");
		lowcorn.put("lowery", "131707.739");
		JSONObject joregion = new JSONObject();
		joregion.put("srsname","EPSG:3857");
		joregion.put("lowercorner",lowcorn);
		joregion.put("uppercorner",upcorn);
		jo.put("region", joregion);
		jo.put("agent", "http://www.theworldavatar.com/kb/agents/Service__Episode.owl#Service");
		jo.put("airStationIRI","http://www.theworldavatar.com/kb/sgp/singapore/AirQualityStation-002.owl#AirQualityStation-002");
		return jo;
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
	
	
//		callAgent(jo);
	}
	
	public JSONObject executeHKDataADMS(JSONObject jo) throws ExecutionException, InterruptedException {
		JSONObject upcorn = new JSONObject();
//		upcorn.put("upperx", "12708579.81");
//		upcorn.put("uppery", "2547126.72");
//		JSONObject lowcorn = new JSONObject();
//		lowcorn.put("lowerx", "12706653.262");
//		lowcorn.put("lowery", "2545200.172");
		upcorn.put("upperx", "12711879.81");
		upcorn.put("uppery", "2550426.72");
		JSONObject lowcorn = new JSONObject();
		lowcorn.put("lowerx", "12706653.262");
		lowcorn.put("lowery", "2545200.172");
		JSONObject joregion = new JSONObject();
		joregion.put("srsname","EPSG:3857");
		joregion.put("lowercorner",lowcorn);
		joregion.put("uppercorner",upcorn);
		jo.put("region", joregion);
		jo.put("agent", "http://www.theworldavatar.com/kb/agents/Service__ADMS.owl#Service");
		jo.put("airStationIRI","http://www.theworldavatar.com/kb/hkg/hongkong/AirQualityStation-001.owl#AirQualityStation-001");

		return jo;
	}
	public JSONObject executeHKDataEPISODE(JSONObject jo) throws ExecutionException, InterruptedException {

		JSONObject upcorn = new JSONObject();
		upcorn.put("upperx", "12720578.56");
		upcorn.put("uppery", "2562555.26");
		JSONObject lowcorn = new JSONObject();
		lowcorn.put("lowerx", "12694101.21");
		lowcorn.put("lowery", "2534900.06");
		JSONObject joregion = new JSONObject();
		joregion.put("srsname","EPSG:3857");
		joregion.put("lowercorner",lowcorn);
		joregion.put("uppercorner",upcorn);
		jo.put("region", joregion);
		jo.put("agent", "http://www.theworldavatar.com/kb/agents/Service__Episode.owl#Service");
		jo.put("airStationIRI","http://www.theworldavatar.com/kb/hkg/hongkong/AirQualityStation-002.owl#AirQualityStation-002");
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
			JSONObject episode=executeSGDataEPISODE(jo2);
			JSONObject adms=executeSGDataADMS(jo);
//			JSONObject episodeHK=executeHKDataEPISODE(jo4);
//			JSONObject admsHK=executeHKDataADMS(jo3);
			
			callAgent(adms,episode);
//			callAgent(admsHK,episodeHK);
		} catch (Exception e) {
			e.printStackTrace();
		}
		//executeHKData(jo);

		
		System.out.println("it is executed");
	}
	
	

}
