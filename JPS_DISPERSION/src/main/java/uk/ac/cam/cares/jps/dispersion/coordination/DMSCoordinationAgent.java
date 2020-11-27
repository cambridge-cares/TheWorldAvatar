package uk.ac.cam.cares.jps.dispersion.coordination;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;

import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;

import org.apache.http.client.methods.HttpPost;
import org.json.JSONArray;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.config.IKeys;
import uk.ac.cam.cares.jps.base.config.KeyValueManager;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.JenaResultSetFormatter;
import uk.ac.cam.cares.jps.base.query.KnowledgeBaseClient;
import uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet;
import uk.ac.cam.cares.jps.dispersion.general.DispersionModellingAgent;

//@WebServlet("/DMSCoordinationAgent")
@WebServlet(urlPatterns = { "/episode/dispersion/coordination", "/adms/dispersion/coordination" })
public class DMSCoordinationAgent extends JPSHttpServlet {

	Logger logger = LoggerFactory.getLogger(DMSCoordinationAgent.class);
	private static final String PARAM_KEY_SHIP = "ship";
	public static final String DISPERSION_PATH = "/JPS_DISPERSION";
	public static final String EPISODE_PATH = "/episode/dispersion/coordination";
	public static final String ADMS_PATH = "/adms/dispersion/coordination";
	public static final String NO_CITY_FOUND_MSG = "No City found in Region.";
	
	public boolean validateWeatherInput(String context) {
		String sensorinfo = "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>"
				+ "PREFIX j4:<http://www.theworldavatar.com/ontology/ontosensor/OntoSensor.owl#>"
				+ "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/process_control_equipment/measuring_instrument.owl#>"
				+ "PREFIX j6:<http://www.w3.org/2006/time#>" + "SELECT ?class ?propval ?proptimeval " + "{ GRAPH <"
				+ context + "> " + "{ "

				+ "  ?entity j4:observes ?prop ." + " ?prop a ?class ." + " ?prop   j2:hasValue ?vprop ."
				+ " ?vprop   j2:numericalValue ?propval ." + " ?vprop   j6:hasTime ?proptime ."
				+ " ?proptime   j6:inXSDDateTime ?proptimeval ." + "}" + "}" + "ORDER BY DESC(?proptimeval)LIMIT 7";

		String dataseturl = KeyValueManager.get(IKeys.DATASET_WEATHER_URL);
		String resultfromrdf4j = KnowledgeBaseClient.query(dataseturl, null, sensorinfo);
		String[] keys = JenaResultSetFormatter.getKeys(resultfromrdf4j);
		List<String[]> listmap = JenaResultSetFormatter.convertToListofStringArrays(resultfromrdf4j, keys);
		String timestampInit = listmap.get(0)[2];
		for (int x = 0; x < listmap.size(); x++) {
			String timestamp = listmap.get(x)[2];
			if (!timestampInit.contentEquals(timestamp)) {
				return false;
			}
		}
		return true;
	}

	private JSONArray getNewWasteAsync(String reactionMechanism, JSONObject jsonShip) {
		JSONArray newwaste = new JSONArray();
		ArrayList<CompletableFuture> wastes = new ArrayList<>();
		JSONArray ships = jsonShip.getJSONObject("collection").getJSONArray("items");
		int sizeofshipselected = ships.length();

		for (int i = 0; i < sizeofshipselected; i++) {
			logger.info("Ship AGENT called: " + i);
			JSONObject jsonReactionShip = new JSONObject();
			jsonReactionShip.put("reactionmechanism", reactionMechanism);
			jsonReactionShip.put("ship", ships.getJSONObject(i));

			CompletableFuture<String> getAsync = CompletableFuture
					.supplyAsync(() -> execute("/JPS_SHIP/ShipAgent", jsonReactionShip.toString()));

			CompletableFuture<String> processAsync = getAsync
					.thenApply(wasteResult -> new JSONObject(wasteResult).getString("waste"));
			wastes.add(processAsync);
		}

		for (CompletableFuture waste : wastes) {
			try {
				newwaste.put(waste.get());
			} catch (InterruptedException | ExecutionException e) {
				throw new JPSRuntimeException(e.getMessage());
			}
		}

		return newwaste;
	}

	private String getCity(JSONObject requestParams) {
		String regionToCityResult = null;
		String city = null;

		if (requestParams != null) {
			JSONObject regionInfo = requestParams.getJSONObject("region");
			if (regionInfo != null & regionInfo.length() != 0) {
				JSONObject region = new JSONObject().put("region", regionInfo);
				regionToCityResult = execute("/JPS/RegionToCity", region.toString());
			}
		}

		if (!regionToCityResult.isEmpty()) {
			city = new JSONObject(regionToCityResult).getString("city");
		} else {
			throw new JPSRuntimeException(NO_CITY_FOUND_MSG);
		}
		return city;
	}
	
	@Override
	protected JSONObject processRequestParameters(JSONObject requestParams, HttpServletRequest request) {
		String path = request.getServletPath();
		String city = getCity(requestParams);

		requestParams.put("city", city);
		String result = execute("/JPS/GetBuildingListFromRegion", requestParams.toString());
		JSONArray building = new JSONObject(result).getJSONArray("building");
		requestParams.put("building", building);
		logger.info("building FROM COORDINATION AGENT: " + building.toString());

		// @TODO - improve weather update frequency
		result = execute("/JPS_DISPERSION/SensorWeatherAgent", requestParams.toString());
		JSONArray stationiri = new JSONObject(result).getJSONArray("stationiri");
		requestParams.put("stationiri", stationiri);

		if (city.toLowerCase().contains("kong") || city.toLowerCase().contains("singapore")) {

			String resultship;
			if (AgentLocator.isJPSRunningAtCMCL()) {
				logger.info("calling ship data agent = " + requestParams.getJSONObject("region").toString());
				resultship = AgentCaller.executeGetWithJsonParameter("JPS_SHIP/ShipDataAgent", 
						requestParams.getJSONObject("region").toString());
			} else {
//				=======================================================================
				logger.info("calling postgres= " + requestParams.toString());
				String url = KeyValueManager.get(IKeys.URL_POSITIONQUERY);
				url += "/getEntitiesWithinRegion";
				resultship = AgentCaller.executeGetWithURLAndJSON(url, requestParams.toString());
			}

			JSONObject jsonShip = new JSONObject(resultship);
			requestParams.put(PARAM_KEY_SHIP, jsonShip);

			if (((JSONArray) ((JSONObject) jsonShip.get("collection")).get("items")).length() != 0) {
				String reactionMechanism = requestParams.optString("reactionmechanism");
				JSONArray newwaste;

				newwaste = getNewWasteAsync(reactionMechanism, jsonShip);

				requestParams.put("waste", newwaste);

				// result = execute("/JPS_DISPERSION/DispersionModellingAgent",
				// requestParams.toString(), HttpPost.METHOD_NAME);

				String resultPath = DISPERSION_PATH;
				if (path.equals(ADMS_PATH) && validateWeatherInput(stationiri.getString(0))) {
					resultPath = resultPath + DispersionModellingAgent.ADMS_PATH;
					result = execute(resultPath, requestParams.toString(), HttpPost.METHOD_NAME);

					String folder = new JSONObject(result).getString("folder");
					requestParams.put("folder", folder);

					JSONObject newJo = new JSONObject();
					newJo.put("city", city);
					newJo.put("airStationIRI", requestParams.get("airStationIRI").toString());
					newJo.put("agent", requestParams.get("agent").toString());
					String interpolationcall = execute("/JPS_DISPERSION/InterpolationAgent/startSimulation",
							newJo.toString());

					String statisticcall = execute("/JPS_DISPERSION/StatisticAnalysis", newJo.toString());

				} else if (path.equals(EPISODE_PATH) && validateWeatherInput(stationiri.getString(0))
						&& validateWeatherInput(stationiri.getString(1))) {
					resultPath = resultPath + DispersionModellingAgent.EPISODE_PATH;
					result = execute(resultPath, requestParams.toString(), HttpPost.METHOD_NAME);

					String folder = new JSONObject(result).getString("folder");
					requestParams.put("folder", folder);

//					JSONObject newJo = new JSONObject();
//					newJo.put("city", city);
//					newJo.put("airStationIRI", requestParams.get("airStationIRI").toString());
//					newJo.put("agent", requestParams.get("agent").toString());
//					String interpolationcall = execute("/JPS_DISPERSION/InterpolationAgent/startSimulation",
//							newJo.toString());
//
//					String statisticcall = execute("/JPS_DISPERSION/StatisticAnalysis", newJo.toString());
				}
			}

		} else {
			// =======================================================================
			String wasteresult = execute("/JPS/PowerPlant", requestParams.toString());
			String waste = new JSONObject(wasteresult).getString("waste");
			requestParams.put("waste", waste);
			result = execute("/JPS_DISPERSION/adms/dispersion", requestParams.toString(), HttpPost.METHOD_NAME);
			String folder = new JSONObject(result).getString("folder");
			requestParams.put("folder", folder);
		}

		return requestParams;

	}
}
