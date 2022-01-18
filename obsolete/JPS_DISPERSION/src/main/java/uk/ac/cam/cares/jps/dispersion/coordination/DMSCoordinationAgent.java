package uk.ac.cam.cares.jps.dispersion.coordination;

import java.net.URL;
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
	
	@Override
	protected JSONObject processRequestParameters(JSONObject requestParams, HttpServletRequest request) {
		String path = request.getServletPath();

		// temporary workaround until we remove city IRIs completely!
		String city = requestParams.getString("city");

		String result;

		// @TODO - improve weather update frequency
		result = execute("/JPS_DISPERSION/SensorWeatherAgent", requestParams.toString());
		
		JSONArray stationiri = new JSONObject(result).getJSONArray("stationiri");
		requestParams.put("stationiri", stationiri);

		logger.info("calling ship data agent = " + requestParams.getJSONObject("region").toString());
		String resultship = AgentCaller.executeGetWithJsonParameter("JPS_DISPERSION/ShipDataAgent", 
				requestParams.getJSONObject("region").toString());

		JSONObject jsonShip = new JSONObject(resultship);
		requestParams.put(PARAM_KEY_SHIP, jsonShip);

		if (((JSONArray) ((JSONObject) jsonShip.get("collection")).get("items")).length() != 0) {
			String reactionMechanism = requestParams.optString("reactionmechanism");
			JSONArray newwaste;

			newwaste = getNewWasteAsync(reactionMechanism, jsonShip);

			requestParams.put("waste", newwaste);

			String resultPath = DISPERSION_PATH;
			if (path.equals(ADMS_PATH)) {
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

			} else if (path.equals(EPISODE_PATH)) {
				resultPath = resultPath + DispersionModellingAgent.EPISODE_PATH;
				result = execute(resultPath, requestParams.toString(), HttpPost.METHOD_NAME);

				String folder = new JSONObject(result).getString("folder");
				requestParams.put("folder", folder);
			}
		}

		return requestParams;

	}
}
