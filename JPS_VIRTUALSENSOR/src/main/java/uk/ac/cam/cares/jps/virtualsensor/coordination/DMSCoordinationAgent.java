package uk.ac.cam.cares.jps.virtualsensor.coordination;

import java.util.ArrayList;
import java.util.concurrent.CompletableFuture;

import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;

import org.apache.http.client.methods.HttpPost;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet;
import uk.ac.cam.cares.jps.virtualsensor.general.DispersionModellingAgent;
import uk.ac.cam.cares.jps.virtualsensor.sparql.DispSimSparql;
import uk.ac.cam.cares.jps.virtualsensor.sparql.ShipSparql;

//@WebServlet("/DMSCoordinationAgent")
@WebServlet(urlPatterns = { "/episode/dispersion/coordination", "/adms/dispersion/coordination" })
public class DMSCoordinationAgent extends JPSHttpServlet {

	Logger logger = LoggerFactory.getLogger(DMSCoordinationAgent.class);
	public static final String DISPERSION_PATH = "/JPS_VIRTUALSENSOR";
	public static final String EPISODE_PATH = "/episode/dispersion/coordination";
	public static final String ADMS_PATH = "/adms/dispersion/coordination";

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
	
	@Override
	protected JSONObject processRequestParameters(JSONObject requestParams, HttpServletRequest request) {
		String path = request.getServletPath();

		// temporary workaround until we remove city IRIs completely!
		String city = requestParams.getString("city");

		String result;

		AgentCaller.executeGetWithJsonParameter("JPS_VIRTUALSENSOR/WeatherAgent", requestParams.toString());

		logger.info("calling ship data agent = " + requestParams.toString());
		AgentCaller.executeGetWithJsonParameter("JPS_VIRTUALSENSOR/ShipDataAgent", requestParams.toString());

		String[] shipIRI = DispSimSparql.GetEmissionSources(requestParams.getString(DispSimSparql.SimKey));
		
		if (shipIRI.length != 0) {
			RunShipAsync(shipIRI);

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
				String interpolationcall = execute("/JPS_VIRTUALSENSOR/InterpolationAgent/startSimulation",
						newJo.toString());

				String statisticcall = execute("/JPS_VIRTUALSENSOR/StatisticAnalysis", newJo.toString());

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
