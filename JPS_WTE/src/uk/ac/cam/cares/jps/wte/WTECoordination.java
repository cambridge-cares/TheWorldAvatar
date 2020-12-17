package uk.ac.cam.cares.jps.wte;

import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;

import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.base.scenario.BucketHelper;
import uk.ac.cam.cares.jps.base.scenario.JPSContext;
import uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet;

@WebServlet(urlPatterns= {"/startsimulationCoordinationWTE"})
public class WTECoordination extends JPSHttpServlet{

	private static final long serialVersionUID = 1L;
	 @Override
	 //this should ONLY be called by scenarioAgent
	   	protected JSONObject processRequestParameters(JSONObject requestParams, HttpServletRequest request) {
			JSONObject jo = AgentCaller.readJsonParameter(request);
			String baseUrl= QueryBroker.getLocalDataPath();
			//check name of scenario: 
			String sourceUrl = JPSContext.getScenarioUrl(requestParams);
			String sourceName = BucketHelper.getScenarioName(sourceUrl);
			logger.info("Scenario Url" + sourceUrl);
			jo.put("baseUrl", baseUrl);
			AgentCaller.executeGetWithJsonParameter("JPS_WTE/startsimulation", jo.toString()); //I pray hard that this works
			return jo;
		}

}
