package uk.ac.cam.cares.jps.wte;

import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;

import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.base.scenario.BucketHelper;
import uk.ac.cam.cares.jps.base.scenario.JPSContext;
import uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet;

@WebServlet(urlPatterns= {"/startsimulationCoordinationWTE"})
public class WTECoordination extends JPSAgent{

	private static final long serialVersionUID = 1L;
	@Override
	public JSONObject processRequestParameters(JSONObject requestParams) {
	    requestParams = processRequestParameters(requestParams, null);
	    return requestParams;
	}
	@Override
	public JSONObject processRequestParameters(JSONObject requestParams,HttpServletRequest request) {
		String baseUrl= QueryBroker.getLocalDataPath();
		//check name of scenario: 
		requestParams.put("baseUrl", baseUrl);
		AgentCaller.executeGetWithJsonParameter("JPS_WTE/startSimulationAgent", requestParams.toString()); //I pray hard that this works
		return requestParams;
	}

}
