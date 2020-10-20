package uk.ac.cam.cares.jps.agent.matlab;

//import java.io.IOException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;

import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
//import uk.ac.cam.cares.jps.base.query.QueryBroker;
//import uk.ac.cam.cares.jps.base.scenario.BucketHelper;
//import uk.ac.cam.cares.jps.base.scenario.JPSContext;
import uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet;


/**
 * Servlet implementation class JPSAgentCaller
 */
@WebServlet("/JPSAgentCaller")
public class JPSAgentCaller extends JPSHttpServlet {
	private static final long serialVersionUID = 1L;
	
	 @Override
	 //this should ONLY be called by scenarioAgent
	   	protected JSONObject processRequestParameters(JSONObject requestParams, HttpServletRequest request) {
			JSONObject jo = AgentCaller.readJsonParameter(request);
			String baseUrl= "C:/JParkSimulator-git-project/JPS_DIGITAL_TWIN/src/main/resources/input_mat/";
			jo.put("baseUrl", baseUrl);
			AgentCaller.executeGetWithJsonParameter("JPS_DIGITALTWIN/test", jo.toString());
			return jo;
		}

}
