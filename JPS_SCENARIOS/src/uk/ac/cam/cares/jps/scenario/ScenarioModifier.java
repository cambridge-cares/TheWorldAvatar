package uk.ac.cam.cares.jps.scenario;

import java.io.IOException;
import java.util.UUID;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.scenario.ScenarioHelper;

@WebServlet(urlPatterns = {"/scenariomod/*"})
public class ScenarioModifier {

	private static Logger logger = LoggerFactory.getLogger(ScenarioModifier.class);
	/**
	 * @see HttpServlet#doGet(HttpServletRequest request, HttpServletResponse response)
	 */
	protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		
		JSONObject jo = AgentCaller.readJsonParameter(request);
		logger.debug(jo.toString());
		String path = request.getPathInfo();
		logger.debug("called for path=" + path);
		
		String[] parts = ScenarioHelper.dividePath(path);
		String scenarioName = parts[0]; //name of scenario: base, testNuclear
		String newscenarioName = scenarioName + UUID.randomUUID().toString();//give random uuid to distinguish
		path.replace(scenarioName, newscenarioName);//replace string
		path.replaceFirst("scenariomod", "scenario");//substitute string to call on path. 
		String result = AgentCaller.executeGetWithJsonParameter("jps/scenario/"+newscenarioName, jo.toString()); //get a String result
		// the created scenario url / name is part of the response body
		//such that the client can use the scenario in future
		//F, why 
		AgentCaller.printToResponse(result, response);
	}
}
