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

import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.config.JPSConstants;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet;
import uk.ac.cam.cares.jps.base.scenario.ScenarioHelper;
import uk.ac.cam.cares.jps.base.util.MiscUtil;

@WebServlet(urlPatterns = {"/scenariomod/*"})
/** Used if new Scenario is created. it runs into here to create new scenario
 * Otherwise it should just call ScenarioAgent
 * @author LONG01
 *
 */
public class ScenarioModifier extends JPSAgent{

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	private static Logger logger = LoggerFactory.getLogger(ScenarioModifier.class);
	/**
	 * @see HttpServlet#doGet(HttpServletRequest request, HttpServletResponse response)
	 */
	protected void dxxoGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		
		JSONObject jo = AgentCaller.readJsonParameter(request);
		logger.debug(jo.toString());
		String path = request.getPathInfo();
		logger.debug("called for path=" + path);
		
		String[] parts = ScenarioHelper.dividePath(path);
		String scenarioName = parts[0]; //name of scenario: base, testNuclear
		String newscenarioName = scenarioName + UUID.randomUUID().toString();//give random uuid to distinguish
		path.replace(scenarioName, newscenarioName);//replace string
		path.replaceFirst("scenariomod", "scenario");//substitute string to call on path. 
		logger.info("new path: " + path);
		// Think that only time new scenario is called would be if there is a call. 
		String result = AgentCaller.executeGetWithJsonParameter("jps/scenario/"+newscenarioName+"/call", jo.toString()); //get a String result
		// the created scenario url / name is part of the response body
		//such that the client can use the scenario in future
		//F, why 
		AgentCaller.printToResponse(result, response);
	}
	@Override
    public JSONObject processRequestParameters(JSONObject requestParams) {
    	validateInput(requestParams);
    	String path = MiscUtil.optNullKey(requestParams, JPSConstants.PATH);
		logger.debug("called for path=" + path);
		String[] parts = ScenarioHelper.dividePath(path);
		String scenarioName = parts[0]; //name of scenario: base, testNuclear
		String newscenarioName = scenarioName + UUID.randomUUID().toString();//give random uuid to distinguish
		path.replace(scenarioName, newscenarioName);//replace string
		path.replaceFirst("scenariomod", "scenario");//substitute string to call on path. 
		logger.info("new path: " + path);
		String result = AgentCaller.executeGetWithJsonParameter("jps/scenario/"+newscenarioName+"/call", requestParams.toString()); //get a String result
    	return new JSONObject(result);
    }
	
}
