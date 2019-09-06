package uk.ac.cam.cares.jps.scenario;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.json.JSONArray;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import uk.ac.cam.cares.jps.base.config.JPSConstants;
import uk.ac.cam.cares.jps.base.config.KeyValueManager;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.scenario.BucketHelper;
import uk.ac.cam.cares.jps.base.scenario.JPSContext;
import uk.ac.cam.cares.jps.base.scenario.ScenarioHelper;
import uk.ac.cam.cares.jps.scenario.ScenarioLog.ScenarioLogEntry;

@WebServlet(urlPatterns = {"/scenariomanagement/*"})
public class ScenarioManagementAgent extends HttpServlet {

	private static final long serialVersionUID = 1733142247564226760L;
	private static Logger logger = LoggerFactory.getLogger(ScenarioManagementAgent.class);


	
//	public static String getScenarioDescriptionName(String scenarioName) {
//		return getWorkingDir() + "/" + scenarioName + ".owl";
//	}
	
	
	
	public static String getScenarioLogPath(String scenarioName) {
		return ScenarioHelper.getScenarioWorkingDir() + "/" + scenarioName + "/" + scenarioName + ".json";
	}
	
	public static ScenarioLog getScenarioLog(String scenarioName) {	
		String path = ScenarioManagementAgent.getScenarioLogPath(scenarioName);
		return new ScenarioLog(scenarioName, path);
	}
	
	public static String getScenarioIRI(String scenarioName) {
		return KeyValueManager.getServerAddress() + ScenarioHelper.getScenarioPath(scenarioName) + ".owl#Service";
	}
	
	public static String getScenarioUrl(String scenarioName) {
		return KeyValueManager.getServerAddress() + ScenarioHelper.getScenarioPath(scenarioName);
	}
	
	/**
	 * Returns the latest value for copy-on-read from the log. Default is false, i.e. copy-on-write.
	 * 
	 * @param log
	 * @return
	 */
	public static boolean getCopyOnRead(ScenarioLog log) {
		List<ScenarioLogEntry> entries = log.search(JPSConstants.SCENARIO_OPTION_COPY_ON_READ, null);
		if (entries.size() > 0) {
			ScenarioLogEntry latestEntry = entries.get(entries.size()-1);
			return latestEntry.message.getBoolean(JPSConstants.SCENARIO_OPTION_COPY_ON_READ);
		}
		
		return false;
	}
	
	/**
	 * @see HttpServlet#doGet(HttpServletRequest request, HttpServletResponse response)
	 */
	protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		
		String path = request.getPathInfo();
		logger.info("called for path=" + path);
		
		if ("/list".equals(path)) {

			String result = listScenariosAndAgentsAsJson();
			AgentCaller.printToResponse(result, response);
			
		} else {
			throw new JPSRuntimeException("unknown operation");
		}
	}
	
	public String listScenariosAndAgentsAsJson() {

		String descriptions = AgentCaller.executeGet("/JPS_COMPOSITION/agentdescriptions");
		JSONObject joresult = new JSONObject(descriptions);	
		JSONArray joarray = joresult.getJSONArray("result");
		
		// create for each scenario a scenario agent as json object and add it to joarray
		List<String> names = getScenarioNames();
		for (String current : names) {
			logger.info("adding scenario agent for scenario=" + current);
			ScenarioLog log = ScenarioManagementAgent.getScenarioLog(current);
			JSONObject jo = createScenarioAgent(current, log);
			if (jo != null) {							
				joarray.put(jo);
			}
		}
		
		return joresult.toString();
	}
	
	public JSONObject createScenarioAgent(String scenarioName, ScenarioLog log) {
		
		JSONObject result = null;
		
		// add standard operations from scenario agents
		// TODO-AE SC URGENT 20190215 deploy scenario agent description to claudius without parameter scenarioname
		List<ScenarioLogEntry> entries = log.search("extendsagent", null);
		String agent = entries.get(0).message.getString("extendsagent");
		JSONObject input = new JSONObject().put("agent", agent); 
		String jsondescr = AgentCaller.executeGetWithJsonParameter("/JPS_COMPOSITION/describe", input.toString());
		
		result = new JSONObject(jsondescr);
		result.put("type", "scenario");
		result.put("name", scenarioName);
		// TODO-AE SC 20190215 change the OWL IRI to JSON?
		result.put("id", getScenarioIRI(scenarioName));
		
		JSONArray joservice = result.getJSONArray("service");
		int size = joservice.length();
		for (int i=0; i<size; i++) {
			JSONObject jooperation = joservice.getJSONObject(i).getJSONObject("hasOperation");
			String httpUrl = jooperation.getString("hasHttpUrl");
			int index = httpUrl.lastIndexOf("/");
			httpUrl = getScenarioUrl(scenarioName) + httpUrl.substring(index);
			jooperation.put("hasHttpUrl", httpUrl);
		}
		
		// add operations from the latest mocked agent
		agent = ScenarioMockManager.getLatestMockedAgent(log);
		if (agent != null) {
			input = new JSONObject().put("agent", agent); 
			jsondescr = AgentCaller.executeGetWithJsonParameter("/JPS_COMPOSITION/describe", input.toString());
			
			JSONArray joservicemocked = new JSONObject(jsondescr).getJSONArray("service");
			size = joservicemocked.length();
			for (int i=0; i<size; i++) {
				JSONObject jooperation = joservicemocked.getJSONObject(i).getJSONObject("hasOperation");

				String httpUrl = jooperation.optString("hasHttpUrl");
				System.out.println(httpUrl);
				if (httpUrl != null && !httpUrl.isEmpty()) {
					// replace the original agent URL by the scenario URL for the same operation
					int index = httpUrl.lastIndexOf("/");
					httpUrl = getScenarioUrl(scenarioName) + httpUrl.substring(index);
					jooperation.put("hasHttpUrl", httpUrl); 
				} else {
					jooperation.remove("hasHttpUrl");
				}
				
					
				// add jooperation to the joservice for the result
				JSONObject newoperation = new JSONObject().put("hasOperation", jooperation);
				joservice.put(newoperation);
			}
		}
		
		return result;
	}
	
	public List<String> getScenarioNames() {
		List<String> result = new ArrayList<String>();
		
		File dir = new File(ScenarioHelper.getScenarioWorkingDir());
		for (File current : dir.listFiles()) {
			if (current.isDirectory()) {
				String scenarioName = current.getName();
				result.add(scenarioName);
			}
		}
		
		return result;
	}
	
	public List<String> getScenarioIRIsOLD() {
		List<String> result = new ArrayList<String>();
		
		File dir = new File(ScenarioHelper.getScenarioWorkingDir());
		for (File current : dir.listFiles()) {
			if (current.isFile() && current.getName().endsWith(".owl")) {
				String iri = getScenarioIRI(current.getName());
				result.add(iri);
			}
		}
		
		return result;
	}
	
	public static String execute(String scenarioName, String httpUrl, JSONObject jo) {
		addJpsContext(scenarioName, jo);
		return AgentCaller.executeGetWithURLAndJSON(httpUrl, jo.toString());
	}
	
	public static void addJpsContext(String scenarioName, JSONObject jo) {
		// set the scenario url as input parameter 
		// this has the following consequence: if one agent makes a call to access the knowledge graph then its call is redirected
		// to the scenario agent
		String scenarioUrl = getScenarioUrl(scenarioName);
		JPSContext.putScenarioUrl(jo, scenarioUrl);
		String usecaseUrl = jo.optString(JPSConstants.SCENARIO_USE_CASE_URL);
		if (usecaseUrl == null) {
			// create new usecaseUrl
			usecaseUrl = BucketHelper.getUsecaseUrl(scenarioUrl);
			JPSContext.putUsecaseUrl(jo, usecaseUrl);
		}
	}
}
