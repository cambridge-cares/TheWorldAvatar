package uk.ac.cam.cares.jps.scenario;

import java.io.File;
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
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.base.query.ScenarioKeys;

@WebServlet(urlPatterns = {"/scenario/*"})
public class ScenarioAgent extends HttpServlet {
	
	private static final long serialVersionUID = 3746092168199681624L;

	private static Logger logger = LoggerFactory.getLogger(ScenarioAgent.class);
	
	// TODO-AE SC move to JPS Base
	public static final String SCENARIO_AGENT = "scenarioagent";

	/**
	 * @see HttpServlet#doGet(HttpServletRequest request, HttpServletResponse response)
	 */
	protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		
		JSONObject jo = AgentCaller.readJsonParameter(request);
		
		String path = request.getPathInfo();
		String[] parts = dividePath(path);
		String scenarioName = parts[0];
		String operation = parts[1];
		
		// TODO-AE SC the created scenario url / name  might be part of the response body such that the client can use the scenario in future
		
		logger.info("called for path=" + path);
		logger.info("divided into scenario name=" + scenarioName + ", operation=" + operation);
		
		String scenarioBucket = new ScenarioManagementAgent().getScenarioBucket(scenarioName);
		
		String result = null;
		if (operation == null) {
			
			result = getScenarioFile(path);

		} else if ("/call".equals(operation)) {

			result = call(jo, scenarioName);
				
		} else if ("/read".equals(operation)) {
			
			result = readFile(jo, scenarioBucket);
			
		} else if ("/query".equals(operation)) {
			
			result = queryFile(jo, scenarioBucket);
			
		} else {
			
			String httpUrl = getScenarioSpecificHttpUrl(scenarioBucket, operation);
			if (httpUrl == null) {
				throw new JPSRuntimeException("unknown operation for scenario agent=" + getScenarioUrl(scenarioName) + ", operation=" + operation);
			}
			result = AgentCaller.executeGetWithURLAndJSON(httpUrl, jo.toString());
		}
		
		AgentCaller.printToResponse(result, response);
	}
	
	/**
	 * Divides the path into one to three Strings:<br>
	 * - the first string is the scenario name, e.g. for URL http://www.twa.com/JPS_SCENARIO/scenario/foo1234567 the scenarioName is foo1234567<br>
	 * - the second string is an optional operation, e.g. start or clean<br>
	 * 
	 * @param path
	 * @return Array with two strings
	 */
	public String[] dividePath(String path) {
		
		String scenarioName = null;
		String operation = null; 
		
		// non-empty paths have a leading /
		if ((path == null) || (path.length() <= 1)) {
			scenarioName =  UUID.randomUUID().toString();
			logger.info("creating a new scenario with name = " + scenarioName);
		} else {
			// remove leading / at the beginning of path
			path = path.substring(1);
			int index =  path.indexOf("/");
			
			if (index < 0) {
				scenarioName = path;
				operation = null;
			} else {
				scenarioName = path.substring(0, index);
				operation = path.substring(index);
			} 
			
			if (scenarioName.length() < 10) {
				throw new JPSRuntimeException("the length of the scenario name must be at least 10");
			} else {
				// TODO-AE SC check that scenario name only contains letters, digits and - or _
			}
		}
		
		return new String[] {scenarioName, operation};
	}
	
	private String getScenarioUrl(String scenarioName) {
		// TODO-AE SC URGENT localhost 8080
		return "http://localhost:8080/JPS_SCENARIO/scenario/" + scenarioName;
	}
	
	private String getScenarioFile(String name) {
		
		// skip leading /
		String fileName = name.substring(1);
		int i = fileName.indexOf(".owl");
		if (i < -1) {
			fileName += ".owl";
		}
		String fullFileName = ScenarioManagementAgent.getWorkingDir() + "/" + fileName;
		return new QueryBroker().readFile(fullFileName);
	}
	
	private String call(JSONObject jo, String scenarioName) {
		
//		if (jo.isNull(ScenarioKeys.SCENARIO_AGENT_URL)) {
//			throw new JPSRuntimeException("missing input parameter scenarioagenturl");
//		}
		
		String agent = null;
		if (!jo.isNull(SCENARIO_AGENT)) {
			agent = jo.getString(SCENARIO_AGENT);
			new ScenarioManagementAgent().createScenarioDescription(scenarioName, agent);
		}

		
		// set the scenario url as input parameter 
		// this has the following consequence: if one agent makes a call to access the knowledge graph then its call is redirected
		// to the scenario agent
		jo.put(ScenarioKeys.SCENARIO_URL, getScenarioUrl(scenarioName));
		
		// start the scenario by calling the operation (an operation can be called even if no agent or a different agent was given)
		String operation = jo.getString(ScenarioKeys.SCENARIO_AGENT_OPERATION);
	 	if (operation.startsWith("http")) {
	 		return AgentCaller.executeGetWithURLAndJSON(operation, jo.toString());
	 	} else {
	 		return AgentCaller.executeGetWithJsonParameter(operation, jo.toString());
	 	}
	}
	
	/**
	 * The resource location is the full path for the resource: either the original url given as an input parameter or the local scenario path. 
	 * 
	 * @param jo
	 * @param scenarioBucket
	 * @return the resource location
	 */
	private String prepare(JSONObject jo, String scenarioBucket) {
			    
		String resource = jo.getString(ScenarioKeys.SCENARIO_RESOURCE);
	    int i = resource.lastIndexOf("/");
	    int hashForPath = resource.substring(0, i).hashCode();
		String fileNameWithinBucket = hashForPath + "_" + resource.substring(i+1);
		String completePathWithinBucket = scenarioBucket + "/" + fileNameWithinBucket;
		logger.info("readFile path within bucket=" + completePathWithinBucket);
		File fileWithinBucket = new File(completePathWithinBucket);
	    if (!fileWithinBucket.exists()) {
	    	String content = new QueryBroker().readFile(resource);
		    ScenarioManagementAgent.writeToFile(content, completePathWithinBucket);
	    } 

	    return completePathWithinBucket;
	}
	
	/**
	 * This method checks whether the demanded file already exists. If not, the file is read and copied to the scenario bucket. 
	 * If yes, it is read from the scenario bucket. 
	 * There is no need for checking whether copy-on-write is configured because in this case the scenario agent is not called for reading a file at all. 
	 * 
	 * @param scenarioName
	 * @param rdfResource
	 * @return
	 */
	private String readFile(JSONObject jo, String scenarioBucket) {
		
		String resource = prepare(jo, scenarioBucket);
		// TODO-AE SC the prepare method might create a scenario copy; in this case prepare method already reads the content; i.e. in this case
		// we read it here a second time --> refactor the code such that this is not required; the same for queryFile
		return new QueryBroker().readFile(resource);
	}
	
	
	private String queryFile(JSONObject jo, String scenarioBucket) {
		
		String resource = prepare(jo, scenarioBucket);
		String sparqlQuery = jo.getString(ScenarioKeys.QUERY_SPARQL_QUERY);
		return new QueryBroker().queryFile(resource, sparqlQuery);
	}
	
	private String getScenarioSpecificHttpUrl(String scenarioBucket, String operation) {
		
		String resource = scenarioBucket + ".owl";
		String content = new QueryBroker().readFile(resource);
		String httpUrl = ScenarioManagementAgent.getHttpUrl(content);
		int lastslash = httpUrl.lastIndexOf("/");
		String agentOperation = httpUrl.substring(lastslash);
		if (operation.equals(agentOperation)) {
			return httpUrl;
		}
		return null;
	}
}