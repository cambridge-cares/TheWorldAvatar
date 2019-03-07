package uk.ac.cam.cares.jps.scenario;

import java.io.File;
import java.io.IOException;

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
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.base.scenario.ScenarioHelper;

@WebServlet(urlPatterns = {"/scenario/*"})
public class ScenarioAgent extends HttpServlet {
	
	private static final long serialVersionUID = 3746092168199681624L;

	private static Logger logger = LoggerFactory.getLogger(ScenarioAgent.class);
	

	/**
	 * @see HttpServlet#doGet(HttpServletRequest request, HttpServletResponse response)
	 */
	protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		
		JSONObject jo = AgentCaller.readJsonParameter(request);
		
		String path = request.getPathInfo();
		String[] parts = ScenarioHelper.dividePath(path);
		String scenarioName = parts[0];
		String operation = parts[1];
		
		// TODO-AE SC the created scenario url / name  might be part of the response body such that the client can use the scenario in future
		
		logger.info("called for path=" + path);
		logger.info("divided into scenario name=" + scenarioName + ", operation=" + operation);
		
		String scenarioBucket = ScenarioHelper.getScenarioBucket(scenarioName);
		
		ScenarioLog log = getScenarioLog(scenarioName);
		boolean copyOnRead = ScenarioManagementAgent.getCopyOnRead(log);
		
		String result = "";
		if (operation == null) {
			
			// do nothing, the scenario log file has been already created above
			
			//result = getScenarioFile(scenarioName);
			
		} else if ("/option".equals(operation)) {
			
			setOptions(jo, scenarioName, log);

		} else if ("/mock".equals(operation)) {
			
			mock(jo, scenarioName, log);
		
		} else if ("/call".equals(operation)) {

			result = call(jo, scenarioName);
				
		} else if ("/read".equals(operation)) {
			
			result = readFile(jo, scenarioBucket, copyOnRead);
			
		} else if ("/query".equals(operation)) {
			
			result = queryFile(jo, scenarioBucket, copyOnRead);
			
		} else if ("/update".equals(operation)) {

			updateFile(jo, scenarioBucket);
			
		} else if ("/delete".equals(operation)) {
			
			deleteScenario(scenarioName);
			
		} else if ("/compose".equals(operation)) {
			
			result = compose(jo, scenarioName, log);

		} else if ("/preparerecording".equals(operation)) {
			
			result = prepareRecording(jo, scenarioName, log);
			
		} else {
			
			result = executeOperationOfMockedAgent(jo, scenarioName, operation, log);
		}
		
		AgentCaller.printToResponse(result, response);
	}
	
	private ScenarioLog getScenarioLog(String scenarioName) {
		
		String path = ScenarioManagementAgent.getScenarioLogPath(scenarioName);
		return new ScenarioLog(scenarioName, path);
	}
	
	private void setOptions(JSONObject jo, String scenarioName, ScenarioLog log) {
		
		JSONObject message = new JSONObject().put("operation", "option");
		
		if (jo.has(ScenarioManagementAgent.COPY_ON_READ)) {
			message.put(ScenarioManagementAgent.COPY_ON_READ, jo.getBoolean(ScenarioManagementAgent.COPY_ON_READ));
		}
		
		log.logMessage(scenarioName, message);
	}
	
	private void mock(JSONObject jo, String scenarioName, ScenarioLog log) {
		
		if (jo.isNull(JPSConstants.SCENARIO_AGENT)) {
			throw new JPSRuntimeException("missing input parameter " + JPSConstants.SCENARIO_AGENT);
		}
		
		String agent = jo.getString(JPSConstants.SCENARIO_AGENT);

		JSONObject message = new JSONObject().put("operation", "mock").put("agent", agent);
		log.logMessage(scenarioName, message);
	}
	
	private String call(JSONObject jo, String scenarioName) {
		
//		if (jo.isNull(JPSConstants.SCENARIO_AGENT_URL)) {
//			throw new JPSRuntimeException("missing input parameter scenarioagenturl");
//		}
		
//		String agent = null;
//		if (!jo.isNull(SCENARIO_AGENT)) {
//			agent = jo.getString(SCENARIO_AGENT);
//			createScenarioDescription(scenarioName, agent);
//		}

		
		// set the scenario url as input parameter 
		// this has the following consequence: if one agent makes a call to access the knowledge graph then its call is redirected
		// to the scenario agent
		// TODO-AE SC URGENT URGENT 20190515  also add for compose and mockedoperation....
		jo.put(JPSConstants.SCENARIO_URL, ScenarioManagementAgent.getScenarioUrl(scenarioName));
		
		// start the scenario by calling the operation (an operation can be called even if no agent or a different agent was given)
		String operation = jo.getString(JPSConstants.SCENARIO_AGENT_OPERATION);
	 	if (operation.startsWith("http")) {
	 		return AgentCaller.executeGetWithURLAndJSON(operation, jo.toString());
	 	} else {
	 		return AgentCaller.executeGetWithJsonParameter(operation, jo.toString());
	 	}
	}
	
	/**
	 * Returns the path of the requested resource from the bucket. If it doesn't exist and copyToBucket = true,
	 * then its content is copied to the bucket. If if doesn't exist and copyToBucket = false, then 
	 * the path of the requested resource itself is returned.
	 * 
	 * @param jo
	 * @param scenarioBucket
	 * @param copyToBucket if true the copy the requested resource to the scenario bucket
	 * @return the complete path of the scenario resource
	 */
	private String getResourcePath(JSONObject jo, String scenarioBucket, boolean copyToBucket) {
			    
		String resource = jo.getString(JPSConstants.SCENARIO_RESOURCE);
		String completePathWithinBucket = ScenarioHelper.getFileNameWithinBucket(resource, scenarioBucket);
		logger.info("get resource path for resource=" + resource + ", in bucket=" + completePathWithinBucket + ", copyToBucket=" + copyToBucket);
		
		File fileWithinBucket = new File(completePathWithinBucket);
	    if (fileWithinBucket.exists()) {
	    	return completePathWithinBucket;
	    } else if (copyToBucket) {
	    	String content = new QueryBroker().readFile(resource);
	    	QueryBroker.writeFileLocally2(completePathWithinBucket, content);
	    	return completePathWithinBucket;
	    }  

	    return resource;
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
	private String readFile(JSONObject jo, String scenarioBucket, boolean copyOnRead) {
		
		String resource = getResourcePath(jo, scenarioBucket, copyOnRead);
		// TODO-AE SC the prepare method might create a scenario copy; in this case prepare method already reads the content; i.e. in this case
		// we read it here a second time --> refactor the code such that this is not required; the same for queryFile
		return new QueryBroker().readFile(resource);
	}
	
	private String queryFile(JSONObject jo, String scenarioBucket, boolean copyOnRead) {
		
		String resource = getResourcePath(jo, scenarioBucket, copyOnRead);
		String sparqlQuery = jo.getString(JPSConstants.QUERY_SPARQL_QUERY);
		
		logger.info("sparqlquery=" + sparqlQuery);
		
		return new QueryBroker().queryFile(resource, sparqlQuery);
	}
	
	private void updateFile(JSONObject jo, String scenarioBucket) {
		
		String resource = getResourcePath(jo, scenarioBucket, true);
		String sparqlUpdate = jo.getString(JPSConstants.QUERY_SPARQL_UPDATE);
		
		logger.info("sparqlupdate=" + sparqlUpdate);
		
		new QueryBroker().updateFile(resource, sparqlUpdate);
	}
	
	public void deleteScenario(String scenarioName) {
		
		String scenarioBucket = ScenarioHelper.getScenarioBucket(scenarioName);
		File directory = new File(scenarioBucket);
				
//		String deletionBucket = ScenarioHelper.getScenarioBucket("DELETED_" + scenarioName + "_" + System.currentTimeMillis());
//		File deletionDirectory = new File(deletionBucket);
//		boolean renamed = directory.renameTo(deletionDirectory);
//		if (!renamed) {
//			logger.info("can't rename directory to " + deletionDirectory.getName());
//		}
		
		
		for(File current : directory.listFiles()) {
			boolean deleted = current.delete();
			if (!deleted) {
				logger.info("can't delete file = " + current.getName());
			}
		}
		boolean deleted = directory.delete();
		if (!deleted) {
			logger.info("can't delete directory = " + directory.getName());
		}
	}
	
	private String compose(JSONObject jo, String scenarioName, ScenarioLog log) {
		
		// The variable composedAgent is the IRI of the scenario description (OWL file). 
		// The description contains all information (input and output parameter) that the composition engine needs
		// to compose the corresponding agent.  
		// The input variable jo contains already the values for all input parameter (for executing the composed agent). 
		// However, it does not contained the parameter types and output parameter that the composition engines needs to know. 
		// Thus, we have to add the IRI of composedAgent.
		//String composedAgent =  ScenarioManagementAgent.getScenarioIRI(scenarioName);
		// TODO-AE SC 20190215 move log constructor out this method, maybe managed by ScenarioManagementAgent?
		String composedAgent = ScenarioManagementAgent.getLatestMockedAgent(log);
		// TODO-AE SC URGENT define "agent" as key in Base (that is also used by AgentWebAPI). Summarize this with SCENARIO_AGENT.
		jo.put("agent", composedAgent);
		
		// The scenario url is the URL of the scenario functionality for mocking the composed agent.
		// It is used for "callbacks" from agents that are involved in the composition to the scenario. 
		jo.put(JPSConstants.SCENARIO_URL, ScenarioManagementAgent.getScenarioUrl(scenarioName));
		
		return AgentCaller.executeGetWithJsonParameter("/JPS_COMPOSITION/execute", jo.toString());
	}
	
	private String executeOperationOfMockedAgent(JSONObject jo, String scenarioName, String operation, ScenarioLog log) {
		
		String httpUrl = findHttpUrlForOperationOfMockedAgent(jo, scenarioName, operation, log);
		if (httpUrl == null) {
			throw new JPSRuntimeException("unknown operation for scenario agent, scenarioName=" + scenarioName + ", operation=" + operation);
		}
		
		jo.put(JPSConstants.SCENARIO_URL, ScenarioManagementAgent.getScenarioUrl(scenarioName));
		String result = AgentCaller.executeGetWithURLAndJSON(httpUrl, jo.toString());
		
		System.out.println("MYMY = " + result);
		
		JSONObject joresult = new JSONObject();
		if ((result != null) && !result.isEmpty()) {
			joresult = new JSONObject(result);
		}
		
		JSONObject message = new JSONObject();
		String agent = ScenarioManagementAgent.getLatestMockedAgent(log);
		message.put("agent", agent);
		message.put("operation", operation);
		// TODO-AE SC 20190220 only log the input and output parameters for the mocked operation
		message.put("input", jo);
		message.put("output", joresult);
		log.logMessage(scenarioName, message);
		
		return result;
	}
	
	private String findHttpUrlForOperationOfMockedAgent(JSONObject jo, String scenarioName, String operation, ScenarioLog log) {
		
		String agent = ScenarioManagementAgent.getLatestMockedAgent(log);
		if (agent != null ) {
			JSONObject input = new JSONObject().put("agent", agent); 
			String jsondescr = AgentCaller.executeGetWithJsonParameter("/JPS_COMPOSITION/describe", input.toString());
			JSONArray joservice = new JSONObject(jsondescr).getJSONArray("service");
			int size = joservice.length();
			for (int i=0; i<size; i++) {
				JSONObject jooperation = joservice.getJSONObject(i).getJSONObject("hasOperation");
				String httpUrl = jooperation.getString("hasHttpUrl");
				int index = httpUrl.lastIndexOf("/");
				if (operation.equals(httpUrl.substring(index))) {
					return httpUrl;
				}
			}
		}
		
		return null;
	}
	
	private String prepareRecording(JSONObject jo, String scenarioName, ScenarioLog log) {
		
		if (jo.isNull(JPSConstants.SCENARIO_AGENT)) {
			throw new JPSRuntimeException("missing input parameter " + JPSConstants.SCENARIO_AGENT);
		}
		
		if (jo.isNull(JPSConstants.SCENARIO_AGENT_OPERATION)) {
			throw new JPSRuntimeException("missing input parameter " + JPSConstants.SCENARIO_AGENT_OPERATION);
		}
		
		String agent = jo.getString(JPSConstants.SCENARIO_AGENT);
		String agentoperation = jo.getString(JPSConstants.SCENARIO_AGENT_OPERATION);

		
		// TODO-AE SC 20190218 extend recording to several sets of input parameters
		// by now, the original result is overwritten if the parameters are changed
		// the parameters also have to be written to the scenario log
		JSONObject message = new JSONObject().put("operation", "preparerecording")
				.put(JPSConstants.SCENARIO_AGENT, agent)
				.put(JPSConstants.SCENARIO_AGENT_OPERATION, agentoperation);
		log.logMessage(scenarioName, message);

		return "";
	}
}