package uk.ac.cam.cares.jps.scenario;

import java.io.File;
import java.io.IOException;
import java.util.Date;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.io.FileUtils;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import uk.ac.cam.cares.jps.base.config.JPSConstants;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.base.scenario.BucketHelper;
import uk.ac.cam.cares.jps.base.scenario.JPSContext;
import uk.ac.cam.cares.jps.base.scenario.ScenarioHelper;
import uk.ac.cam.cares.jps.base.util.FileUtil;

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
		logger.debug("called for path=" + path);
		
		String[] parts = ScenarioHelper.dividePath(path);
		String scenarioName = parts[0];
		String operation = parts[1];
		
		// TODO-AE SC the created scenario url / name  might be part of the response body such that the client can use the scenario in future

		String scenariourl = JPSContext.getScenarioUrl(jo);
		String usecaseurl = JPSContext.getUsecaseUrl(jo);
		logger.info("called for scenario name=" + scenarioName + ", operation=" + operation + ", scenariourl=" + scenariourl + ", usecaseurl=" + usecaseurl);
		//logger.debug("with input param=" + jo);
		//logger.debug("with query string=" + request.getQueryString());
		//logger.debug("with request uri=" + request.getRequestURI());
		
		// The information of the scenarioUrl in JSON object from the input is redundant when
		// calling the scenario agent. However, it is needed as soon
		// as the scenario agent calls other agents.
		if ((scenariourl == null) || scenariourl.isEmpty()) {
			String scenarioUrl = ScenarioManagementAgent.getScenarioUrl(scenarioName);
			JPSContext.putScenarioUrl(jo, scenarioUrl);
		}
				
		ScenarioLog log = ScenarioManagementAgent.getScenarioLog(scenarioName);
		boolean copyOnRead = ScenarioManagementAgent.getCopyOnRead(log);
		
		String result = "";
		if (operation == null) {
			
			// do nothing, the scenario log file has been already created above
			
			//result = getScenarioFile(scenarioName);
		
		} else if ("/option".equals(operation)) {
			
			setOptions(jo, scenarioName, log);

		} else if ("/mock".equals(operation)) {
			
			new ScenarioMockManager().mock(jo, scenarioName, log);
		
		} else if ("/call".equals(operation)) {

			result = call(jo, scenarioName, log);
				
		} else if ("/read".equals(operation)) {
			
			result = readFile(jo, scenarioName, copyOnRead);
			
		} else if ("/query".equals(operation)) {
			
			result = queryFile(jo, scenarioName, copyOnRead);
			
		} else if ("/update".equals(operation)) {

			updateFile(jo, scenarioName);
			
		} else if ("/delete".equals(operation)) {
			
			deleteScenario(scenarioName);
			
		} else if ("/compose".equals(operation)) {
			
			result = compose(jo, scenarioName, log);

		} else if ("/preparerecording".equals(operation)) {
			
			result = prepareRecording(jo, scenarioName, log);
			
		} else if ("/ping".equals(operation)) {
			
			result = new Date().toString();
			
		} else if ("/mergescenario".equals(operation)) {
			
			mergeScenario(jo, scenarioName, log);
			
		} else {
			
			if (operation.startsWith("/" + JPSConstants.SCENARIO_SUBDIR_DATA + "/") 
					|| operation.startsWith("/" + JPSConstants.SCENARIO_SUBDIR_KB + "/")) {
				String localPath = ScenarioHelper.getScenarioBucket(scenarioName) + operation;
				result = FileUtil.readFileLocally(localPath);
			} else {
				
				result = new ScenarioMockManager().execute(jo, scenarioName, operation, log);
			}
		}
		
		AgentCaller.printToResponse(result, response);
	}
	
	private void setOptions(JSONObject jo, String scenarioName, ScenarioLog log) {
		
		JSONObject message = new JSONObject().put("operation", "option");
		
		if (jo.has(JPSConstants.SCENARIO_OPTION_COPY_ON_READ)) {
			message.put(JPSConstants.SCENARIO_OPTION_COPY_ON_READ, jo.getBoolean(JPSConstants.SCENARIO_OPTION_COPY_ON_READ));
		}
		
		log.logMessage(scenarioName, message);
	}
	
	private String call(JSONObject jo, String scenarioName, ScenarioLog log) {
		
//		if (jo.isNull(JPSConstants.SCENARIO_AGENT_URL)) {
//			throw new JPSRuntimeException("missing input parameter scenarioagenturl");
//		}
		
//		String agent = null;
//		if (!jo.isNull(SCENARIO_AGENT)) {
//			agent = jo.getString(SCENARIO_AGENT);
//			createScenarioDescription(scenarioName, agent);
//		}

		String result = null;
		
		// start the scenario by calling the operation (an operation can be called even if no agent or a different agent was given)
		String operation = jo.getString(JPSConstants.SCENARIO_AGENT_OPERATION);
	 	if (operation.startsWith("http")) {
	 		result = ScenarioManagementAgent.execute(scenarioName, operation, jo);
	 	} else {
	 		//throw new RuntimeException("can't call operation without http, operation = " + operation);
	 		 ScenarioManagementAgent.addJpsContext(scenarioName, jo);
	 		return AgentCaller.executeGetWithJsonParameter(operation, jo.toString());
	 	}
	 	
		JSONObject joresult = new JSONObject();
		if ((result != null) && !result.isEmpty()) {
			joresult = new JSONObject(result);
		}
	 	
		JSONObject message = new JSONObject();
		message.put("operation", "call");
		message.put("input", jo);
		message.put("output", joresult);
		log.logMessage(scenarioName, message);
		
		return result;
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
	private String getResourcePath(JSONObject jo, String scenarioName, boolean copyToBucket) {
			    
		String resource = jo.getString(JPSConstants.SCENARIO_RESOURCE);
		//String completePathWithinBucket = ScenarioHelper.getFileNameWithinBucket(resource, scenarioBucket);
		String scenarioUrl = BucketHelper.getScenarioUrl(scenarioName);
		String completePathWithinBucket = BucketHelper.getLocalPath(resource, scenarioUrl);
		logger.debug("get resource path for resource=" + resource + ", in bucket=" + completePathWithinBucket + ", copyToBucket=" + copyToBucket);
		
		File fileWithinBucket = new File(completePathWithinBucket);
	    if (fileWithinBucket.exists()) {
	    	return completePathWithinBucket;
	    } else if (copyToBucket) {
	    	String content = new QueryBroker().readFile(resource);
	    	FileUtil.writeFileLocally(completePathWithinBucket, content);
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
	private String readFile(JSONObject jo, String scenarioName, boolean copyOnRead) {
		
		String resource = getResourcePath(jo, scenarioName, copyOnRead);
		// TODO-AE SC the prepare method might create a scenario copy; in this case prepare method already reads the content; i.e. in this case
		// we read it here a second time --> refactor the code such that this is not required; the same for queryFile
		return new QueryBroker().readFile(resource);
	}
	
	private String queryFile(JSONObject jo, String scenarioName, boolean copyOnRead) {
		
		String resource = getResourcePath(jo, scenarioName, copyOnRead);
		String sparqlQuery = jo.getString(JPSConstants.QUERY_SPARQL_QUERY);
		
		logger.debug("sparqlquery=" + sparqlQuery);
		
		return new QueryBroker().queryFile(resource, sparqlQuery);
	}
	
	private void updateFile(JSONObject jo, String scenarioName) {
		
		String resource = getResourcePath(jo, scenarioName, true);
		String sparqlUpdate = jo.getString(JPSConstants.QUERY_SPARQL_UPDATE);
		
		logger.debug("sparqlupdate=" + sparqlUpdate);
		
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
		String composedAgent = ScenarioMockManager.getLatestMockedAgent(log);
		// TODO-AE SC URGENT define "agent" as key in Base (that is also used by AgentWebAPI). Summarize this with SCENARIO_AGENT.
		jo.put("agent", composedAgent);
		
		// The scenario url is the URL of the scenario functionality for mocking the composed agent.
		// It is used for "callbacks" from agents that are involved in the composition to the scenario. 
		//jo.put(JPSConstants.SCENARIO_URL, ScenarioManagementAgent.getScenarioUrl(scenarioName));
		
		//return AgentCaller.executeGetWithJsonParameter("/JPS_COMPOSITION/execute", jo.toString());
		
		return ScenarioManagementAgent.execute(scenarioName, "/JPS_COMPOSITION/execute", jo);
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
	
	/**
	 * This method merges the current scenario (the destination scenario) with the source scenario
	 * given as input parameter by its scenario url. The merging consists of two steps:<br>
	 * <br>
	 * (1) Merge the scenario log of the current scenario with the log of the source scenario<br>
	 * <br>
	 * (2) Merge the OWL files from the knowledge base of the two scenarios. Note that if an OWL file
	 * from the knowledge base subdirectory already exists in the current scenario, 
	 * it is replaced by that one of the source scenario; of course, those files can't be merged at file level. 
	 * Note also that files from the data subdirectory are not copied at all.
	 * 
	 * @param jo
	 * @param scenarioName
	 * @param log
	 */
	private void mergeScenario(JSONObject jo, String scenarioName, ScenarioLog log) {
		
		String sourceUrl = JPSContext.getScenarioUrl(jo);
		String sourceName = BucketHelper.getScenarioName(sourceUrl);
		
		// merge the logs
		ScenarioLog sourceLog = ScenarioManagementAgent.getScenarioLog(sourceName);
		log.merge(sourceLog);
		
		// merge the knowledge bases
		String sourceBucket = ScenarioHelper.getScenarioBucket(sourceName);
		String destBucket = ScenarioHelper.getScenarioBucket(scenarioName);
		copyOwlFiles(sourceBucket, destBucket);
	}
	
	public void copyOwlFiles(String sourceBucket, String destBucket) {
		
		File sourceDir = new File(sourceBucket);
		for (File current : sourceDir.listFiles()) {
			if (current.isDirectory()) {
				// current is a host sub directory
				String hostName = current.getName();
				for (File currentSub: current.listFiles()) {
					String subName = currentSub.getName();
					if (currentSub.isDirectory() && !"data".equals(subName)) {
						
						String destPath = destBucket + "/" + hostName + "/" + subName;
						logger.info("copying OWL files from " + currentSub.getAbsolutePath() + " to " + destPath);
						File destDir = new File(destPath);
						try {
							FileUtils.copyDirectory(currentSub, destDir);
						} catch (IOException e) {
							throw new JPSRuntimeException(e.getMessage(), e);
						}
					}
				}
			} 
			// else skip the scenario log json file
		}
	}
}