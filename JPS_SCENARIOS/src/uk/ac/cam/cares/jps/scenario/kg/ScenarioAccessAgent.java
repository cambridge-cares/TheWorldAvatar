package uk.ac.cam.cares.jps.scenario.kg;

import java.io.File;
import java.io.IOException;
import java.util.Date;

import javax.servlet.annotation.WebServlet;

import org.apache.commons.io.FileUtils;
import org.apache.commons.validator.routines.UrlValidator;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import uk.ac.cam.cares.jps.base.annotate.MetaDataAnnotator;
import uk.ac.cam.cares.jps.base.config.JPSConstants;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.AccessAgentCaller;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.base.scenario.BucketHelper;
import uk.ac.cam.cares.jps.base.scenario.JPSContext;
import uk.ac.cam.cares.jps.base.scenario.ScenarioHelper;
import uk.ac.cam.cares.jps.base.util.FileUtil;
import uk.ac.cam.cares.jps.base.util.MiscUtil;
import uk.ac.cam.cares.jps.scenario.ScenarioLog;
import uk.ac.cam.cares.jps.scenario.ScenarioManagementAgent;
import uk.ac.cam.cares.jps.scenario.ScenarioMockManager;
import uk.ac.cam.cares.jps.scenario.kb.ScenarioStoreClient;

//@WebServlet(urlPatterns = {"/scenario/*"})
public class ScenarioAccessAgent  extends AccessAgent { 

	private static final long serialVersionUID = 1L;

	private static Logger logger = LoggerFactory.getLogger(ScenarioAccessAgent.class);
	
	//Methods inherited from AccessAgent:
	//public JSONObject processRequestParameters(JSONObject requestParams)
    //public JSONObject processRequestParameters(JSONObject requestParams, HttpServletRequest request) 
	//public boolean validateInput(JSONObject requestParams) throws BadRequestException
	
	@Override
	public JSONObject get(JSONObject requestParams) {
		
		String path = MiscUtil.optNullKey(requestParams, JPSConstants.PATH);
		System.out.println("JSONPARAMS: "+ requestParams.toString());
		logger.info("called for path=" + path);
		String[] parts = ScenarioHelper.dividePath(path);
		String scenarioName = parts[0]; //name of scenario: base, testNuclear
		String operation = parts[1];
		
		// TODO-AE SC the created scenario url / name  might be part of the response body such that the client can use the scenario in future

		String scenariourl = JPSContext.getScenarioUrl(requestParams);
		String usecaseurl = JPSContext.getUsecaseUrl(requestParams);
		
		logger.info("called for scenario name=" + scenarioName + ", operation=" + operation + ", scenariourl=" + scenariourl + ", usecaseurl=" + usecaseurl);
		//logger.debug("with input param=" + jo);
		//logger.debug("with query string=" + request.getQueryString());
		//logger.debug("with request uri=" + request.getRequestURI());
		
		// The information of the scenarioUrl in JSON object from the input is redundant when
		// calling the scenario agent. However, it is needed as soon
		// as the scenario agent calls other agents.
		if ((scenariourl == null) || scenariourl.isEmpty()) {
			scenariourl = ScenarioManagementAgent.getScenarioUrl(scenarioName); //keyaddress/jps/scenario/scenarioName
			JPSContext.putScenarioUrl(requestParams, scenariourl);
		}
		
		ScenarioLog log = ScenarioManagementAgent.getScenarioLog(scenarioName);
		boolean copyOnRead = ScenarioManagementAgent.getCopyOnRead(log);
		
		String result = "";
		JSONObject jo = new JSONObject();
		if (operation == null) { //SWITCH doesn't accept operation as a null
			if (requestParams.has(JPSConstants.SCENARIO_RESOURCE)) {
				result = getOrQuery(requestParams, scenarioName, copyOnRead);
				return jo.put("result", result);
			} else {
				// just return the scenario log
				JSONObject resultjo = new JSONObject(log.getLogAsString());
				return resultjo;
			}
		}else {
			switch (operation) {			
				case "/option":			
					setOptions(requestParams, scenarioName, log);
					break;
				case "/mock":			
					new ScenarioMockManager().mock(requestParams, scenarioName, log);
					break;
				case "/call":
					result = call(requestParams, scenarioName, log);
					break;				
				case "/read":			
					result = readFile(requestParams, scenarioName, copyOnRead);
					break;			
				case "/query":			
					result = queryFile(requestParams, scenarioName, copyOnRead);
					break;			
				case "/update":
					updateFile(requestParams, scenarioName);
					break;			
				case "/delete":			
					deleteScenario(scenarioName);
					break;			
				case "/compose":			
					result = compose(requestParams, scenarioName, log);
					break;
				case "/preparerecording":			
					result = prepareRecording(requestParams, scenarioName, log);
					break;			
				case "/ping":			
					result = new Date().toString();
					break;			
				case "/mergescenario":			
					mergeScenario(requestParams, scenarioName, log);
					break;
				default:
					if (operation.startsWith("/" + JPSConstants.SCENARIO_SUBDIR_DATA + "/") 
							|| operation.startsWith("/" + JPSConstants.SCENARIO_SUBDIR_KB + "/")) {
						String localPath = ScenarioHelper.getScenarioBucket(scenarioName) + operation;
						result = FileUtil.readFileLocally(localPath); //TODO
					} 
					else  {					
						result = new ScenarioMockManager().execute(requestParams, scenarioName, operation, log);
					}
			}
			return jo.put("result", result);
		}
	}
	
	/**
	 * Perform Get or sparql query.
	 * @param requestParams
	 * @param scenarioName
	 * @param copyOnRead
	 * @return
	 */
	public String getOrQuery(JSONObject requestParams, String scenarioName, boolean copyOnRead) {

		String requestUrl =  MiscUtil.optNullKey(requestParams, JPSConstants.REQUESTURL);
		String sparqlquery = MiscUtil.optNullKey(requestParams, JPSConstants.QUERY_SPARQL_QUERY);
		String sparqlupdate = MiscUtil.optNullKey(requestParams, JPSConstants.QUERY_SPARQL_UPDATE);
		String paramResourceUrl= MiscUtil.optNullKey(requestParams,JPSConstants.SCENARIO_RESOURCE);
		String paramDatasetUrl = MiscUtil.optNullKey(requestParams, JPSConstants.SCENARIO_DATASET);
		String accept = MiscUtil.optNullKey(requestParams, JPSConstants.HEADERS);		
	
		if(sparqlupdate != null) {
		   	throw new JPSRuntimeException("parameter " + JPSConstants.QUERY_SPARQL_UPDATE + " is not allowed");
		}
		 
		try {
			logInputParams(requestParams, sparqlquery, false);
			
			String scenarioUrl = getScenarioUrl(scenarioName);
			ScenarioStoreClient storeClient = new ScenarioStoreClient(scenarioUrl);
			String resourceUrl = getResourceUrl(scenarioUrl, requestUrl, paramResourceUrl);
			
			String result = "";
			
			if (sparqlquery == null) {
				result = getFromKnowledgeBase(storeClient, paramDatasetUrl, resourceUrl, copyOnRead, accept);
			} else {
				result = queryKnowledgeBase(storeClient, resourceUrl, sparqlquery, copyOnRead);
			}
		
			return result;

		} catch (RuntimeException e) {
			logInputParams(requestParams, sparqlquery, true);
			throw new JPSRuntimeException(e);
		}
	}
	
	public static String getScenarioUrl(String scenarioName) {
		return ScenarioManagementAgent.getScenarioUrl(scenarioName);
	}
	
	/**
	 * Perform get.
	 * @param storeClient
	 * @param externalDatasetUrl
	 * @param resourceUrl
	 * @param copyOnRead
	 * @param accept
	 * @return
	 */
	public String getFromKnowledgeBase(ScenarioStoreClient storeClient, String externalDatasetUrl, String resourceUrl, boolean copyOnRead, String accept) {
		if (storeClient.exists(resourceUrl)) {
			return storeClient.get(resourceUrl, accept);
		} 
		
		String content = AccessAgentCaller.get(externalDatasetUrl, resourceUrl, accept);
		if (copyOnRead) {
			storeClient.put(resourceUrl, content, accept);
			if (accept != null) {
				// read it again but this time form the knowledge base and in the correct format
				return storeClient.get(resourceUrl, accept);
			} 
		}
		return content;
	}
	
	public String queryKnowledgeBase(ScenarioStoreClient storeClient, String resourceUrl, String sparql, boolean copyOnRead) {

		logger.info("queryKnowledgeBase");
		
		String metadatasetUrl = MetaDataAnnotator.getMetadataSetUrl();
		if ((resourceUrl != null) && resourceUrl.equals(metadatasetUrl)) {
			// will not work with current knowledge base implementation for Fuseki
			//String datasetUrl = kb.getDatasetUrl();
			//return KnowledgeBaseClient.query(metadatasetUrl, datasetUrl, sparql);
			return AccessAgentCaller.query(metadatasetUrl, null, sparql);
		}
		
		if (storeClient.exists(resourceUrl)) {
			return storeClient.query(resourceUrl, sparql);
		} 
		
		if (copyOnRead) {
			String content = AccessAgentCaller.get(null, resourceUrl, null);
			storeClient.put(resourceUrl, content, null);
			return storeClient.query(resourceUrl, sparql);
		} else {
			logger.info("query from KnowledgeBaseClient");	
			return AccessAgentCaller.query(null, resourceUrl, sparql);
		}
	}
	
	@Override
	public void put(JSONObject requestParams) {
		
		String requestUrl =  MiscUtil.optNullKey(requestParams, JPSConstants.REQUESTURL);
		String sparqlquery = MiscUtil.optNullKey(requestParams, JPSConstants.QUERY_SPARQL_QUERY);
		String sparqlupdate = MiscUtil.optNullKey(requestParams, JPSConstants.QUERY_SPARQL_UPDATE);
		String paramResourceUrl= MiscUtil.optNullKey(requestParams,JPSConstants.SCENARIO_RESOURCE);
		String contentType = MiscUtil.optNullKey(requestParams, JPSConstants.CONTENTTYPE);
		
		if(sparqlquery!=null && sparqlupdate!=null) {
	    	throw new JPSRuntimeException("parameters " + JPSConstants.QUERY_SPARQL_QUERY + " and " 
	    									+ JPSConstants.QUERY_SPARQL_UPDATE + " are not allowed");
	    }
		
		try {
			logInputParams(requestParams, null, false);
			
			String scenarioUrl = getDatasetUrl(requestUrl); //TODO check this, getScenarioUrl?
			ScenarioStoreClient storeClient = new ScenarioStoreClient(scenarioUrl);
			String resourceUrl = getResourceUrl(scenarioUrl, requestUrl, paramResourceUrl);
			String body = MiscUtil.optNullKey(requestParams, JPSConstants.CONTENT);
			
			storeClient.put(resourceUrl, body, contentType);

		} catch (RuntimeException e) {
			logInputParams(requestParams, null, true);
			throw new JPSRuntimeException(e);
		}
	}
	
	@Override
	public void post(JSONObject requestParams) {
		
		String requestUrl =  MiscUtil.optNullKey(requestParams, JPSConstants.REQUESTURL);
		String sparqlquery = MiscUtil.optNullKey(requestParams, JPSConstants.QUERY_SPARQL_QUERY);
		String sparqlupdate = MiscUtil.optNullKey(requestParams, JPSConstants.QUERY_SPARQL_UPDATE);
		String paramResourceUrl= MiscUtil.optNullKey(requestParams,JPSConstants.SCENARIO_RESOURCE);
		
		if(sparqlquery != null) {
			throw new JPSRuntimeException("parameter " + JPSConstants.QUERY_SPARQL_QUERY + " is not allowed");
		}
		
		try {
			logInputParams(requestParams, sparqlupdate, false);
			
			if (sparqlupdate == null) {
				throw new JPSRuntimeException("parameter " + JPSConstants.QUERY_SPARQL_UPDATE + " is missing");
			}
			
			String scenarioUrl = getDatasetUrl(requestUrl); //TODO check this, getScenarioUrl?
			ScenarioStoreClient storeClient = new ScenarioStoreClient(scenarioUrl);
			String resourceUrl = getResourceUrl(scenarioUrl, requestUrl, paramResourceUrl);

			updateKnowledgeBase(storeClient, resourceUrl, sparqlupdate);
			
		} catch (RuntimeException e) {
			logInputParams(requestParams, sparqlupdate, true);
			throw new JPSRuntimeException(e);
		}
	}
	
	protected void updateKnowledgeBase(ScenarioStoreClient storeClient, String resourceUrl, String sparql) {
		
		logger.info("updateKnowledgeBase");
		
		String metadatasetUrl = MetaDataAnnotator.getMetadataSetUrl();
		
		if ((resourceUrl != null) && resourceUrl.equals(metadatasetUrl)) {
			// at the moment, the knowledge base impl for Fuseki does not support SPARQL update if the named graph is given as extra resource URL
			// KnowledgeBaseClient.update(metadatasetUrl, datasetUrl, sparql);
			// for this reason, we have to set resource URL to null and use the GRAPH clause within the SPARQL update string itself 
			// (which is done bz the MetaDataAnnotator)
			AccessAgentCaller.update(metadatasetUrl, null, sparql);
			return;
		}
		
		if (!storeClient.exists(resourceUrl)) {
			String content = AccessAgentCaller.get(null, resourceUrl, null);
			storeClient.put(resourceUrl, content, null);
		}
		storeClient.update(resourceUrl, sparql);
	}
	
	private void setOptions(JSONObject jo, String scenarioName, ScenarioLog log) {
		
		JSONObject message = new JSONObject().put("operation", "option");
		
		if (jo.has(JPSConstants.SCENARIO_OPTION_COPY_ON_READ)) {
			message.put(JPSConstants.SCENARIO_OPTION_COPY_ON_READ, jo.getBoolean(JPSConstants.SCENARIO_OPTION_COPY_ON_READ));
		}
		
		log.logMessage(scenarioName, message);
	}
	
	public String call(JSONObject jo, String scenarioName, ScenarioLog log) {
		System.out.println("CALL METHOD " + "1: ");
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
		//System.out.println("operation: "+ operation  + " scenario name "+ scenarioName + " jo: "+ jo);
		System.out.println("CALL METHOD " + "2: ");
		System.out.println("jo " + jo);
		System.out.println("scenarioname " + scenarioName);
		System.out.println("operation " + operation);
	 	if (operation.startsWith("http")) {
	 		
	 		result = ScenarioManagementAgent.execute(scenarioName, operation, jo);
			System.out.println("CALL METHOD " + "3: ");
			System.out.println("jo " + jo);
			System.out.println("scenarioname " + scenarioName);
			System.out.println("operation " + operation);
			System.out.println("result " + result);
	 	} else {
	 		//throw new RuntimeException("can't call operation without http, operation = " + operation);
	 		 ScenarioManagementAgent.addJpsContext(scenarioName, jo);
	 		System.out.println("CALL METHOD " + "4: ");
	 		
	 		 //test to see if it was really put into context
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
		System.out.println("CALL METHOD " + "5: ");
		System.out.println("jo " + jo);
		System.out.println("scenarioname " + scenarioName);
		System.out.println("operation " + operation);
		System.out.println("result " + result);
		System.out.println("joresult " + joresult);
		System.out.println("message " + message);
		
		return result;
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

		return new QueryBroker().readFileLocal(resource);
	}
	
	private String queryFile(JSONObject jo, String scenarioName, boolean copyOnRead) {
		
		String resource = getResourcePath(jo, scenarioName, copyOnRead);
		String sparqlQuery = jo.getString(JPSConstants.QUERY_SPARQL_QUERY);
		
		logger.debug("sparqlquery=" + sparqlQuery);
		
		return new QueryBroker().queryFileOld(resource, sparqlQuery); //swap to queryFileOLD due to queryFil location not including http check and used within bucket
	}
	
	private void updateFile(JSONObject jo, String scenarioName) {
		//TODO: This method isn't working; it's calling on KBA's update in the old code. 
		String resource = getResourcePath(jo, scenarioName, true);
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
		// However, it does not contain the parameter types and output parameters that the composition engines needs to know. 
		// Thus, we have to add the IRI of composedAgent.
		String composedAgent = ScenarioMockManager.getLatestMockedAgent(log);
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
		logger.info("scenarioURL= "+scenarioUrl);
		logger.info("completePathWithinBucket= "+completePathWithinBucket);
		logger.debug("get resource path for resource=" + resource + ", in bucket=" + completePathWithinBucket + ", copyToBucket=" + copyToBucket);
		
		File fileWithinBucket = new File(completePathWithinBucket);
		if (fileWithinBucket.exists()) {
			return completePathWithinBucket;
		}
		if (copyToBucket && !fileWithinBucket.exists()) {
			String content;
			UrlValidator urlValidator = new UrlValidator();

			try {
				if (urlValidator.isValid(resource)) {
					content = new QueryBroker().readFile(resource);
				} else {
					//assuming regular file
					content = new QueryBroker().readFileLocal(resource);
				}
				if (!content.isEmpty()) {
					FileUtil.writeFileLocally(completePathWithinBucket, content);
					resource = completePathWithinBucket;
				}
			} catch (Exception ex) {
				throw new JPSRuntimeException(ex);
			}
		}

	    //copyToBucket is false: i.e. return the IRI directly. 
	    return resource;
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
