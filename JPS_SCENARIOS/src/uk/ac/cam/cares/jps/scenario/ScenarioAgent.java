package uk.ac.cam.cares.jps.scenario;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.util.Date;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.ws.rs.BadRequestException;
import javax.ws.rs.core.Response;

import org.apache.commons.io.FileUtils;
import org.apache.commons.validator.routines.UrlValidator;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.client.methods.HttpPut;
import org.eclipse.rdf4j.rio.RDFFormat;
import org.json.JSONException;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import uk.ac.cam.cares.jps.base.annotate.MetaDataAnnotator;
import uk.ac.cam.cares.jps.base.config.JPSConstants;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.http.Http;
import uk.ac.cam.cares.jps.base.query.KnowledgeBaseClient;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.base.scenario.BucketHelper;
import uk.ac.cam.cares.jps.base.scenario.JPSContext;
import uk.ac.cam.cares.jps.base.scenario.ScenarioHelper;
import uk.ac.cam.cares.jps.base.util.FileUtil;
import uk.ac.cam.cares.jps.base.util.MiscUtil;
import uk.ac.cam.cares.jps.scenario.kb.KnowledgeBaseAbstract;
import uk.ac.cam.cares.jps.scenario.kb.KnowledgeBaseAgent;
import uk.ac.cam.cares.jps.scenario.kb.KnowledgeBaseManager;

@WebServlet(urlPatterns = {"/scenario/*"})
public class ScenarioAgent extends KnowledgeBaseAgent {
	
	private static final long serialVersionUID = 3746092168199681624L;

	private static Logger logger = LoggerFactory.getLogger(ScenarioAgent.class);
	
	@Override
	public JSONObject processRequestParameters(JSONObject requestParams) {
		return new JSONObject();
	}
	/**
	 * @see HttpServlet#doGet(HttpServletRequest request, HttpServletResponse response)
	 */
	protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		
		JSONObject requestParams = AgentCaller.readJsonParameter(request);
		
		String path = request.getPathInfo();
		logger.debug("called for path=" + path);
		
		String[] parts = ScenarioHelper.dividePath(path);
		String scenarioName = parts[0]; //name of scenario: base, testNuclear
		String operation = parts[1];

		System.out.println("JSONPARAMS: "+ requestParams.toString());
		System.out.println("\noperation: "+operation);
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
			
//			usecaseurl = BucketHelper.getUsecaseUrl(scenariourl);
//			JPSContext.putUsecaseUrl(jo, usecaseurl);
//			
//			logger.info("scenariourl now= "+scenariourl + " usecaseurl now= "+usecaseurl); 
//			logger.info("context now= " + JPSContext.getJpsContext());
//			logger.info("JSON now= " + jo.toString());
		}

				
		ScenarioLog log = ScenarioManagementAgent.getScenarioLog(scenarioName);
		boolean copyOnRead = ScenarioManagementAgent.getCopyOnRead(log);
		
		String result = "";
		if (operation == null) {
			
			if (requestParams.has(JPSConstants.SCENARIO_RESOURCE)) {
				
				doGetNew(request, response, scenarioName, copyOnRead);
				return;
				
//				if (jo.has(JPSConstants.QUERY_SPARQL_QUERY)) {
//					result = queryFile(jo, scenarioName, copyOnRead);
//				} else {
//					result = readFile(jo, scenarioName, copyOnRead);
//				}
			} else {
			
				// just return the scenario log
				JSONObject resultjo = new JSONObject(log.getLogAsString());
				// pretty print with 2 spaces to indent
				result = resultjo.toString(2);
			}
		
		} else if ("/option".equals(operation)) {
			
			setOptions(requestParams, scenarioName, log);

		} else if ("/mock".equals(operation)) {
			
			new ScenarioMockManager().mock(requestParams, scenarioName, log);
		
		} else if ("/call".equals(operation)) {

			result = call(requestParams, scenarioName, log);
				
		} else if ("/read".equals(operation)) {
			
			result = readFile(requestParams, scenarioName, copyOnRead);
			
		} else if ("/query".equals(operation)) {
			
			result = queryFile(requestParams, scenarioName, copyOnRead);
			
		} else if ("/update".equals(operation)) {

			updateFile(requestParams, scenarioName);
			
		} else if ("/delete".equals(operation)) {
			
			deleteScenario(scenarioName);
			
		} else if ("/compose".equals(operation)) {
			
			result = compose(requestParams, scenarioName, log);

		} else if ("/preparerecording".equals(operation)) {
			
			result = prepareRecording(requestParams, scenarioName, log);
			
		} else if ("/ping".equals(operation)) {
			
			result = new Date().toString();
			
		} else if ("/mergescenario".equals(operation)) {
			
			mergeScenario(requestParams, scenarioName, log);
			
		} else {
			
			if (operation.startsWith("/" + JPSConstants.SCENARIO_SUBDIR_DATA + "/") 
					|| operation.startsWith("/" + JPSConstants.SCENARIO_SUBDIR_KB + "/")) {
				String localPath = ScenarioHelper.getScenarioBucket(scenarioName) + operation;
				result = FileUtil.readFileLocally(localPath);
			} else {
				
				result = new ScenarioMockManager().execute(requestParams, scenarioName, operation, log);
			}
		}
		
		AgentCaller.printToResponse(result, response);
	}
	private void doGetNew(HttpServletRequest req, HttpServletResponse resp, String scenarioName, boolean copyOnRead) 
			throws ServletException, IOException {
		
		String requestUrl = req.getRequestURL().toString();
		String path = req.getPathInfo();
		JSONObject input = Http.readJsonParameter(req);
		String sparql = MiscUtil.optNullKey(input, JPSConstants.QUERY_SPARQL_QUERY);
		String paramDatasetUrl = MiscUtil.optNullKey(input, JPSConstants.SCENARIO_DATASET);
		String paramResourceUrl = MiscUtil.optNullKey(input, JPSConstants.SCENARIO_RESOURCE);
		String contentType = req.getContentType();
		String accept = MiscUtil.optNullKey(input, "acceptHeaders");
		try {
			logInputParams("GET", requestUrl, path, paramDatasetUrl, paramResourceUrl, contentType, sparql, false);
			
			
			String scenarioUrl = ScenarioManagementAgent.getScenarioUrl(scenarioName);
			KnowledgeBaseAbstract kb = KnowledgeBaseManager.getKnowledgeBase(scenarioUrl);
			String resourceUrl = getResourceUrl(scenarioUrl, requestUrl, paramResourceUrl);
			
			String result = "";	
			if (sparql == null) {
				//result = kb.get(resourceUrl, accept);
				result = getFromKnowledgeBase(kb, paramDatasetUrl, resourceUrl, copyOnRead, accept);
			} else {
				//result = kb.query(resourceUrl, sparql);
				result = queryKnowledgeBase(kb, resourceUrl, sparql, copyOnRead);
			}
			
			Http.printToResponse(result, resp);

		} catch (RuntimeException e) {
			logInputParams("GET", requestUrl, path, paramDatasetUrl, paramResourceUrl, contentType, sparql, true);
			throw e;
		}
	}
	@Override
    public JSONObject processRequestParameters(JSONObject requestParams, HttpServletRequest request) {	
		if (!validateInput(requestParams)) {
			throw new JSONException("ScenarioAgent: Input parameters not found.\n ");
		}
		//TODO: To be fixed with a proper POST or PUT method later on
        String method = MiscUtil.optNullKey(requestParams, JPSConstants.METHOD);
		System.out.println("METHOD: "+ method);
        if (method.equals(HttpPost.METHOD_NAME) || method.equals(HttpPut.METHOD_NAME)) {
        	doPutOrPost(requestParams);
        	return new JSONObject();
        }
        else {
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
					result = getNew(request, scenarioName, copyOnRead);
					return jo.put("result", result);
					} else {
				
					// just return the scenario log
					JSONObject resultjo = new JSONObject(log.getLogAsString());
					// pretty print with 2 spaces to indent
	//				result = resultjo.toString(2);
					return resultjo;
					
				}
			}
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
						result = FileUtil.readFileLocally(localPath);
					} 
					else  {					
						result = new ScenarioMockManager().execute(requestParams, scenarioName, operation, log);
					}
			}
			return jo.put("result", result);
        }
	}
	protected void doHttpJPS(HttpServletRequest request, HttpServletResponse response) throws IOException, ServletException {
    	setLogger();
        try {
            String responseBody = getResponseBody(request);
            JSONObject jo = new JSONObject(responseBody);
            if (jo.has("result")) {
                response.getWriter().write(jo.getString("result"));
            }else {
            	response.getWriter().write(responseBody);
            }
        } catch (BadRequestException e) {
            response.setStatus(Response.Status.BAD_REQUEST.getStatusCode());
        } catch (JPSRuntimeException e) {
        	response.setStatus(Response.Status.SERVICE_UNAVAILABLE.getStatusCode());
        }
    }
	/** to handle doPut OR doPost methods since ScenarioAgent originally called on KBA's put/post methods 
	 * 
	 * @param requestParams
	 */
	protected void doPutOrPost(JSONObject requestParams) {
		String paramResourceUrl= MiscUtil.optNullKey(requestParams,JPSConstants.SCENARIO_RESOURCE);
        String sparql = MiscUtil.optNullKey(requestParams, JPSConstants.QUERY_SPARQL_UPDATE);

		String requestUrl = MiscUtil.optNullKey(requestParams, JPSConstants.REQUESTURL);
		String datasetUrl = KnowledgeBaseManager.getDatasetUrl(requestUrl);
		KnowledgeBaseAbstract kb = KnowledgeBaseManager.getKnowledgeBase(datasetUrl);
		String resourceUrl = getResourceUrl(datasetUrl, requestUrl, paramResourceUrl);
        String method = MiscUtil.optNullKey(requestParams, JPSConstants.METHOD);
		switch (method) {
			case HttpPost.METHOD_NAME:
				if (sparql == null) {
					throw new JPSRuntimeException("parameter " + JPSConstants.QUERY_SPARQL_UPDATE + " is missing");
				}
				kb.update(resourceUrl, sparql);
				break;
			case HttpPut.METHOD_NAME:
				String body = MiscUtil.optNullKey(requestParams, JPSConstants.CONTENT);
				String contentType = MiscUtil.optNullKey(requestParams, JPSConstants.CONTENTTYPE);
				if (sparql != null) {
					throw new JPSRuntimeException("parameter " + JPSConstants.QUERY_SPARQL_UPDATE + " is not allowed");
				}    			
				kb.put(resourceUrl, body, contentType);
				break;
		  
			}		
	}
	private void setOptions(JSONObject jo, String scenarioName, ScenarioLog log) {
		
		JSONObject message = new JSONObject().put("operation", "option");
		
		if (jo.has(JPSConstants.SCENARIO_OPTION_COPY_ON_READ)) {
			message.put(JPSConstants.SCENARIO_OPTION_COPY_ON_READ, jo.getBoolean(JPSConstants.SCENARIO_OPTION_COPY_ON_READ));
		}
		
		log.logMessage(scenarioName, message);
	}
	
	private String call(JSONObject jo, String scenarioName, ScenarioLog log) {
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
	
	protected void updateKnowledgeBase(KnowledgeBaseAbstract kb, String resourceUrl, String sparql) {
		
		logger.info("updateKnowledgeBase");
		
		String datasetUrl = kb.getDatasetUrl();
		String metadatasetUrl = MetaDataAnnotator.getMetadataSetUrl();
		
		if ((resourceUrl != null) && resourceUrl.equals(metadatasetUrl)) {
			// at the moment, the knowledge base impl for Fuseki does not support SPARQL update if the named graph is given as extra resource URL
			// KnowledgeBaseClient.update(metadatasetUrl, datasetUrl, sparql);
			// for this reason, we have to set resource URL to null and use the GRAPH clause within the SPARQL update string itself 
			// (which is done bz the MetaDataAnnotator)
			KnowledgeBaseClient.update(metadatasetUrl, null, sparql);
			return;
		}
		
		if (!kb.exists(resourceUrl)) {
			String content = KnowledgeBaseClient.get(null, resourceUrl, null);
			kb.put(resourceUrl, content, null);
		}
		kb.update(resourceUrl, sparql);
	}
	
	protected String getFromKnowledgeBase(KnowledgeBaseAbstract kb, String externalDatasetUrl, String resourceUrl, boolean copyOnRead, String accept) {
		if (kb.exists(resourceUrl)) {
			return kb.get(resourceUrl, accept);
		} 
		
		String content = KnowledgeBaseClient.get(externalDatasetUrl, resourceUrl, accept);
		if (copyOnRead) {
			kb.put(resourceUrl, content, accept);
			if (accept != null) {
				// read it again but this time form the knowledge base and in the correct format
				return kb.get(resourceUrl, accept);
			} 
		}

		return content;
	}
	
	protected String queryKnowledgeBase(KnowledgeBaseAbstract kb, String resourceUrl, String sparql, boolean copyOnRead) {

		logger.info("queryKnowledgeBase");
		
		String metadatasetUrl = MetaDataAnnotator.getMetadataSetUrl();
		if ((resourceUrl != null) && resourceUrl.equals(metadatasetUrl)) {
			// will not work with current knowledge base implementation for Fuseki
			//String datasetUrl = kb.getDatasetUrl();
			//return KnowledgeBaseClient.query(metadatasetUrl, datasetUrl, sparql);
			return KnowledgeBaseClient.query(metadatasetUrl, null, sparql);
		}
		
		if (kb.exists(resourceUrl)) {
			return kb.query(resourceUrl, sparql);
		} 
		
		String content = KnowledgeBaseClient.get(null, resourceUrl, null);
		if (copyOnRead) {
			kb.put(resourceUrl, content, null);
			return kb.query(resourceUrl, sparql);
		} else {
			logger.info("query from KnowledgeBaseAbstract");
			InputStream inputStream = FileUtil.stringToInputStream(content);
			RDFFormat format =  KnowledgeBaseAbstract.getRDFFormatFromFileType(resourceUrl);
			return KnowledgeBaseAbstract.query(inputStream, format, sparql);
		}
	}
	
	private String getNew(HttpServletRequest req, String scenarioName, boolean copyOnRead) 
			{
		
		String requestUrl = req.getRequestURL().toString();
		String path = req.getPathInfo();
		JSONObject input = Http.readJsonParameter(req);
		String sparql = MiscUtil.optNullKey(input, JPSConstants.QUERY_SPARQL_QUERY);
		String paramDatasetUrl = MiscUtil.optNullKey(input, JPSConstants.SCENARIO_DATASET);
		String paramResourceUrl = MiscUtil.optNullKey(input, JPSConstants.SCENARIO_RESOURCE);
		String contentType = req.getContentType();
		String accept = MiscUtil.optNullKey(input, "acceptHeaders");
		try {
			logInputParams("GET", requestUrl, path, paramDatasetUrl, paramResourceUrl, contentType, sparql, false);
			
			
			String scenarioUrl = ScenarioManagementAgent.getScenarioUrl(scenarioName);
			KnowledgeBaseAbstract kb = KnowledgeBaseManager.getKnowledgeBase(scenarioUrl);
			String resourceUrl = getResourceUrl(scenarioUrl, requestUrl, paramResourceUrl);
			
			String result = "";	
			if (sparql == null) {
				//result = kb.get(resourceUrl, accept);
				result = getFromKnowledgeBase(kb, paramDatasetUrl, resourceUrl, copyOnRead, accept);
			} else {
				//result = kb.query(resourceUrl, sparql);
				result = queryKnowledgeBase(kb, resourceUrl, sparql, copyOnRead);
			}
			
			return result;

		} catch (RuntimeException e) {
			e.printStackTrace();
			logInputParams("GET", requestUrl, path, paramDatasetUrl, paramResourceUrl, contentType, sparql, true);
			throw e;
		}
	}
}