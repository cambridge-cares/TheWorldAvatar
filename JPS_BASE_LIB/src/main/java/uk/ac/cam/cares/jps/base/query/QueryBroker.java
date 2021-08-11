package uk.ac.cam.cares.jps.base.query;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import org.apache.jena.ontology.OntModel;
import org.apache.jena.query.ResultSet;
import org.apache.jena.update.UpdateAction;
import org.apache.jena.update.UpdateFactory;
import org.apache.jena.update.UpdateRequest;
import org.json.JSONArray;
import org.json.JSONObject;
import org.json.JSONStringer;

import uk.ac.cam.cares.jps.base.config.IKeys;
import uk.ac.cam.cares.jps.base.config.JPSConstants;
import uk.ac.cam.cares.jps.base.config.KeyValueManager;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.log.JPSBaseLogger;
import uk.ac.cam.cares.jps.base.scenario.BucketHelper;
import uk.ac.cam.cares.jps.base.scenario.JPSContext;
import uk.ac.cam.cares.jps.base.scenario.ScenarioClient;
import uk.ac.cam.cares.jps.base.scenario.ScenarioHelper;
import uk.ac.cam.cares.jps.base.util.FileUtil;

public class QueryBroker {
	
	public String readFile(String url) {
		if (!url.startsWith("http")) {
			throw new JPSRuntimeException("destinationUrl must be a URL");
		}
		String result = AccessAgentCaller.get(null, url, null);
		return result;
	}
	
	public String readFileLocal(String path) {
		if (path.startsWith("http")) {
			throw new JPSRuntimeException("destinationUrl must not be a URL");
		}
		String localFile = ScenarioHelper.cutHash(path);
		return FileUtil.readFileLocally(localFile);
	}
	
	// TODO-AE SC 20190321 all methods should be extended in such a way that scenario url might passed
	// directly as a parameter (instead of using the super class JPSHttpServlet and ThreadContext)
	public String readFileOLD(String urlOrPath) {
		
		String scenarioUrl = JPSContext.getScenarioUrl();	
		JPSBaseLogger.info(this, "reading file for urlOrPath=" + urlOrPath + ", scenarioUrl=" + scenarioUrl);
		
		// TODO-AE SC 20190416 this is just a hack to read local file, refactor this method
		if (!urlOrPath.startsWith("http")) {
			String localFile = ScenarioHelper.cutHash(urlOrPath);
			return FileUtil.readFileLocally(localFile);
		}
		
		// call the scenario agent if a scenario url is set in the input
		// the scenario agent has to be called even for copy-on-write since in the past
		// another agent might have updated the file within the same scenario 
		if (scenarioUrl != null) {
			return new ScenarioClient().read(scenarioUrl, urlOrPath);
		}
		
//		urlOrPath = ResourcePathConverter.convert(urlOrPath);
		
		if (urlOrPath.startsWith("http")) {
		
			// Apache HTTP client applies percentage encoding to any URL.
			// Usually, this is not a problem when requesting an OWL file. 
			// But if requesting http://www.theworldavatar.com/kb/agents/Service__OpenWeatherMap.owl#Service 
			// then percentage encoding results into http://www.theworldavatar.com/kb/agents/Service__OpenWeatherMap.owl%23Service
			// and a consecutive Tomcat error.
			// To avoid %23 instead of #, we simply skip the #-part.
			int i = urlOrPath.lastIndexOf("#");
			if (i >= 0) {
				urlOrPath = urlOrPath.substring(0, i);
			}
			
			return AgentCaller.executeGetWithURL(urlOrPath);
		} else {
			String localFile = ScenarioHelper.cutHash(urlOrPath);
			return FileUtil.readFileLocally(localFile);
		}
	}
	
	public String queryFile(String targetUrl, String sparqlQuery) {
		String result = AccessAgentCaller.query(null, targetUrl, sparqlQuery);
		return result;
	}

	public String queryFileOld(String urlOrPath, String sparqlQuery) {
		
		String scenarioUrl = JPSContext.getScenarioUrl();
		JPSBaseLogger.info(this, "querying file for urlOrPath=" + urlOrPath + ", scenarioUrl=" + scenarioUrl);
		
		// call the scenario agent if a scenario url is set in the input
		// the scenario agent has to be called even for copy-on-write since in the past
		// another agent might have updated the file within the same scenario 
		if (scenarioUrl != null)  {
			return new ScenarioClient().query(scenarioUrl, urlOrPath, sparqlQuery);
		}
		
		urlOrPath = ResourcePathConverter.convert(urlOrPath);
		
		ResultSet resultSet = null;
		if (urlOrPath.startsWith("http")) {
			resultSet = JenaHelper.queryUrl(urlOrPath, sparqlQuery);
		} else {
			String localFile = ScenarioHelper.cutHash(urlOrPath);
			resultSet = JenaHelper.queryFile(localFile, sparqlQuery);
		}
		
		return JenaResultSetFormatter.convertToJSONW3CStandard(resultSet);
	}
	
	public OntModel readModelGreedy(String urlOrPath, String greedySparqlQuery) {
		String greedyResult = queryFile(urlOrPath, greedySparqlQuery);
		JSONObject jo = JenaResultSetFormatter.convertToSimplifiedList(greedyResult);
		JSONArray ja = jo.getJSONArray("results");
		
		List<String> nodesToVisit = new ArrayList<String>();
		for (int i=0; i<ja.length(); i++) {
			JSONObject row = ja.getJSONObject(i);
			for (String current : row.keySet()) {
				String potentialIri =  row.getString(current);
				if (potentialIri.startsWith("http")) {
					int index = potentialIri.lastIndexOf("#");
					if (index > 0) {
						potentialIri = potentialIri.substring(0, index);
					}
					if (!nodesToVisit.contains(potentialIri)) {
						nodesToVisit.add(potentialIri);
					} 
				}
			}
		}
		
		JPSBaseLogger.info(this, "number of nodes to visit for greedy sparql query = " + nodesToVisit.size());
		
		//OntModel model = ModelFactory.createOntologyModel();
		OntModel model = JenaHelper.createModel();
		int count=0;
		for (String current : nodesToVisit) {
			count++;
			if (count % 50 == 0) {
				JPSBaseLogger.info(this, "reading file number=" + count + ", name=" + current);
			}
			current = ResourcePathConverter.convert(current);
			JPSBaseLogger.info(this,"what is the current now?: "+current);
			model.read(current, null); 
		}
		
		return model;
	}
	
	public String queryFilesGreedy(String urlOrPath, String greedySparqlQuery, String secondSparqlQuery) {
		//TODO-AE SC URGENT 20190304 make queryFilesGreedy scenario capable
		OntModel model = readModelGreedy(urlOrPath, greedySparqlQuery);
		ResultSet result = JenaHelper.query(model, secondSparqlQuery);
		return JenaResultSetFormatter.convertToJSONW3CStandard(result);
	}
	
	public String writeFile(String urlOrPath, String content) {
		String result = AccessAgentCaller.put(null, urlOrPath, content, null);
		return result;
	}
	
	/**
	 * @param urlOrPath
	 * @param content
	 * @return the URL to access the written file
	 */
	public String writeFileOLD(String urlOrPath, String content) {
		
		String scenarioUrl = JPSContext.getScenarioUrl();	
		JPSBaseLogger.info(this, "writing file for urlOrPath=" + urlOrPath + ", scenarioUrl=" + scenarioUrl);
		
		if (scenarioUrl != null) {
			
			int i = scenarioUrl.lastIndexOf("/");
			String scenarioName = scenarioUrl.substring(i);
			String scenarioBucket = ScenarioHelper.getScenarioBucket(scenarioName);
			String path = ScenarioHelper.getFileNameWithinBucket(urlOrPath, scenarioBucket);

			// Don't use HTTP GET to call the scenario agent as is done in method readFile().
			// In future, HTTP PUT might be used to call the scenario agent on a different server for writing a file.
			// However, so far the content is written locally.
			FileUtil.writeFileLocally(path, content);
			return path;
		}
		
		if (urlOrPath.startsWith("http")) {
			// In future, HTTP PUT might be used to write a file on a remote server.
			throw new JPSRuntimeException("writing file via HTTP is not supported yet, urlOrPath=" + urlOrPath);
		}
		
		FileUtil.writeFileLocally(urlOrPath, content);
		return urlOrPath;
	}
	
	public void put(String destinationUrl, String content) {
		if (!destinationUrl.startsWith("http")) {
			throw new JPSRuntimeException("destinationUrl must be a URL");
		}
		AccessAgentCaller.put(null, destinationUrl, content, null);
	}
	
	public void putLocal(String destinationUrl, String content) {
		if (destinationUrl.startsWith("http")) {
			throw new JPSRuntimeException("destinationUrl must not be a URL");
		}
		FileUtil.writeFileLocally(destinationUrl, content);
	}
	
	public void putOld(String destinationUrl, String content) {
		
		//String scenarioUrl = ThreadContext.get(JPSConstants.SCENARIO_URL);	
		//JPSBaseLogger.info(this, "put for destinationUrl=" + destinationUrl + ", scenarioUrl=" + scenarioUrl);
		
		// TODO-AE SC 20190416 this is just a hack to read local file, refactor this method
		String path = destinationUrl;
		if (destinationUrl.startsWith("http")) {
			
			//AgentCaller.executePut(destinationUrl, content);
			
			String destinationUrlWithoutHash = ScenarioHelper.cutHash(destinationUrl);
			path = BucketHelper.getLocalPath(destinationUrlWithoutHash);
		}
		
		FileUtil.writeFileLocally(path, content);
	}
	
	public void put(String destinationUrl, File file) {
		if (!destinationUrl.startsWith("http")) {
			throw new JPSRuntimeException("destinationUrl must be a URL");
		}
		
		String content = FileUtil.readFileLocally(file.getAbsolutePath());
		put(destinationUrl, content);
	}
	
	public void putLocal(String destinationUrl, File file) {
		if (destinationUrl.startsWith("http")) {
			throw new JPSRuntimeException("destinationUrl must not be a URL");
		}
		String content = FileUtil.readFileLocally(file.getAbsolutePath());
		FileUtil.writeFileLocally(destinationUrl, content);
	}

	public void updateFile(String targetUrl, String sparqlUpdate) {
		AccessAgentCaller.update(null, targetUrl, sparqlUpdate);
	}
	
	/**
	 * Useful links for the question how to update with Jena for future purpose:<br>
	 * <br> 
	 * https://jena.apache.org/documentation/rdfconnection/ <br>
	 * https://jena.apache.org/documentation/javadoc/rdfconnection/org/apache/jena/rdfconnection/RDFConnection.html <br>
	 * https://jena.apache.org/documentation/javadoc/rdfconnection/org/apache/jena/rdfconnection/RDFConnectionFactory.html <br>
	 * https://github.com/apache/jena/blob/master/jena-rdfconnection/src/main/java/org/apache/jena/rdfconnection/examples/RDFConnectionExample1.java  <br>
	 * https://jena.apache.org/documentation/query/update.html <br>
	 * https://github.com/apache/jena/tree/master/jena-arq/src-examples/arq/examples/update   <br>
	 * https://jena.apache.org/documentation/javadoc/arq/org/apache/jena/update/UpdateExecutionFactory.html  <br>
	 * https://jena.apache.org/documentation/javadoc/arq/org/apache/jena/query/DatasetFactory.html   <br>
	 * https://stackoverflow.com/questions/16487746/jena-sparql-update-doesnt-execute   <br>
	 * https://stackoverflow.com/questions/13709698/sparql-update-query-over-local-files
	 * 
	 * 
	 * @param urlOrPath
	 * @param sparqlUpdate
	 */
	public void updateFileOLD(String urlOrPath, String sparqlUpdate) {
		
		String scenarioUrl = JPSContext.getScenarioUrl();	
		JPSBaseLogger.info(this, "updating file for urlOrPath=" + urlOrPath + ", scenarioUrl=" + scenarioUrl);
		
		
		// TODO-AE SC URGENT 20190218
		// call the scenario agent if a scenario url is set in the input
		if (scenarioUrl != null)  {
			String url = scenarioUrl + "/update";
			
			String json = new JSONStringer().object()
					.key(JPSConstants.SCENARIO_RESOURCE).value(urlOrPath)
					.key(JPSConstants.QUERY_SPARQL_UPDATE).value(sparqlUpdate)
					.endObject().toString();
			
			AgentCaller.executeGetWithURLAndJSON(url, json);
			return;
		}
		
		// TODO-AE SC for updating a remote file, first load the content and update it. But the update can only
		// be stored within scenarios. Later: Broker should find out, whether an QueryAgent (or another broker)
		// is running on remote server which has local access. 
		
//		urlOrPath = ResourcePathConverter.convert(urlOrPath);
		
		String localFile = urlOrPath;
		if (urlOrPath.startsWith("http")) {
			localFile = ScenarioHelper.cutHash(localFile);
			localFile = ResourcePathConverter.convertToLocalPath(localFile);
		}
		
		JPSBaseLogger.info(this, "updating local file=" + localFile);
		JPSBaseLogger.info(this, "SPARQL update =" + sparqlUpdate);
		
		UpdateRequest request = UpdateFactory.create(sparqlUpdate);
		OntModel model = JenaHelper.createModel(localFile);	
		UpdateAction.execute(request, model);
		JenaHelper.writeAsFile(model, localFile);
		
		// alternative:
//		UpdateRequest request = UpdateFactory.create(sparqlUpdate);
//		OntModel model = JenaHelper.createModel(localFile);	
//		Dataset dataset = DatasetFactory.wrap(model);
//		UpdateProcessor processor = UpdateExecutionFactory.create(request, dataset);
//		processor.execute();		
//		Model updatedModel = dataset.getDefaultModel();
//		JenaHelper.writeAsFile(updatedModel, localFile);
	}
	
	/**
	 * return something like "http://www.theworldavatar.com/jps/kb/<uuid>" or 
	 * "http://localhost:8080/jps/kb/<uuid>"
	 * 
	 * @return
	 */
	public static String getIriPrefix() {
		return BucketHelper.getIriPrefix();
	}
	
	public static String getLocalDataPath() {
		return BucketHelper.getLocalDataPath();
	}
	
	public static String getLocalDataPath(String humanReadableSubPath) {
		String separator = KeyValueManager.get(IKeys.SCENARIO_USECASEDIRECTORY_SEPARATOR);
		String path = BucketHelper.getLocalDataPath() + separator + humanReadableSubPath;
		return path;
	}
}
