package uk.ac.cam.cares.jps.base.query;

import java.io.FileOutputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;

import org.apache.jena.ontology.OntModel;
import org.apache.jena.query.ResultSet;
import org.apache.jena.update.UpdateAction;
import org.apache.jena.update.UpdateFactory;
import org.apache.jena.update.UpdateRequest;
import org.apache.logging.log4j.ThreadContext;
import org.json.JSONStringer;

import uk.ac.cam.cares.jps.base.config.JPSConstants;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.log.JPSBaseLogger;
import uk.ac.cam.cares.jps.base.scenario.ScenarioHelper;

public class QueryBroker {
	
	// TODO-AE SC 20190321 all methods should be extended in such a way that scenario url might passed
	// directly as a parameter (instead of using the super class JPSHttpServlet and ThreadContext)
	public String readFile(String urlOrPath) {
		
		String scenarioURL = ThreadContext.get(JPSConstants.SCENARIO_URL);	
		JPSBaseLogger.info(this, "reading file for urlOrPath=" + urlOrPath + ", scenarioURL=" + scenarioURL);
		
		// call the scenario agent if a scenario url is set in the input
		// the scenario agent has to be called even for copy-on-write since in the past
		// another agent might have updated the file within the same scenario 
		if (scenarioURL != null) {
			String url = scenarioURL + "/read";
			
			String json = new JSONStringer().object()
					.key(JPSConstants.SCENARIO_RESOURCE).value(urlOrPath)
					.endObject().toString();
			
			return AgentCaller.executeGetWithURLAndJSON(url, json);
		}
		
		urlOrPath = ResourcePathConverter.convert(urlOrPath);
		
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
			return readFileLocally(localFile);
		}
	}
	
	public String readFileLocally(String path) {
		
		try {
			return new String(Files.readAllBytes(Paths.get(path)));
		} catch (IOException e) {
			throw new JPSRuntimeException(e.getMessage(), e);
		}
	}
		
	public String queryFile(String urlOrPath, String sparqlQuery) {
		
		String scenarioURL = ThreadContext.get(JPSConstants.SCENARIO_URL);	
		JPSBaseLogger.info(this, "querying file for urlOrPath=" + urlOrPath + ", scenarioURL=" + scenarioURL);
		
		// call the scenario agent if a scenario url is set in the input
		// the scenario agent has to be called even for copy-on-write since in the past
		// another agent might have updated the file within the same scenario 
		if (scenarioURL != null)  {
			String url = scenarioURL + "/query";
			
			String json = new JSONStringer().object()
					.key(JPSConstants.SCENARIO_RESOURCE).value(urlOrPath)
					.key(JPSConstants.QUERY_SPARQL_QUERY).value(sparqlQuery)
					.endObject().toString();
			
			return AgentCaller.executeGetWithURLAndJSON(url, json);
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
	
	/**
	 * @param urlOrPath
	 * @param content
	 * @return the URL to access the written file
	 */
	public String writeFile(String urlOrPath, String content) {
		
		String scenarioURL = ThreadContext.get(JPSConstants.SCENARIO_URL);	
		JPSBaseLogger.info(this, "writing file for urlOrPath=" + urlOrPath + ", scenarioURL=" + scenarioURL);
		
		if (scenarioURL != null) {
			
			int i = scenarioURL.lastIndexOf("/");
			String scenarioName = scenarioURL.substring(i);
			String scenarioBucket = ScenarioHelper.getScenarioBucket(scenarioName);
			String path = ScenarioHelper.getFileNameWithinBucket(urlOrPath, scenarioBucket);

			// Don't use HTTP GET to call the scenario agent as is done in method readFile().
			// In future, HTTP PUT might be used to call the scenario agent on a different server for writing a file.
			// However, so far the content is written locally.
			writeFileLocally(path, content);
			return path;
		}
		
		if (urlOrPath.startsWith("http")) {
			// In future, HTTP PUT might be used to write a file on a remote server.
			throw new JPSRuntimeException("writing file via HTTP is not supported yet, urlOrPath=" + urlOrPath);
		}
		
		writeFileLocally(urlOrPath, content);
		return urlOrPath;
	}
	
	public static void writeFileLocally(String path, String content) {
		
	    FileWriter fileWriter = null;
	    
	    try {
			fileWriter = new FileWriter(path);
			fileWriter.write(content);
			fileWriter.close();
		} catch (IOException e) {
			throw new JPSRuntimeException(e.getMessage(), e);
		}   
	}
	
	public static void writeFileLocally2(String path, String content) {
		FileOutputStream outputStream = null;
	    try {
		    outputStream = new FileOutputStream(path);
		    byte[] strToBytes = content.getBytes();
			outputStream.write(strToBytes);
		} catch (IOException e) {
			throw new JPSRuntimeException(e.getMessage(), e);
		} finally {
			try {
				outputStream.close();
			} catch (IOException e) {
				throw new JPSRuntimeException(e.getMessage(), e);
			}
		}
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
	public void updateFile(String urlOrPath, String sparqlUpdate) {
		
		String scenarioURL = ThreadContext.get(JPSConstants.SCENARIO_URL);	
		JPSBaseLogger.info(this, "updating file for urlOrPath=" + urlOrPath + ", scenarioURL=" + scenarioURL);
		
		
		// TODO-AE SC 20190218 URGENT
		// call the scenario agent if a scenario url is set in the input
		if (scenarioURL != null)  {
			String url = scenarioURL + "/update";
			
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
		
		urlOrPath = ResourcePathConverter.convert(urlOrPath);
		
		String localFile = urlOrPath;
		if (urlOrPath.startsWith("http")) {
			localFile = ScenarioHelper.cutHash(localFile);
			localFile = ResourcePathConverter.convertToLocalPath(localFile);
		}
		
		JPSBaseLogger.info(this, "updating local file=" + localFile);
		
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
}
