package uk.ac.cam.cares.jps.base.query;

import java.io.IOException;
import java.net.URI;
import java.nio.file.Files;
import java.nio.file.Paths;

import org.apache.jena.ontology.OntModel;
import org.apache.jena.ontology.OntModelSpec;
import org.apache.jena.query.ResultSet;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.update.UpdateFactory;
import org.apache.jena.update.UpdateRequest;
import org.apache.logging.log4j.ThreadContext;
import org.json.JSONStringer;

import uk.ac.cam.cares.jps.base.config.KeyValueServer;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.log.LogServer;

public class QueryBroker {
	
	public String readFile(String urlOrPath) {
		
		String scenarioURL = ThreadContext.get(ScenarioKeys.SCENARIO_URL);	
		LogServer.debug(this, "reading file for urlOrPath=" + urlOrPath + ", scenarioURL=" + scenarioURL);
		
		// call the scenario agent if a scenario url is set in the input
		// the scenario agent has to be called even for copy-on-write since in the past
		// another agent might have updated the file within the same scenario 
		if (scenarioURL != null) {
			String url = scenarioURL + "/read";
			
			String json = new JSONStringer().object()
					.key(ScenarioKeys.SCENARIO_RESOURCE).value(urlOrPath)
					.endObject().toString();
			
			return AgentCaller.executeGetWithURLAndJSON(url, json);
		}
		
		if (urlOrPath.startsWith("http")) {
			return AgentCaller.executeGetWithURL(urlOrPath);
		} else {
			return readFileLocally(urlOrPath);
		}
	}
	
	private String readFileLocally(String path) {
		
		try {
			return new String(Files.readAllBytes(Paths.get(path)));
		} catch (IOException e) {
			throw new JPSRuntimeException(e.getMessage(), e);
		}
	}
	
	public String queryFile(String urlOrPath, String sparqlQuery) {
		
		String scenarioURL = ThreadContext.get(ScenarioKeys.SCENARIO_URL);	
		LogServer.debug(this, "querying file for urlOrPath=" + urlOrPath + ", scenarioURL=" + scenarioURL);
		
		// call the scenario agent if a scenario url is set in the input
		// the scenario agent has to be called even for copy-on-write since in the past
		// another agent might have updated the file within the same scenario 
		if (scenarioURL != null)  {
			String url = scenarioURL + "/query";
			
			String json = new JSONStringer().object()
					.key(ScenarioKeys.SCENARIO_RESOURCE).value(urlOrPath)
					.key(ScenarioKeys.QUERY_SPARQL_QUERY).value(sparqlQuery)
					.endObject().toString();
			
			return AgentCaller.executeGetWithURLAndJSON(url, json);
		}
		
		ResultSet resultSet = null;
		if (urlOrPath.startsWith("http")) {
			resultSet = JenaHelper.queryUrl(urlOrPath, sparqlQuery);
		} else {
			resultSet = JenaHelper.queryFile(urlOrPath, sparqlQuery);
		}
		
		return JenaResultSetFormatter.convertToJSONW3CStandard(resultSet);
	}
	
	public String updateFile(String urlOrPath, String sparqlUpdate) {
		
		String scenarioURL = ThreadContext.get(ScenarioKeys.SCENARIO_URL);	
		LogServer.debug(this, "updating file for urlOrPath=" + urlOrPath + ", scenarioURL=" + scenarioURL);
		
		// call the scenario agent if a scenario url is set in the input
		if (scenarioURL != null)  {
			String url = scenarioURL + "/update";
			
			String json = new JSONStringer().object()
					.key(ScenarioKeys.SCENARIO_RESOURCE).value(urlOrPath)
					.key(ScenarioKeys.QUERY_SPARQL_UPDATE).value(sparqlUpdate)
					.endObject().toString();
			
			return AgentCaller.executeGetWithURLAndJSON(url, json);
		}
		
		// TODO-AE SC for updating a remote file, first load the content and update it. But the update can only
		// be stored within scenarios. Later: Broker should find out, whether an QueryAgent (or another broker)
		// is running on remote server which has local access. 
		String localFile = urlOrPath;
		if (urlOrPath.startsWith("http")) {
			URI uri = AgentCaller.createURI(urlOrPath);
			String rootPath = KeyValueServer.get("absdir.root");
			localFile = rootPath + uri.getPath();
		}
		
		LogServer.debug(this, "updating local file=" + localFile);
		
		
		OntModel model =  ModelFactory.createOntologyModel(OntModelSpec.OWL_DL_MEM);
		
		UpdateRequest request = UpdateFactory.create("bla");
		
		
		
//		ResultSet resultSet = null;
//		if (urlOrPath.startsWith("http")) {
//			resultSet = JenaHelper.queryUrl(urlOrPath, sparqlUpdate);
//		} else {
//			resultSet = JenaHelper.queryFile(urlOrPath, sparqlUpdate);
//		}
//		
//		return JenaResultSetFormatter.convertToJSONW3CStandard(resultSet);
		
		return null;
	}
	

}
