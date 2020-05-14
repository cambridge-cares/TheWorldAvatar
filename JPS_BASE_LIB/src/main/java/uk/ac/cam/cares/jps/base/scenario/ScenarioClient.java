package uk.ac.cam.cares.jps.base.scenario;

import java.net.URI;

import org.apache.http.client.methods.HttpGet;
import org.json.JSONObject;
import org.json.JSONStringer;

import uk.ac.cam.cares.jps.base.config.JPSConstants;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.http.Http;
import uk.ac.cam.cares.jps.base.query.KnowledgeBaseClient;

public class ScenarioClient {
	
	public String call(String scenarioNameOrUrl, String scenarioAgentOperation, String jsonInputParams) {
		
		JSONObject jo = new JSONObject(jsonInputParams);
		jo.put(JPSConstants.SCENARIO_AGENT_OPERATION, scenarioAgentOperation);
		String json = jo.toString();
		
		if (scenarioNameOrUrl.startsWith("http")) {
			String url = scenarioNameOrUrl + "/call";
			return AgentCaller.executeGetWithURLAndJSON(url, json);
		}	
	
		String path = ScenarioHelper.getScenarioPath(scenarioNameOrUrl) + "/call";
		return AgentCaller.executeGetWithJsonParameter(path, json);
	}
	
	public String mockedOperation(String scenarioNameOrUrl, String shortOperationName, String jsonInputParams) {
		
		JSONObject jo = new JSONObject(jsonInputParams);
		String json = jo.toString();
		
		if (scenarioNameOrUrl.startsWith("http")) {
			String url = scenarioNameOrUrl + shortOperationName;
			return AgentCaller.executeGetWithURLAndJSON(url, json);
		}	
	
		String path = ScenarioHelper.getScenarioPath(scenarioNameOrUrl) + shortOperationName;
		return AgentCaller.executeGetWithJsonParameter(path, json);
	}
	
	public String read(String scenarioNameOrUrl, String resourceUrl) {
		
		String json = new JSONStringer().object()
				.key(JPSConstants.SCENARIO_RESOURCE).value(resourceUrl)
				.endObject().toString();
		
		if (scenarioNameOrUrl.startsWith("http")) {
			String url = scenarioNameOrUrl + "/read";
			return AgentCaller.executeGetWithURLAndJSON(url, json);
		}	
	
		String path = ScenarioHelper.getScenarioPath(scenarioNameOrUrl) + "/read";
		return AgentCaller.executeGetWithJsonParameter(path, json);
	}
	
	public String setOptionCopyOnRead(String scenarioNameOrUrl, boolean value) {
		return option(scenarioNameOrUrl, JPSConstants.SCENARIO_OPTION_COPY_ON_READ, value);
	}
	
	private String option(String scenarioNameOrUrl, String key, Object value) {
		
		String json = new JSONStringer().object()
				.key(key).value(value)
				.endObject().toString();
		
		if (scenarioNameOrUrl.startsWith("http")) {
			String url = scenarioNameOrUrl + "/option";
			return AgentCaller.executeGetWithURLAndJSON(url, json);
		}	
	
		String path = ScenarioHelper.getScenarioPath(scenarioNameOrUrl) + "/option";
		return AgentCaller.executeGetWithJsonParameter(path, json);
	}
	
	public URI getReadUrl(String scenarioUrl, String resourceUrl) {
		
		//throw new UnsupportedOperationException();
		
		Object[] a = KnowledgeBaseClient.createRequestUrl(null, resourceUrl, false);
		String requestUrl = (String) a[0];
		JSONObject joparams = (JSONObject) a[1];
		HttpGet get = Http.get(requestUrl, null, joparams);
		return get.getURI();
			
//		String requestUrl = cutHashFragment(resourceUrl);
//		return Http.execute(Http.get(requestUrl, accept));
		
//		OLD IMPLEMENTATION: 
//		String json = new JSONStringer().object()
//				.key(JPSConstants.SCENARIO_RESOURCE).value(resourceUrl)
//				.endObject().toString();
//	
//		String url = scenarioUrl + "/read";
//		return AgentCaller.createURIWithURLandJSON(url, json);
	}
	
	public String query(String scenarioUrl, String resourceUrl, String sparqlQuery) {

		String json = new JSONStringer().object()
				.key(JPSConstants.SCENARIO_RESOURCE).value(resourceUrl)
				.key(JPSConstants.QUERY_SPARQL_QUERY).value(sparqlQuery)
				.endObject().toString();
		
		String url = scenarioUrl + "/query";
		return AgentCaller.executeGetWithURLAndJSON(url, json);
	}
}
