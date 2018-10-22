package uk.ac.cam.cares.jps.base.discovery.test;

import java.net.URI;

import org.json.JSONException;
import org.json.JSONObject;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;

public class TestAgentCaller extends TestCase {

	public void testJsonQuery() throws JSONException {
		
		JSONObject json = new JSONObject();
		json.put("key1", "value1");
		json.put("key2", "value2");
		
		String result = AgentCaller.executeGetWithJsonParameter("JPS_BASE/test/AgentOne/*", json.toString());
		json = new JSONObject(result);
		
		assertEquals("value1", json.get("key1"));
		assertEquals("value2", json.get("key2"));
	}
	
	public void testJsonQueryWithKeyValuePairs() throws JSONException {
		
		String result = AgentCaller.executeGet("JPS_BASE/test/AgentOne/*", "key1", "value1", "key2", "value2");
		JSONObject json = new JSONObject(result);
		
		assertEquals("value1", json.get("key1"));
		assertEquals("value2", json.get("key2"));
	}
	
	public void testCreateURI() {
		
		URI uri = AgentCaller.createURI("http://www.theworldavatar.com:80/damecoolquestion/berlinbuildings/query", "query", "this is a query with & sign");	
		assertEquals("http://www.theworldavatar.com:80/damecoolquestion/berlinbuildings/query?query=this+is+a+query+with+%26+sign", uri.toString());	
	}
}
