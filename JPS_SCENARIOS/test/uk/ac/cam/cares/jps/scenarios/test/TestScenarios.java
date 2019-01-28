package uk.ac.cam.cares.jps.scenarios.test;

import java.util.List;

import org.apache.logging.log4j.ThreadContext;
import org.json.JSONException;
import org.json.JSONObject;
import org.json.JSONStringer;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.query.JenaResultSetFormatter;
import uk.ac.cam.cares.jps.base.query.ScenarioKeys;
//import uk.ac.cam.cares.jps.base.query.test.TestQuery;
import uk.ac.cam.cares.jps.scenario.ScenarioAgent;

public class TestScenarios extends TestCase {
	
	private void putToThreadContext() {
		ThreadContext.put("scenarioname", "abc");
	}
	
	public void testThreadContext() {
		putToThreadContext();
		assertEquals("abc", ThreadContext.get("scenarioname"));
	}
	
	public void testDividePath() {
		
		// auto generate a scenario id
		String path = "/";
		String[] actual= new ScenarioAgent().dividePath(path);
		assertNotNull(actual[0]);
		assertNull(actual[1]);
		
		// throws an exception because path (without /) does not contain at least 10 characters
		path = "/123456789";
		try {
			new ScenarioAgent().dividePath(path);
		} catch (Exception e) {
		}
	
		path = "/1234567890";
		actual= new ScenarioAgent().dividePath(path);
		assertEquals("1234567890", actual[0]);
		assertNull(actual[1]);
		
		path = "/1234567890/any/fancy/operation";
		actual= new ScenarioAgent().dividePath(path);
		assertEquals("1234567890", actual[0]);
		assertEquals("/any/fancy/operation", actual[1]);
	}
	
	public void testEmissionTestAgentQueryEmission() {
		
		String plant = "http://www.theworldavatar.com/kb/powerplants/Northwest_Kabul_Power_Plant_Afghanistan.owl";		
		
		String json = new JSONStringer().object()
				.key("powerplant").value(plant)
				.endObject().toString();

		String result = AgentCaller.executeGetWithJsonParameter("/JPS_SCENARIO/EmissionTestAgent/queryemission", json);
		System.out.println(result);
	}
	
	public void testCreateScenarioAndCallEmissionTestAgentRead() throws JSONException {
		
		//String plant = AgentLocator.getCurrentJpsAppDirectory(this)+"/testres/Northwest_Kabul_Power_Plant_Afghanistan.owl";
		String plant = "http://www.theworldavatar.com/kb/powerplants/Northwest_Kabul_Power_Plant_Afghanistan.owl";		
		
		String json = new JSONStringer().object()
				.key(ScenarioKeys.SCENARIO_AGENT_OPERATION).value("/JPS_SCENARIO/EmissionTestAgent/read")
				.key("powerplant").value(plant)
				.endObject().toString();
		
		String result = AgentCaller.executeGetWithJsonParameter("/JPS_SCENARIO/scenario/test1234567/call", json);
		assertTrue(result.startsWith("<rdf:RDF"));
	}
	
	public void TODOtestCreateScenarioAndCallWeatherAgentWithCreatingScenarioDescription() throws JSONException {
		
		String city = "http://dbpedia.org/resource/Berlin";		
		
		String json = new JSONStringer().object()
				.key(ScenarioAgent.SCENARIO_AGENT).value("http://www.theworldavatar.com/kb/agents/Service__OpenWeatherMap.owl")
				.key(ScenarioKeys.SCENARIO_AGENT_OPERATION).value("http://www.theworldavatar.com/JPS_COMPOSITION/CityToWeather")
				.key("city").value(city)
				.endObject().toString();
		
		String result = AgentCaller.executeGetWithJsonParameter("/JPS_SCENARIO/scenario/test1234567/call", json);
		assertTrue(result.startsWith("<rdf:RDF"));
	}
	
	public void testCreateScenarioAndCallEmissionAgentCreatingScenarioDescription() throws JSONException {
		
		createScenarioAndCallEmissionAgentCreatingScenarioDescription("scenabcemissionagenttest");
	}
	
	public static void createScenarioAndCallEmissionAgentCreatingScenarioDescription(String scenarioName) throws JSONException {
		String agent = AgentLocator.getCurrentJpsAppDirectory(new TestScenarios()) + "/testres/Service__EmissionTest.owl";
		String plant = "http://www.theworldavatar.com/kb/powerplants/Northwest_Kabul_Power_Plant_Afghanistan.owl";	
		
		String json = new JSONStringer().object()
				.key(ScenarioAgent.SCENARIO_AGENT).value(agent)
				.key(ScenarioKeys.SCENARIO_AGENT_OPERATION).value("http://localhost:8080/JPS_SCENARIO/EmissionTestAgent/queryemission")
				.key("powerplant").value(plant)
				.endObject().toString();
		
		String result = AgentCaller.executeGetWithJsonParameter("/JPS_SCENARIO/scenario/" + scenarioName + "/call", json);
		List<JSONObject> list = JenaResultSetFormatter.convertToSimplifiedList(result);
		assertEquals("15.75", list.get(0).get("emissionvaluenum"));
		
		result = AgentCaller.executeGetWithJsonParameter("/JPS_SCENARIO/scenario/" + scenarioName + "/queryemission", json);
		list = JenaResultSetFormatter.convertToSimplifiedList(result);
		assertEquals("15.75", list.get(0).get("emissionvaluenum"));
	}
	
	public void testCreateScenarioAndCallEmissionTestAgentQueryEmission() throws JSONException {
		
		//String plant = AgentLocator.getCurrentJpsAppDirectory(this)+"/testres/Northwest_Kabul_Power_Plant_Afghanistan.owl";
		String plant = "http://www.theworldavatar.com/kb/powerplants/Northwest_Kabul_Power_Plant_Afghanistan.owl";		
		
		String json = new JSONStringer().object()
				.key(ScenarioKeys.SCENARIO_AGENT_OPERATION).value("/JPS_SCENARIO/EmissionTestAgent/queryemission")
				.key("powerplant").value(plant)
				.endObject().toString();
		
		String result = AgentCaller.executeGetWithJsonParameter("/JPS_SCENARIO/scenario/test1234567/call", json);
		List<JSONObject> list = JenaResultSetFormatter.convertToSimplifiedList(result);
		assertEquals("15.75", list.get(0).get("emissionvaluenum"));
	}
	
	public void testCreateScenarioAndCallEmissionTestAgentIncreaseEmission() throws JSONException {
		
		String plant = "http://www.theworldavatar.com/kb/powerplants/Northwest_Kabul_Power_Plant_Afghanistan.owl";		
		
		String json = new JSONStringer().object()
				.key(ScenarioKeys.SCENARIO_AGENT_OPERATION).value("/JPS_SCENARIO/EmissionTestAgent/increaseemission")
				.key("powerplant").value(plant)
				.key("increment").value("2")
				.endObject().toString();
		
		String result = AgentCaller.executeGetWithJsonParameter("/JPS_SCENARIO/scenario/test1234567/call", json);
		
		System.out.println(result);
	}
	
	public void testJsonQuery() throws JSONException {
		
		JSONObject json = new JSONObject();
		json.put("key1", "value1");
		json.put("key2", "value2");
		
		String result = AgentCaller.executeGetWithJsonParameter("JPS_BASE/test/AgentBase/*", json.toString());
		json = new JSONObject(result);
		
		assertEquals("value1", json.get("key1"));
		assertEquals("value2", json.get("key2"));
	}
}
