package uk.ac.cam.cares.jps.scenarios.test;

import org.apache.logging.log4j.ThreadContext;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;
import org.json.JSONStringer;
import org.junit.Ignore;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.scenario.ScenarioClient;
import uk.ac.cam.cares.jps.base.scenario.ScenarioHelper;

@Ignore("This test sends HTTP requests. Cannot run without correct environment setup.")
public class TestScenarios extends TestCase {
	
	//private static final String PLANT = "http://www.theworldavatar.com/kb/powerplants/Northwest_Kabul_Power_Plant_Afghanistan.owl";
	public static final String PLANT = "http://www.theworldavatar.com/kb/powerplants/Northwest_Kabul_Power_Plant_Afghanistan.owl#Northwest_Kabul_Power_Plant_Afghanistan";
	
	private void putToThreadContext() {
		ThreadContext.put("scenarioname", "abc");
	}
	
	public void testThreadContext() {
		putToThreadContext();
		assertEquals("abc", ThreadContext.get("scenarioname"));
	}
	
	public void testJsonQueryOnJpsBase() throws JSONException {
		
		JSONObject json = new JSONObject();
		json.put("key1", "value1");
		json.put("key2", "value2");
		
		String result = AgentCaller.executeGetWithJsonParameter("JPS_BASE/test/AgentBase/*", json.toString());
		json = new JSONObject(result);
		
		assertEquals("value1", json.get("key1"));
		assertEquals("value2", json.get("key2"));
	}
	
	public static void assertEmissionValue(String scenario, String plant, double expected) {
		
		String json = new JSONStringer().object()
				.key("plant").value(plant)
				.endObject().toString();
		
		String result = new ScenarioClient().call(scenario, "/JPS_BASE/EmissionTestAgent/queryemission", json);
		
		System.out.println(result);
		
		JSONArray list = new JSONObject(result).getJSONArray("results");
		double actual = list.getJSONObject(0).getDouble("emissionvaluenum");
		assertEquals(expected, actual);
	}
	
	public static String setEmissionValue(String scenario, String plant, double emissionValue) {
		String json = new JSONStringer().object()
				.key("plant").value(plant)
				.key("emission").value(emissionValue)
				.endObject().toString();
		
		return new ScenarioClient().call(scenario, "/JPS_BASE/EmissionTestAgent/setemission", json);
	}

	public void testCreateScenarioAndCallEmissionTestAgentRead() throws JSONException {
		
		String json = new JSONStringer().object()
				.key("plant").value(PLANT)
				.endObject().toString();
		
		String result = new ScenarioClient().call("test1234567", "/JPS_BASE/EmissionTestAgent/read", json);
		System.out.println(result);
		assertTrue(result.contains("<rdf:RDF"));
	}
	
	public void testCreateAndDeleteScenario() {
		
		String scenarioName = "test1234567b";
		//new ScenarioAgent().deleteScenario(scenarioName);
		
		setEmissionValue(scenarioName, PLANT, 100.00);
		String path = ScenarioHelper.getScenarioPath(scenarioName) + "/delete";
		System.out.println(path);
		//AgentCaller.executeGet(path);
		
		// TODO-AE SC 20190219 problems with deleting OWL files --> problem might be caused by Jena. 
		// deletion is also necessary to clean up the test cases
		throw new RuntimeException("deletion of files is not solved yet");
	}
	
	public void testCreateScenarioAndCallEmissionTestAgentGet() throws JSONException {
		
		String scenarioName = "test1234567c";
		double emissionValue = 139.99;
		setEmissionValue(scenarioName, PLANT, emissionValue);
		
		String json = new JSONStringer().object()
				.key("plant").value(PLANT)
				.endObject().toString();
		new ScenarioClient().call(scenarioName, "/JPS_BASE/EmissionTestAgent/getemission", json);

		assertEmissionValue(scenarioName, PLANT, emissionValue);
	}
	
	public void testCreateScenarioAndCallEmissionTestAgentSet() throws JSONException {
	
		String scenarioName = "test1234567d";
		double emissionValue = 114.07;
		setEmissionValue(scenarioName, PLANT, emissionValue);
		assertEmissionValue(scenarioName, PLANT, emissionValue);
	}
	
	public void testCreateScenarioAndCallEmissionTestAgentAdd() throws JSONException {
		
		String scenarioName = "test1234567e";
		double emissionValue = 167.01;
		setEmissionValue(scenarioName, PLANT, emissionValue);
		
		String json = new JSONStringer().object()
				.key("plant").value(PLANT)
				.key("increment").value("3")
				.endObject().toString();
		new ScenarioClient().call(scenarioName, "/JPS_BASE/EmissionTestAgent/add", json);
		
		assertEmissionValue(scenarioName, PLANT, 170.01);
	}
	
	public void testCreateScenarioAndCallEmissionTestAgentMultiply() throws JSONException {
		
		String scenarioName = "test1234567f";
		double emissionValue = 100;
		setEmissionValue(scenarioName, PLANT, emissionValue);
		
		String json = new JSONStringer().object()
				.key("plant").value(PLANT)
				.key("factor").value("2.511")
				.endObject().toString();
		new ScenarioClient().call(scenarioName, "/JPS_BASE/EmissionTestAgent/multiply", json);
				
		assertEmissionValue(scenarioName, PLANT, 251.1);
	}
	
	/**
	 * This test case calls the change method of EmissionTestAgent with the formula "+10.5*2-22"
	 * which lead to 2 further recursive calls of the change method. This test case
	 * shows that the scenario mechanism works even if several agents call each other 
	 * (all the agents involved in this test case are of the same type EmissionTestAgent).
	 * 
	 * @throws JSONException
	 */
	public void testCreateScenarioAndCallEmissionTestAgentChange() throws JSONException {
		
		String scenarioName = "test1234567g";
		double emissionValue = 100;
		setEmissionValue(scenarioName, PLANT, emissionValue);
		
		String json = new JSONStringer().object()
				.key("plant").value(PLANT)
				.key("formula").value("+10.5*2-22")
				.endObject().toString();
		new ScenarioClient().call(scenarioName, "/JPS_BASE/EmissionTestAgent/change", json);
				
		assertEmissionValue(scenarioName, PLANT, 199);
	}
	
	public void testCreateScenarioAndCallWeatherAgent() throws JSONException {
		
		String scenarioName = "test1234567h";

		String json = new JSONStringer().object()
				.key("city").value("http://dbpedia.org/resource/Berlin")
				.endObject().toString();
		String result = new ScenarioClient().call(scenarioName, "http://www.theworldavatar.com/JPS_COMPOSITION/CityToWeather", json);
		
		System.out.println(result);
		JSONObject jo = new JSONObject(result);
		assertTrue(jo.has("weatherstate"));
	}
	
}
