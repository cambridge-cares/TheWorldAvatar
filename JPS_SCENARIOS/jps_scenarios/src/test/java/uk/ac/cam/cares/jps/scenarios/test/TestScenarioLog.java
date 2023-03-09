package uk.ac.cam.cares.jps.scenarios.test;

import org.json.JSONArray;
import org.json.JSONObject;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.scenario.ScenarioHelper;
import uk.ac.cam.cares.jps.scenario.ScenarioLog;

public class TestScenarioLog extends TestCase {
	
	private ScenarioLog createSimpleLogWithoutWriting() {
		ScenarioLog log = new ScenarioLog("test789xyz");
		JSONObject message = new JSONObject();
		message.put("operation", "mock").put("input", "some input parameter values");
		log.logMessage("test789xyz", message);
		message = new JSONObject();
		message.put("operation", "query").put("input", "some other input parameter values");
		log.logMessage("test789xyz", message);
		
		return log;
	}

	public void testToJson() {
		
		ScenarioLog log = createSimpleLogWithoutWriting();
		JSONObject jo = log.toJson();
		System.out.println(jo.toString());
		
		JSONArray ja = jo.getJSONArray("entries");
		assertEquals(3, ja.length());
		assertEquals("test789xyz", ja.getJSONObject(0).getString("scenario"));
	}
	
	public void testMerge() {
		
		ScenarioLog log1 = createSimpleLogWithoutWriting();
		assertEquals(3, log1.getEntries().size());
		
		String log2Serialized = "{\"entries\":[{\"scenario\":\"aascmerge\",\"message\":{\"extendsagent\":\"http://www.theworldavatar.com/kb/agents/Service__ScenarioAgent.owl#Service\"},\"timestamp\":\"2019-04-23 12:16:47.047-MESZ\"},{\"scenario\":\"aascmerge\",\"message\":{\"agent\":\"http://www.theworldavatar.com/kb/agents/Service__OpenWeatherMap.owl#Service\",\"operation\":\"mock\"},\"timestamp\":\"2019-04-23 12:17:04.772-MESZ\"},{\"scenario\":\"aascmerge\",\"message\":{\"output\":{\"weatherstate\":{\"haswind\":{\"hasspeed\":\"10.3\",\"hasdirection\":\"100\"},\"hashumidity\":{\"hasvalue\":\"41\"},\"hasweathercondition\":\"clear_sky\",\"hasprecipation\":{\"hasintensity\":\"0.0\"},\"hasexteriortemperature\":{\"hasvalue\":\"17.28\"},\"hascloudcover\":{\"hascloudcovervalue\":\"0\"}}},\"input\":{\"city\":\"http://dbpedia.org/resource/Berlin\",\"scenariourl\":\"http://localhost:8080" + ScenarioHelper.SCENARIO_COMP_URL + "/aascmerge\"},\"agent\":\"http://www.theworldavatar.com/kb/agents/Service__OpenWeatherMap.owl#Service\",\"operation\":\"/CityToWeather\"},\"timestamp\":\"2019-04-23 12:19:19.050-MESZ\"},{\"scenario\":\"aascmerge\",\"message\":{\"operation\":\"option\",\"copyonread\":true},\"timestamp\":\"2019-04-23 12:22:02.069-MESZ\"},{\"scenario\":\"aascmerge\",\"message\":{\"agent\":\"http://www.theworldavatar.com/kb/agents/Service__YahooWeather.owl#Service\",\"operation\":\"mock\"},\"timestamp\":\"2019-04-23 12:44:21.333-MESZ\"}]}";
		ScenarioLog log2 = new ScenarioLog("aascmerge");
		log2.read(log2Serialized);
		assertEquals(5, log2.getEntries().size());
		
		log1.merge(log2);
		// plus one additional entry about merging
		assertEquals(8+1, log1.getEntries().size());
		
		String log3Serialized = "{\"entries\":[{\"scenario\":\"aascmergesource\",\"message\":{\"extendsagent\":\"http://www.theworldavatar.com/kb/agents/Service__ScenarioAgent.owl#Service\"},\"timestamp\":\"2019-04-23 12:43:36.502-MESZ\"},{\"scenario\":\"aascmergesource\",\"message\":{\"agent\":\"http://www.theworldavatar.com/kb/agents/Service__ComposedADMS.owl#Service\",\"operation\":\"mock\"},\"timestamp\":\"2019-04-23 12:43:50.841-MESZ\"},{\"scenario\":\"aascmergesource\",\"message\":{\"agent\":\"http://www.theworldavatar.com/kb/agents/Service__MEN.owl#Service\",\"operation\":\"mock\"},\"timestamp\":\"2019-04-23 12:43:55.804-MESZ\"},{\"scenario\":\"aascmergesource\",\"message\":{\"agent\":\"http://www.theworldavatar.com/kb/agents/Service__AccuWeather.owl#Service\",\"operation\":\"mock\"},\"timestamp\":\"2019-04-23 12:44:05.589-MESZ\"},{\"scenario\":\"aascmergesource\",\"message\":{\"output\":{},\"input\":{\"city\":\"http://dbpedia.org/resource/The_Hague\",\"scenariourl\":\"http://localhost:8080" + ScenarioHelper.SCENARIO_COMP_URL + "/aascmergesource\"},\"agent\":\"http://www.theworldavatar.com/kb/agents/Service__AccuWeather.owl#Service\",\"operation\":\"/MockCityToWeather_Accu\"},\"timestamp\":\"2019-04-23 12:46:14.757-MESZ\"},{\"scenario\":\"aascmergesource\",\"message\":{\"output\":{},\"input\":{\"city\":\"http://dbpedia.org/resource/The_Hague\",\"scenariourl\":\"http://localhost:8080" + ScenarioHelper.SCENARIO_COMP_URL + "/aascmergesource\"},\"agent\":\"http://www.theworldavatar.com/kb/agents/Service__AccuWeather.owl#Service\",\"operation\":\"/MockCityToWeather_Accu\"},\"timestamp\":\"2019-04-23 12:46:27.654-MESZ\"},{\"scenario\":\"aascmergesource\",\"message\":{\"agent\":\"http://www.theworldavatar.com/kb/agents/Service__OpenWeatherMap.owl#Service\",\"operation\":\"mock\"},\"timestamp\":\"2019-04-23 12:46:51.916-MESZ\"},{\"scenario\":\"aascmergesource\",\"message\":{\"output\":{\"weatherstate\":{\"haswind\":{\"hasspeed\":\"7.7\",\"hasdirection\":\"90\"},\"hashumidity\":{\"hasvalue\":\"51\"},\"hasweathercondition\":\"scattered_clouds\",\"hasprecipation\":{\"hasintensity\":\"0.0\"},\"hasexteriortemperature\":{\"hasvalue\":\"18.14\"},\"hascloudcover\":{\"hascloudcovervalue\":\"25\"}}},\"input\":{\"city\":\"http://dbpedia.org/resource/The_Hague\",\"scenariourl\":\"http://localhost:8080" + ScenarioHelper.SCENARIO_COMP_URL + "/aascmergesource\"},\"agent\":\"http://www.theworldavatar.com/kb/agents/Service__OpenWeatherMap.owl#Service\",\"operation\":\"/CityToWeather\"},\"timestamp\":\"2019-04-23 12:47:01.968-MESZ\"}]}";
		ScenarioLog log3 = new ScenarioLog("aascmergesource");
		log3.read(log3Serialized);
		assertEquals(8, log3.getEntries().size());
		
		log1.merge(log3);
		// plus one additional entry about merging
		assertEquals(8+1+8+1, log1.getEntries().size());
	}
}
