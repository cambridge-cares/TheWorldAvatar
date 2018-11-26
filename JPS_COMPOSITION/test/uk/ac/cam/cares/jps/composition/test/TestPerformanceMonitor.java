package uk.ac.cam.cares.jps.composition.test;

import java.util.HashMap;
import java.util.Map;

import org.json.JSONException;
import org.junit.After;

public class TestPerformanceMonitor {

	@After
	public void tearDown() throws Exception {
	}

	//@Test
	public void test() throws JSONException {

		Map<String, String> id_map = new HashMap<String, String>();
		id_map .put("/JPS_COMPOSITION/CityToWeather", "0x0B9056fcbf59D283F7c6B909Ea729182Bd69D36E");
		id_map.put("/JPS_COMPOSITION/MockCityToWeather_Accu", "0x700B09517ddaCF44617371D8D00bB09FE9425FC8");
		id_map.put("/JPS_COMPOSITION/MockCityToWeather_Yahoo", "0x1c5048f718289e8b7050648ceb9b330a7b3f310f");
		
		//PerformanceMonitor.pay_deposit();
		
		for(String key : id_map.keySet()) {
			String agent_id = id_map.get(key);
			//String r = PerformanceMonitor.get_token_for_agent(agent_id);
//			JSONObject obj = new JSONObject(r);
//			System.out.println(r);
//			System.out.println("agent id: " + key);
//			System.out.println("time stamp: " + obj.getString("1"));
//			System.out.println("private key: " + "85564251");
//			//verify_token_for_agent(String agent_id, String time_stamp,String private_key) {
//			String newToken = PerformanceMonitor.verify_token_for_agent(agent_id, obj.getString("1"), "85564251");
//			System.out.println("new token : " + newToken);
//			System.out.println("=====================================");
		}
		
	}

}
