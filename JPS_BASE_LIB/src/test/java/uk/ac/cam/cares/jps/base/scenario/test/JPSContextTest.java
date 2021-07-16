package uk.ac.cam.cares.jps.base.scenario.test;

import org.json.JSONObject;
import uk.ac.cam.cares.jps.base.scenario.JPSContext;

import org.junit.Test;
import static org.junit.Assert.*;


public class JPSContextTest{
	
	JSONObject jo = new JSONObject();
	String key = "testkey";
	String value = "testvalue";
	
	// Test get(JSONObject jo, String key) and put(JSONObject jo, String key, String value)
	@Test
	public void getTest(){
		assertNull(JPSContext.get(jo, key));
		JPSContext.put(jo, key, value);
		assertEquals("testvalue", JPSContext.get(jo, key)) ;
	}
	
	@Test
	public void getJpsContextTest() {
		assertNull(JPSContext.getJpsContext());		
	}
	
	@Test
	public void createJpsContextTest() {
		assertTrue(JPSContext.createJpsContext().length()==0);
	}
	
//	@Test
//	public void putScenarioUrlTest() {
//		//put(jo, JPSConstants.SCENARIO_URL, value); JSONObject jo, String value
//		//Test null 
//		JSONObject jo = null;
//		String value = "testvalue";
//		String key = "testkey";
//	}
	
	@Test
	public void getScenarioUrlTest() {
		assertNull(JPSContext.getScenarioUrl(jo));
	}
	
	@Test
	public void putUsecaseUrlTest() {
		//JSONObject jo, String value
		//TODO
	}
	
	@Test
	public void getUsecaseUrlTest() {
		assertNull(JPSContext.getUsecaseUrl(jo));
	}
	
	@Test
	public void putSimulationTimeTest() {
		//TODO
	}
	
	@Test
	public void getSimulationTimeTest() {
		assertNull(JPSContext.getSimulationTime(jo));
	}
	
	@Test
	public void putTest2() {
		//TODO
	}
	
	@Test
	public void getTest2() {
		assertNull(JPSContext.get(key));
	}
	
	@Test
	public void removeTest() {
		//TODO
	}
	
	@Test
	public void putScenarioUrlTest2() {
		//TODO
	}
	
	@Test
	public void getScenarioUrlTest2() {
		assertNull(JPSContext.getScenarioUrl());
	}
	
	@Test
	public void removeScenarioUrlTest() {
		//TODO
	}
	
	@Test
	public void putUsecaseUrlTest2() {
		//TODO
	}
	
	@Test
	public void getUsecaseUrlTest2() {
		assertNull(JPSContext.getUsecaseUrl());
	}
	
	@Test
	public void removeUsecaseUrlTest() {
		//TODO
	}
	
	@Test
	public void putSimulationTimeTest2() {
		//TODO
	}
	
	@Test
	public void getSimulationTimeTest2() {
		assertNull(JPSContext.getSimulationTime());
	}
	
	@Test 
	public void removeSimulationTimeTest() {
		
	}
}
