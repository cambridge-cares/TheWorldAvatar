package uk.ac.cam.cares.jps.base.scenario;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import org.json.JSONObject;
import org.junit.After;
import org.junit.Test;


public class JPSContextTest{

	@After
	public void tearDown() {
		JPSContext.removeJPSContext();
	}

	// Test get(JSONObject jo, String key) and put(JSONObject jo, String key, String value)
	@Test
	public void testPutandGet(){
		JSONObject jo = new JSONObject();
		String key = "testkey";
		String value = "testvalue";
		
		// Default
		assertNull(JPSContext.get(jo, key));
		
		// Custom values
		JPSContext.put(jo, key, value);
		assertEquals("testvalue", JPSContext.get(jo, key));
	}
	
	// Test createJpsContext(), putJPSContext(JSONObject jpsContext), removeJPSContext() and getJpsContext()
	@Test
	public void testCreatePutGetandRemoveJpsContext() {
		//Create new JSON object
		JSONObject testobj = JPSContext.createJpsContext();
		assertTrue(testobj.length()==0);
		
		//Put test content in object
		testobj.put("testkey", "testvalue");
		JPSContext.putJPSContext(testobj);
		assertEquals("{\"testkey\":\"testvalue\"}", JPSContext.getJpsContext().toString());
		
		//Remove content 
		JPSContext.removeJPSContext();
		assertNull(JPSContext.getJpsContext());
		
	}
		
	// Test putScenarioUrl(JSONObject jo, String value) and getScenarioUrl(JSONObject jo)
	@Test
	public void testPutandGetScenarioUrl() {
		JSONObject jo = new JSONObject();
		String value = "testvalue";
		assertNull(JPSContext.getScenarioUrl(jo));
		JPSContext.putScenarioUrl(jo, value);
		assertEquals("testvalue", JPSContext.getScenarioUrl(jo));
	}
	
	// Test putUsecaseUrl((JSONObject jo, String value) and getUsecaseUrl(JSONObject jo)
	@Test
	public void testPutandGetUsecaseUrl() {
		JSONObject jo = new JSONObject();
		String value = "testvalue";
		assertNull(JPSContext.getUsecaseUrl(jo));
		JPSContext.putUsecaseUrl(jo, value);
		assertEquals("testvalue", JPSContext.getUsecaseUrl(jo));
	}
	
	// Test putSimulationTime(JSONObject jo, String value) and getSimulationTime(JSONObject jo)
	@Test
	public void testPutandGetSimulationTime() {
		JSONObject jo = new JSONObject();
		String value = "testvalue";
		assertNull(JPSContext.getSimulationTime(jo));
		JPSContext.putSimulationTime(jo, value);
		assertEquals("testvalue", JPSContext.getSimulationTime(jo));
		
	}

	// Test put(String key, String value); get(String key) and remove(String key)
	@Test
	public void testPutGetandRemove() {
		String key = "testkey";
		String value = "testvalue";
		assertNull(JPSContext.get(key));
		JPSContext.put(key, value);
		assertEquals("testvalue", JPSContext.get(key));
		JPSContext.remove(key);
		assertNull(JPSContext.get(key));
	}

	// Test putScenarioUrl(String value); getScenarioUrl() and removeScenarioUrl()
	@Test
	public void testPutGetandRemoveScenarioUrl() {
		String value = "testvalue";
		assertNull(JPSContext.getScenarioUrl());
		JPSContext.putScenarioUrl(value);
		assertEquals("testvalue", JPSContext.getScenarioUrl());
		JPSContext.removeScenarioUrl();
		assertNull(JPSContext.getScenarioUrl());
	}
	
	// Test putUsecaseUrl(String value); getUsecaseUrl() and removeUsecaseUrl()
	@Test
	public void testPutGetandRemoveUsecaseUrl() {
		String value = "testvalue";
		assertNull(JPSContext.getUsecaseUrl());
		JPSContext.putUsecaseUrl(value);
		assertEquals("testvalue", JPSContext.getUsecaseUrl());
		JPSContext.removeUsecaseUrl();
		assertNull(JPSContext.getUsecaseUrl());
		
	}
	
	// Test putSimulationTime(String value); getSimulationTime() and removeSimulationTime()
	@Test
	public void testPutGetandRemoveSimulationTime() {
		String value = "testvalue";
		assertNull(JPSContext.getSimulationTime());
		JPSContext.putSimulationTime(value);
		assertEquals("testvalue", JPSContext.getSimulationTime());
		JPSContext.removeSimulationTime();
		assertNull(JPSContext.getSimulationTime());
	}

}
