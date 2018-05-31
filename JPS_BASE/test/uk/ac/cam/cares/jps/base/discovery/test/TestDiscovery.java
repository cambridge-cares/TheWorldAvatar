package uk.ac.cam.cares.jps.base.discovery.test;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.google.gson.Gson;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.discovery.AgentRequest;
import uk.ac.cam.cares.jps.base.discovery.Parameter;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

public class TestDiscovery extends TestCase {
	
	public void testSerializeAgentReqestWithJson() {
		
		AgentRequest expected = new AgentRequest();
		Parameter prop = new Parameter("myPropertyKey", "myPropertyValue");
		expected.getProperties().add(prop);
		Parameter paramIn = new Parameter("myInputKey", "myInputValue");
		expected.getInputParameters().add(paramIn);
		Parameter paramOut = new Parameter("myOutputKey", "myOutputValue");
		expected.getOutputParameters().add(paramOut);
		
		Gson gson = new Gson();
		String s = gson.toJson(expected);
		System.out.println("serialized = " + s);
		
		AgentRequest actual = gson.fromJson(s, AgentRequest.class);

		assertTrue(prop.getKey().equals(actual.getProperties().get(0).getKey()));
		assertTrue(prop.getValue().equals(actual.getProperties().get(0).getValue()));
		assertTrue(paramIn.getKey().equals(actual.getInputParameters().get(0).getKey()));
		assertTrue(paramIn.getValue().equals(actual.getInputParameters().get(0).getValue()));
		assertTrue(paramOut.getKey().equals(actual.getOutputParameters().get(0).getKey()));
		assertTrue(paramOut.getValue().equals(actual.getOutputParameters().get(0).getValue()));
	}
	
	public void testSerializeMapWithJson() {
		Map<String, Double> map = new HashMap<>();
		for (int i=1; i<6; i++) {
			map.put("key" + i, i+0.5);
		}
		
		map.put("anotherKey", null);
		
		String s = new Gson().toJson(map);
		System.out.println("serialized = " + s);
		
		Map<String, Double> actual = new Gson().fromJson(s, HashMap.class);
		
		for (String current : actual.keySet()) {
			System.out.println(current + "=" + actual.get(current));
		}
		
		// the key with value null is not considered in the map! 
		assertEquals(5, actual.keySet().size());
	}
	
	public void testSerializableData() {
		
		Map<String, Double> map = new HashMap<String, Double>();
		List<Double> list = new ArrayList<Double>();
		for (int i=1; i<6; i++) {
			map.put("key" + i, i+0.5);
			list.add(i+0.3);
		}
		
		SerializableData data = new SerializableData();
		data.setInput1(map);
		data.setInput2(list);
		
		String s = new Gson().toJson(data);
		System.out.println("serialized = " + s);
		
		SerializableData actual = new Gson().fromJson(s, SerializableData.class);
		
		Map<String, Double> actualMap = (Map<String, Double>) actual.getInput1();
		System.out.println("deserialized map = " + actualMap.getClass());
		for (String current : actualMap.keySet()) {
			System.out.println(current + "=" + actualMap.get(current));
		}
		
		List<Double> actualList = (List<Double>) actual.getInput2();
		System.out.println("deserialized list = " + actualList.getClass());
		for (Double current : actualList) {
			System.out.println(current);
		}
	}
	
	public void testParameterSupportedTypes() {
		
		new Parameter("key", "someValue");
		new Parameter("key", true);
		new Parameter("key", 4.03);
		new Parameter("key", null);
		
		List<Object> l = new ArrayList<Object>();
		l.add("someValue");
		l.add(false);
		l.add(-3.5);
		new Parameter("key", l);
		
		Map<String, Object> m = new HashMap<String, Object>();
		m.put("key1", "someValue");
		m.put("key2", true);
		m.put("key3", 4.7);
		m.put("key4", 2);
		new Parameter("key", m);
	}
	
	public void testParameterUnsupportedTypes() {
		
		boolean excCaught = false;
		try {
			new Parameter("key", new Parameter("", ""));
		} catch (JPSRuntimeException exc) {
			excCaught = true;
		}
		assertTrue(excCaught);
		
		excCaught = false;
		List<Object> l = new ArrayList<Object>();
		l.add("someValue");
		l.add(false);
		l.add(new ArrayList<String>());
		try {
			new Parameter("key", l);
		} catch (JPSRuntimeException exc) {
			excCaught = true;
		}
		assertTrue(excCaught);
		
		excCaught = false;
		Map<String, Object> m = new HashMap<String, Object>();
		m.put("key1", "someValue");
		m.put("key2", true);
		m.put("key3", new Object());
		try {
			new Parameter("key", m);
		} catch (JPSRuntimeException exc) {
			excCaught = true;
		}
		assertTrue(excCaught);
	}
}
