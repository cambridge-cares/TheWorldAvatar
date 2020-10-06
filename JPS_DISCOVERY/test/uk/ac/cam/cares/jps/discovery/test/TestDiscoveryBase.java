package uk.ac.cam.cares.jps.discovery.test;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.google.gson.Gson;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.discovery.AgentRequest;
import uk.ac.cam.cares.jps.base.discovery.Parameter;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.util.MatrixConverter;

public class TestDiscoveryBase extends TestCase {
	
	public class MonthPriceMatrix {
		String[] months = null;
		List<Double> prices = new ArrayList<Double>();
	}
	
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
	
	/**
	 * There are several possibilities to serialize a matrix:<br>
	 * <br>
	 * 1. A matrix with only two columns can be represented as map. But usually the map is not serialized as sorted key value pairs. 
	 * Also any desciption of the columns is missing<br>
	 * <br>
	 * 2. Each column of the matrix is represented as a list and saved an own Java attribute. 
	 * This makes serialization with GSON very easy. GSON uses the attribute name as key but this key has no semantic meaning (it is not an IRI).<br>
	 * <br> 
	 * 3. As in 2. each column of the matrix is represented as a list. Each list is saved as part of a key value pair in a map where the value is the list 
	 * and the key is an IRI describing the column. 
	 */
	public void testSerializeMatrix1WithTwoColumnsAsMap() {
		
		// TreeMap implements the interface SortedMap but doesn't seem to keep the order of pairs
		// But then we can just use HashMap
		//TreeMap<String, Double> data = new TreeMap<String, Double>();
		Map<String, Double> data = new HashMap<String, Double>();
		
		
		String[] months = new String[] {"JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC"};
		for (int i=0; i<12; i++) {
			double price = i / 10.;
			data.put(months[i], price);
		}
		
		String s = new Gson().toJson(data);
		System.out.println("serialized = \n" + s);
		
		//TreeMap<String, Double> actual = new Gson().fromJson(s, TreeMap.class);
		Map<String, Double> actual = new Gson().fromJson(s, HashMap.class);
		assertEquals(0.4, actual.get(months[4]));
	}
	
	public void testSerializeMatrix2WithColumnlists() {
		
		MonthPriceMatrix data = new MonthPriceMatrix();
		data.months = new String[] {"JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC"};
		for (int i=0; i<12; i++) {
			data.prices.add(i / 10.);
		}
		
		String s = new Gson().toJson(data);
		System.out.println("serialized = \n" + s);
		
		MonthPriceMatrix actual = new Gson().fromJson(s, MonthPriceMatrix.class);
		
		assertEquals("MAY", actual.months[4]);
		assertEquals(0.4, actual.prices.get(4));
	}
	
	public void testSerializeMatrix3AsMapOfColumns() {
		
		Map<String, List<Object>> data = new HashMap<String, List<Object>>();
		
		List<Object> monthList = new ArrayList<Object>();
		List<Object> priceList = new ArrayList<Object>();
		
		String[] months = new String[] {"JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC"};
		for (int i=0; i<12; i++) {
			monthList.add(months[i]);
			double price = i / 10.;
			priceList.add(price);
		}
		
		String monthIRI = "months"; //"http://www.theworldavatar.com/ontology/Time.owl#Month";
		data.put(monthIRI, monthList);
		String priceIRI = "prices"; //"http://www.theworldavatar.com/ontology/Economy.owl#Price";
		data.put(priceIRI, priceList);
		
		String s = new Gson().toJson(data);
		System.out.println("serialized = \n" + s);
		
		Map<String, List<Object>> actual = new Gson().fromJson(s, HashMap.class);
		String actualMonth = (String) actual.get(monthIRI).get(4);
		assertEquals("MAY", actualMonth);
		Double actualPrice = (Double) actual.get(priceIRI).get(4);
		assertEquals(0.4, actualPrice);
	}
	
	public void testSerializeMatrix4AsMapOfColumns() {
		
		
		List<Object> monthList = new ArrayList<Object>();
		List<Object> priceList = new ArrayList<Object>();
		
		String[] months = new String[] {"JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC"};
		for (int i=0; i<12; i++) {
			monthList.add(months[i]);
			double price = i / 10.;
			priceList.add(price);
		}
		
		MatrixConverter converter = new MatrixConverter();

		String monthIRI = "months"; //"http://www.theworldavatar.com/ontology/Time.owl#Month";
		converter.putColumn(monthIRI, monthList);
		String priceIRI = "prices"; //"http://www.theworldavatar.com/ontology/Economy.owl#Price";
		converter.putColumn(priceIRI, priceList);
		
		String s = converter.toJson();
		System.out.println("serialized = \n" + s);
		
		Map<String, List<Object>> actual = converter.fromJson(s);
		String actualMonth = (String) actual.get(monthIRI).get(4);
		assertEquals("MAY", actualMonth);
		Double actualPrice = (Double) actual.get(priceIRI).get(4);
		assertEquals(0.4, actualPrice);
	}
}
