package uk.ac.cam.cares.jps.base.derivation;

import java.lang.reflect.Field;
import java.time.Instant;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import org.json.JSONArray;
import org.json.JSONObject;
import org.junit.Assert;
import org.junit.Test;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

public class DerivationOutputsTest {
	private String class1 = "http://class1";
	private String class2 = "http://class2";
	private String class3 = "http://class3";
	private String instance1_1 = "http://instance1_1";
	private String instance1_2 = "http://instance1_2";
	private String instance2_1 = "http://instance2_1";
	private String instance2_2 = "http://instance2_2";
	private String instance2_3 = "http://instance2_3";
	private String instance3_1 = "http://instance3_1";

	@Test
	public void testConstructor1()
			throws NoSuchFieldException, SecurityException, IllegalArgumentException, IllegalAccessException {
		// initialise with constructor DerivationOutputs(JSONObject mappedInstances)
		JSONObject mappedInstances = new JSONObject();
		mappedInstances.put(class1, new JSONArray(Arrays.asList(instance1_1, instance1_2)));
		mappedInstances.put(class2, new JSONArray(Arrays.asList(instance2_1, instance2_2, instance2_3)));
		mappedInstances.put(class3, new JSONArray(Arrays.asList(instance3_1)));
		DerivationOutputs devOutputs = new DerivationOutputs(mappedInstances);
		// Retrieve the value of the private field 'outputs' of the client
		Field outputs = devOutputs.getClass().getDeclaredField("outputs");
		outputs.setAccessible(true);
		Map<String, List<String>> outputsf = (Map<String, List<String>>) outputs.get(devOutputs);
		// the retrieved value should contain the same values as the mappedInstances
		Assert.assertTrue(equalLists(Arrays.asList(instance1_1, instance1_2), outputsf.get(class1)));
		Assert.assertTrue(equalLists(Arrays.asList(instance2_1, instance2_2, instance2_3), outputsf.get(class2)));
		Assert.assertTrue(equalLists(Arrays.asList(instance3_1), outputsf.get(class3)));

		// should fail if given wrong JSON structure
		mappedInstances.put("http://key", new JSONObject().put("http://key2", "http://value"));
		JPSRuntimeException e = Assert.assertThrows(JPSRuntimeException.class,
				() -> new DerivationOutputs(mappedInstances));
		Assert.assertTrue(e.getMessage().contains(
				"Serilise the given JSONObject to DerivationOutputs is not supported:" + mappedInstances.toString()));
	}

	@Test
	public void testConstructor2()
			throws NoSuchFieldException, SecurityException, IllegalArgumentException, IllegalAccessException {
		// initialise with constructor DerivationOutputs(String rdfType, List<String>
		// iris)
		DerivationOutputs devOutputs = new DerivationOutputs(class1, Arrays.asList(instance1_1, instance1_2));
		// Retrieve the value of the private field 'outputs' of the client
		Field outputs = devOutputs.getClass().getDeclaredField("outputs");
		outputs.setAccessible(true);
		Map<String, List<String>> outputsf = (Map<String, List<String>>) outputs.get(devOutputs);
		// the retrieved value should contain the same values as the inputs to the
		// constructor
		Assert.assertTrue(equalLists(Arrays.asList(instance1_1, instance1_2), outputsf.get(class1)));
	}

	@Test
	public void testConstructor3()
			throws NoSuchFieldException, SecurityException, IllegalArgumentException, IllegalAccessException {
		// initialise with constructor DerivationOutputs(String rdfType, String iri)
		DerivationOutputs devOutputs = new DerivationOutputs(class3, instance3_1);
		// Retrieve the value of the private field 'outputs' of the client
		Field outputs = devOutputs.getClass().getDeclaredField("outputs");
		outputs.setAccessible(true);
		Map<String, List<String>> outputsf = (Map<String, List<String>>) outputs.get(devOutputs);
		// the retrieved value should contain the same values as the inputs to the
		// constructor
		Assert.assertTrue(equalLists(Arrays.asList(instance3_1), outputsf.get(class3)));
	}

	@Test
	public void testConstructor4()
			throws NoSuchFieldException, SecurityException, IllegalArgumentException, IllegalAccessException {
		// initialise with constructor DerivationOutputs(Map<String, List<String>>
		// outputs)
		Map<String, List<String>> mappedInstances = new HashMap<>();
		mappedInstances.put(class1, Arrays.asList(instance1_1, instance1_2));
		mappedInstances.put(class2, Arrays.asList(instance2_1, instance2_2, instance2_3));
		mappedInstances.put(class3, Arrays.asList(instance3_1));
		DerivationOutputs devOutputs = new DerivationOutputs(mappedInstances);
		// Retrieve the value of the private field 'outputs' of the client
		Field outputs = devOutputs.getClass().getDeclaredField("outputs");
		outputs.setAccessible(true);
		Map<String, List<String>> outputsf = (Map<String, List<String>>) outputs.get(devOutputs);
		// the retrieved value should contain the same values as the mappedInstances
		Assert.assertTrue(equalLists(Arrays.asList(instance1_1, instance1_2), outputsf.get(class1)));
		Assert.assertTrue(equalLists(Arrays.asList(instance2_1, instance2_2, instance2_3), outputsf.get(class2)));
		Assert.assertTrue(equalLists(Arrays.asList(instance3_1), outputsf.get(class3)));
	}

	@Test
	public void testRetrievedInputsAt() {
		DerivationOutputs devOutputs = new DerivationOutputs(class3, instance3_1);
		// retrievedInputsAt not initialised yet
		Assert.assertEquals(0, devOutputs.getRetrievedInputsAt());
		// initialise retrievedInputsAt
		long timestamp = Instant.now().getEpochSecond();
		devOutputs.setRetrievedInputsAt(timestamp);
		Assert.assertEquals(timestamp, devOutputs.getRetrievedInputsAt());
	}

	@Test
	public void testAddToOutputs1() {
		// initialise with constructor DerivationOutputs(JSONObject mappedInstances)
		JSONObject mappedInstances = new JSONObject();
		mappedInstances.put(class1, new JSONArray(Arrays.asList(instance1_1)));
		DerivationOutputs devOutputs = new DerivationOutputs(mappedInstances);
		devOutputs.addToOutputs(class1, instance1_2);
		devOutputs.addToOutputs(class2, Arrays.asList(instance2_1));
		devOutputs.addToOutputs(class2, Arrays.asList(instance2_2, instance2_3));
		devOutputs.addToOutputs(class3, instance3_1);
		Map<String, List<String>> outputs = devOutputs.getOutputs();
		// the retrieved value should contain the same values as the mappedInstances
		Assert.assertTrue(equalLists(Arrays.asList(instance1_1, instance1_2), outputs.get(class1)));
		Assert.assertTrue(equalLists(Arrays.asList(instance2_1, instance2_2, instance2_3), outputs.get(class2)));
		Assert.assertTrue(equalLists(Arrays.asList(instance3_1), outputs.get(class3)));
	}

	@Test
	public void testAddToOutputs2() {
		// initialise with constructor DerivationOutputs(String rdfType, List<String>
		// iris)
		DerivationOutputs devOutputs = new DerivationOutputs(class1, Arrays.asList(instance1_1));
		devOutputs.addToOutputs(class1, instance1_2);
		devOutputs.addToOutputs(class2, Arrays.asList(instance2_1));
		devOutputs.addToOutputs(class2, Arrays.asList(instance2_2, instance2_3));
		devOutputs.addToOutputs(class3, instance3_1);
		Map<String, List<String>> outputs = devOutputs.getOutputs();
		// the retrieved value should contain the same values as the mappedInstances
		Assert.assertTrue(equalLists(Arrays.asList(instance1_1, instance1_2), outputs.get(class1)));
		Assert.assertTrue(equalLists(Arrays.asList(instance2_1, instance2_2, instance2_3), outputs.get(class2)));
		Assert.assertTrue(equalLists(Arrays.asList(instance3_1), outputs.get(class3)));
	}

	@Test
	public void testAddToOutputs3() {
		// initialise with constructor DerivationOutputs(String rdfType, String iri)
		DerivationOutputs devOutputs = new DerivationOutputs(class1, instance1_1);
		devOutputs.addToOutputs(class1, instance1_2);
		devOutputs.addToOutputs(class2, Arrays.asList(instance2_1));
		devOutputs.addToOutputs(class2, Arrays.asList(instance2_2, instance2_3));
		devOutputs.addToOutputs(class3, instance3_1);
		Map<String, List<String>> outputs = devOutputs.getOutputs();
		// the retrieved value should contain the same values as the mappedInstances
		Assert.assertTrue(equalLists(Arrays.asList(instance1_1, instance1_2), outputs.get(class1)));
		Assert.assertTrue(equalLists(Arrays.asList(instance2_1, instance2_2, instance2_3), outputs.get(class2)));
		Assert.assertTrue(equalLists(Arrays.asList(instance3_1), outputs.get(class3)));
	}

	@Test
	public void testAddToOutputs4() {
		// initialise with constructor DerivationOutputs(Map<String, List<String>>
		// outputs)
		Map<String, List<String>> mappedInstances = new HashMap<>();
		mappedInstances.put(class1, Arrays.asList(instance1_1));
		DerivationOutputs devOutputs = new DerivationOutputs(mappedInstances);
		devOutputs.addToOutputs(class1, instance1_2);
		devOutputs.addToOutputs(class2, Arrays.asList(instance2_1));
		devOutputs.addToOutputs(class2, Arrays.asList(instance2_2, instance2_3));
		devOutputs.addToOutputs(class3, instance3_1);
		Map<String, List<String>> outputs = devOutputs.getOutputs();
		// the retrieved value should contain the same values as the mappedInstances
		Assert.assertTrue(equalLists(Arrays.asList(instance1_1, instance1_2), outputs.get(class1)));
		Assert.assertTrue(equalLists(Arrays.asList(instance2_1, instance2_2, instance2_3), outputs.get(class2)));
		Assert.assertTrue(equalLists(Arrays.asList(instance3_1), outputs.get(class3)));
	}

	@Test
	public void testGetNewDerivedIRI() {
		Map<String, List<String>> mappedInstances = new HashMap<>();
		mappedInstances.put(class1, Arrays.asList(instance1_1, instance1_2));
		mappedInstances.put(class2, Arrays.asList(instance2_1, instance2_2, instance2_3));
		mappedInstances.put(class3, Arrays.asList(instance3_1));
		DerivationOutputs devOutputs = new DerivationOutputs(mappedInstances);
		List<String> expectedNewDerivedIRI = Arrays.asList(instance1_1, instance1_2, instance2_1, instance2_2,
				instance2_3, instance3_1);
		Assert.assertTrue(equalLists(expectedNewDerivedIRI, devOutputs.getNewDerivedIRI()));
	}

	@Test
	public void testToJson() {
		Map<String, List<String>> mappedInstances = new HashMap<>();
		mappedInstances.put(class1, Arrays.asList(instance1_1, instance1_2));
		mappedInstances.put(class2, Arrays.asList(instance2_1, instance2_2, instance2_3));
		mappedInstances.put(class3, Arrays.asList(instance3_1));
		DerivationOutputs devOutputs = new DerivationOutputs(mappedInstances);
		JSONObject toJson = devOutputs.toJson();
		// the toJson value should contain the same values as the mappedInstances
		Assert.assertTrue(equalLists(Arrays.asList(instance1_1, instance1_2),
				IntStream.range(0, toJson.getJSONArray(class1).length())
						.mapToObj(i -> toJson.getJSONArray(class1).getString(i)).collect(Collectors.toList())));
		Assert.assertTrue(equalLists(Arrays.asList(instance2_1, instance2_2, instance2_3),
				IntStream.range(0, toJson.getJSONArray(class2).length())
						.mapToObj(i -> toJson.getJSONArray(class2).getString(i)).collect(Collectors.toList())));
		Assert.assertTrue(
				equalLists(Arrays.asList(instance3_1), IntStream.range(0, toJson.getJSONArray(class3).length())
						.mapToObj(i -> toJson.getJSONArray(class3).getString(i)).collect(Collectors.toList())));
	}

	////////////////////////////////////////////////////////////
	// Below are utility functions to reduce code-duplication //
	////////////////////////////////////////////////////////////

	public boolean equalLists(List<String> a, List<String> b) {
		if (a == null && b == null) {
			return true;
		}
		if ((a == null && b != null) || (a != null && b == null) || (a.size() != b.size())) {
			return false;
		}
		Collections.sort(a);
		Collections.sort(b);
		return a.equals(b);
	}
}
