package uk.ac.cam.cares.jps.base.derivation;

import java.lang.reflect.Field;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.json.JSONArray;
import org.json.JSONObject;
import org.junit.Assert;
import org.junit.Test;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

public class DerivationInputsTest {
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
		// initialise with constructor DerivationInputs(JSONObject mappedInputs)
		JSONObject mappedInstances = new JSONObject();
		mappedInstances.put(class1, new JSONArray(Arrays.asList(instance1_1, instance1_2)));
		mappedInstances.put(class2, new JSONArray(Arrays.asList(instance2_1, instance2_2, instance2_3)));
		mappedInstances.put(class3, new JSONArray(Arrays.asList(instance3_1)));
		DerivationInputs devInputs = new DerivationInputs(mappedInstances);
		// Retrieve the value of the private field 'outputs' of the client
		Field inputs = devInputs.getClass().getDeclaredField("inputs");
		inputs.setAccessible(true);
		Map<String, List<String>> inputsf = (Map<String, List<String>>) inputs.get(devInputs);
		// the retrieved value should contain the same values as the mappedInstances
		Assert.assertTrue(equalLists(Arrays.asList(instance1_1, instance1_2), inputsf.get(class1)));
		Assert.assertTrue(equalLists(Arrays.asList(instance2_1, instance2_2, instance2_3), inputsf.get(class2)));
		Assert.assertTrue(equalLists(Arrays.asList(instance3_1), inputsf.get(class3)));

		// should fail if given wrong JSON structure
		mappedInstances.put("http://key", new JSONObject().put("http://key2", "http://value"));
		JPSRuntimeException e = Assert.assertThrows(JPSRuntimeException.class,
				() -> new DerivationInputs(mappedInstances));
		Assert.assertTrue(e.getMessage()
				.contains(DerivationInputs.DERIVATIONINPUTS_SERIALISE_ERROR + mappedInstances.toString()));
	}

	@Test
	public void testConstructor2()
			throws NoSuchFieldException, SecurityException, IllegalArgumentException, IllegalAccessException {
		// initialise with constructor DerivationInputs(List<Entity> entities)
		Entity entity1 = new Entity(instance1_1);
		entity1.setRdfType(class1);
		Entity entity2 = new Entity(instance1_2);
		entity2.setRdfType(class1);
		Entity entity3 = new Entity(instance2_1);
		entity3.setRdfType(class2);
		Entity entity4 = new Entity(instance2_2);
		entity4.setRdfType(class2);
		Entity entity5 = new Entity(instance2_3);
		entity5.setRdfType(class2);
		Entity entity6 = new Entity(instance3_1);
		entity6.setRdfType(class3);
		List<Entity> entities = Arrays.asList(entity1, entity2, entity3, entity4, entity5, entity6);

		DerivationInputs devInputs = new DerivationInputs(entities);
		// Retrieve the value of the private field 'outputs' of the client
		Field inputs = devInputs.getClass().getDeclaredField("inputs");
		inputs.setAccessible(true);
		Map<String, List<String>> inputsf = (Map<String, List<String>>) inputs.get(devInputs);
		// the retrieved value should contain the same values as the mappedInstances
		Assert.assertTrue(equalLists(Arrays.asList(instance1_1, instance1_2), inputsf.get(class1)));
		Assert.assertTrue(equalLists(Arrays.asList(instance2_1, instance2_2, instance2_3), inputsf.get(class2)));
		Assert.assertTrue(equalLists(Arrays.asList(instance3_1), inputsf.get(class3)));
	}

	@Test
	public void testConstructor3()
			throws NoSuchFieldException, SecurityException, IllegalArgumentException, IllegalAccessException {
		// initialise with constructor DerivationInputs(JSONObject mappedInputs)
		Map<String, List<String>> mappedInstances = new HashMap<>();
		mappedInstances.put(class1, Arrays.asList(instance1_1, instance1_2));
		mappedInstances.put(class2, Arrays.asList(instance2_1, instance2_2, instance2_3));
		mappedInstances.put(class3, Arrays.asList(instance3_1));
		DerivationInputs devInputs = new DerivationInputs(mappedInstances);
		// Retrieve the value of the private field 'outputs' of the client
		Field inputs = devInputs.getClass().getDeclaredField("inputs");
		inputs.setAccessible(true);
		Map<String, List<String>> inputsf = (Map<String, List<String>>) inputs.get(devInputs);
		// the retrieved value should contain the same values as the mappedInstances
		Assert.assertTrue(equalLists(Arrays.asList(instance1_1, instance1_2), inputsf.get(class1)));
		Assert.assertTrue(equalLists(Arrays.asList(instance2_1, instance2_2, instance2_3), inputsf.get(class2)));
		Assert.assertTrue(equalLists(Arrays.asList(instance3_1), inputsf.get(class3)));
	}

	@Test
	public void testGetIris() {
		JSONObject mappedInstances = new JSONObject();
		mappedInstances.put(class1, new JSONArray(Arrays.asList(instance1_1, instance1_2)));
		mappedInstances.put(class2, new JSONArray(Arrays.asList(instance2_1, instance2_2, instance2_3)));
		mappedInstances.put(class3, new JSONArray(Arrays.asList(instance3_1)));
		DerivationInputs devInputs = new DerivationInputs(mappedInstances);

		Assert.assertTrue(equalLists(Arrays.asList(instance1_1, instance1_2), devInputs.getIris(class1)));
		Assert.assertTrue(equalLists(Arrays.asList(instance2_1, instance2_2, instance2_3), devInputs.getIris(class2)));
		Assert.assertTrue(equalLists(Arrays.asList(instance3_1), devInputs.getIris(class3)));
	}

	@Test
	public void testAddToInputs1() {
		// initialise with constructor DerivationInputs(JSONObject mappedInputs)
		JSONObject mappedInstances = new JSONObject();
		mappedInstances.put(class1, new JSONArray(Arrays.asList(instance1_1)));
		DerivationInputs devInputs = new DerivationInputs(mappedInstances);
		devInputs.addToInputs("<" + class1 + ">", "<" + instance1_2 + ">");
		devInputs.addToInputs(class2 + ">", Arrays.asList("<" + instance2_1 + ">"));
		devInputs.addToInputs("<" + class2, Arrays.asList("<" + instance2_2, instance2_3 + ">"));
		devInputs.addToInputs("<" + class3 + ">", "<" + instance3_1);
		Map<String, List<String>> outputs = devInputs.getInputs();
		// the retrieved value should contain the same values as the mappedInstances
		Assert.assertTrue(equalLists(Arrays.asList(instance1_1, instance1_2), outputs.get(class1)));
		Assert.assertTrue(equalLists(Arrays.asList(instance2_1, instance2_2, instance2_3), outputs.get(class2)));
		Assert.assertTrue(equalLists(Arrays.asList(instance3_1), outputs.get(class3)));
	}

	@Test
	public void testAddToInputs2() {
		// initialise with constructor DerivationInputs(List<Entity> entities)
		Entity entity1 = new Entity(instance1_1);
		entity1.setRdfType(class1);
		DerivationInputs devInputs = new DerivationInputs(Arrays.asList(entity1));
		devInputs.addToInputs("<" + class1, instance1_2 + ">");
		devInputs.addToInputs(class2 + ">", Arrays.asList("<" + instance2_1 + ">"));
		devInputs.addToInputs(class2 + ">", Arrays.asList("<" + instance2_2 + ">", "<" + instance2_3));
		devInputs.addToInputs("<" + class3 + ">", "<" + instance3_1 + ">");
		Map<String, List<String>> outputs = devInputs.getInputs();
		// the retrieved value should contain the same values as the mappedInstances
		Assert.assertTrue(equalLists(Arrays.asList(instance1_1, instance1_2), outputs.get(class1)));
		Assert.assertTrue(equalLists(Arrays.asList(instance2_1, instance2_2, instance2_3), outputs.get(class2)));
		Assert.assertTrue(equalLists(Arrays.asList(instance3_1), outputs.get(class3)));
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
