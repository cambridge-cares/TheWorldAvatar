package uk.ac.cam.cares.jps.base.config;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

import org.junit.Assert;
import org.junit.Test;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

public class KeyValueMapTest {


	@Test
	public void testGetInstance() {
		KeyValueMap kvm = null;

		try {
			kvm = KeyValueMap.getInstance();
		} finally {
			Assert.assertNotNull(kvm);
			Assert.assertEquals(kvm.getClass(), KeyValueMap.class);
			Assert.assertEquals(kvm.getClass().getDeclaredFields().length, 8);
			Assert.assertEquals(kvm.getClass().getDeclaredMethods().length, 7);
		}

	}

	@Test
	public void testGet() throws NoSuchMethodException, SecurityException {
		KeyValueMap kvm = KeyValueMap.getInstance();
		Assert.assertNotNull(kvm.getClass().getDeclaredMethod("get", String.class));
		String key = "key";
		String val = "val";
		kvm.put(key, val);
		Assert.assertEquals(kvm.get(key), val);

		// Accessing a key with value null should result in an exception
		try {
			kvm.get(null);
			Assert.fail();
		} catch (JPSRuntimeException e) {
			Assert.assertEquals(e.getMessage(), "Null keys and values are not allowed.");
		}

		key = "new key";
		Assert.assertEquals(kvm.get(key), "No such key in properties file.");
	}

	@Test
	public void testPut() throws NoSuchMethodException, SecurityException {
		KeyValueMap kvm = KeyValueMap.getInstance();
		Assert.assertNotNull(kvm.getClass().getDeclaredMethod("put", String.class, String.class));
		String key = "key";
		String newKey = "newKey";
		String val = "val";
		String newVal = "newVal";

		// Putting a value with a new key will return null instead of the old value
		Assert.assertNull(kvm.put(newKey, newVal));
		// Putting a value with a an existing key should return the old value
		kvm.put(key, val);
		Assert.assertEquals(kvm.get(key), val);
		Assert.assertEquals(kvm.put(key, newVal), val);
		Assert.assertEquals(kvm.get(key), newVal);

		// Putting a null value should result in an exception
		try {
			key = "key";
			kvm.put(key, null);
			Assert.fail();
		} catch (JPSRuntimeException e) {
			Assert.assertEquals(e.getMessage(), "Null keys and values are not allowed.");
		}

		// Putting a value with a null key should result in an exception
		try {
			val = "val";
			kvm.put(null, val);
			Assert.fail();
		} catch (JPSRuntimeException e) {
			Assert.assertEquals(e.getMessage(), "Null keys and values are not allowed.");
		}

		// Putting a null value with a null key should result in an exception
		try {
			kvm.put(null, null);
			Assert.fail();
		} catch (JPSRuntimeException e) {
			Assert.assertEquals(e.getMessage(), "Null keys and values are not allowed.");
		}

	}

	@Test
	public void testInit() throws NoSuchMethodException, SecurityException, IllegalAccessException,
			IllegalArgumentException, InvocationTargetException {
		KeyValueMap kvm = KeyValueMap.getInstance();

		Assert.assertNotNull(kvm.getClass().getDeclaredMethod("init", Boolean.class));
		Method init = kvm.getClass().getDeclaredMethod("init", Boolean.class);
		init.setAccessible(true);

		init.invoke(kvm, false);
		Assert.assertTrue(kvm.get("dataset.meta.url").contains("theworldavatar"));

		init.invoke(kvm, true);
		Assert.assertTrue(kvm.get("dataset.meta.url").contains("localhost"));
	}

	@Test
	public void testRunningForTest() throws NoSuchMethodException, SecurityException, IllegalAccessException,
			IllegalArgumentException, InvocationTargetException {


		KeyValueMap kvm = KeyValueMap.getInstance();
		// Use reflections to make runningForTestMethod accessible
		Assert.assertNotNull(kvm.getClass().getDeclaredMethod("runningForTest", String.class));
		Method runningForTest = kvm.getClass().getDeclaredMethod("runningForTest", String.class);
		runningForTest.setAccessible(true);

		// Should result in an exception as the properties file is null
		try {
			String filename = null;
			runningForTest.invoke(kvm, filename);
			Assert.fail();
		} catch (InvocationTargetException e) {
			Assert.assertEquals(e.getTargetException().getMessage(), "Properties file does not exist.");
		}

		// Should result in an exception as the properties file does not exist
		try {
			runningForTest.invoke(kvm, "this_is_not_a_valid_filename");
			Assert.fail();
		} catch (InvocationTargetException e) {
			Assert.assertEquals(e.getTargetException().getMessage(), "Properties file does not exist.");
		}

		Assert.assertEquals(runningForTest.invoke(kvm, "/jps.properties"), Boolean.valueOf(kvm.get("test")));

		// TODO: Requires refactoring of the KeyValueMap class as a reset of the map in KeyValueMap effects other tests.
		// kvm.map.clear()
		// runningForTest.invoke(kvm, "/jpstest.properties");
		// Assert.assertEquals(runningForTest.invoke(kvm, "/jpstest.properties"), false);
	}

	@Test
	public void testLoadProperties() throws NoSuchMethodException, SecurityException, IllegalAccessException,
			IllegalArgumentException, InvocationTargetException {

		KeyValueMap kvm = KeyValueMap.getInstance();
		// Use reflections to make loadProperties accessible
		Assert.assertNotNull(kvm.getClass().getDeclaredMethod("loadProperties", String.class));
		Method loadProperties = kvm.getClass().getDeclaredMethod("loadProperties", String.class);
		loadProperties.setAccessible(true);


		String fileName = null;
		// Should result in an exception as the properties file is null
		try {
			loadProperties.invoke(kvm, fileName);
			Assert.fail();
		} catch (InvocationTargetException e) {
			Assert.assertEquals(e.getTargetException().getMessage(), "Properties file does not exist.");
		}
		// Should result in an exception as the properties file does not exist
		try {
			fileName = "this_is_not_a_valid_filename";
			loadProperties.invoke(kvm, fileName);
			Assert.fail();
		} catch (InvocationTargetException e) {
			Assert.assertEquals(e.getTargetException().getMessage(), "Properties file does not exist.");
		}

		// TODO: Is dependent on the values in the properties files
		fileName = "/jps.properties";
		loadProperties.invoke(kvm, fileName);
		Assert.assertTrue(kvm.get("dataset.scenario.url").contains("theworldavatar"));

		fileName = "/jpstest.properties";
		loadProperties.invoke(kvm, fileName);
		Assert.assertTrue(kvm.get("dataset.scenario.url").contains("localhost"));
	}

	@Test
	public void testGetProperty() throws NoSuchMethodException, SecurityException {
		KeyValueMap kvm = KeyValueMap.getInstance();
		Assert.assertNotNull(kvm.getClass().getDeclaredMethod("getProperty", String.class, String.class));

		// Should result in an exception as the properties file does not exist
		try {
			KeyValueMap.getProperty(null, null);
			Assert.fail();
		} catch (JPSRuntimeException e) {
			Assert.assertEquals(e.getMessage(), "Properties file does not exist.");
		}
		// Should result in an exception as the properties file does not exist
		try {
			KeyValueMap.getProperty("this_is_not_a_valid_filename", "test");
			Assert.fail();
		} catch (JPSRuntimeException e) {
			Assert.assertEquals(e.getMessage(), "Properties file does not exist.");
		}
		// Accessing a null key should result in an exception
		try {
			KeyValueMap.getProperty("/jps.properties", null);
			Assert.fail();
		} catch (JPSRuntimeException e) {
			Assert.assertEquals(e.getMessage(), "Null keys and values are not allowed.");
		}
		// Accessing a key that does not exist should result in an specific message
		Assert.assertEquals(KeyValueMap.getProperty("/jpstest.properties", "test"), "No such key in properties file.");

		String fileName = "/jps.properties";
		String key = "test";
		Assert.assertNotNull(KeyValueMap.getProperty(fileName, key));
		Assert.assertEquals("true", KeyValueMap.getProperty(fileName, key));
		Assert.assertTrue(KeyValueMap.getProperty(fileName, "absdir.root").contains("TOMCAT"));
	}
}
