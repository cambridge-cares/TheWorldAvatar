package uk.ac.cam.cares.jps.base.config.test;

import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.net.URISyntaxException;

import junit.framework.TestCase;
import org.junit.Assert;
import org.junit.Test;
import uk.ac.cam.cares.jps.base.config.KeyValueMap;
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
			Assert.assertEquals(kvm.getClass().getDeclaredFields().length, 7);
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
		String val = "val";
		String newVal = "newVal";

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
			IllegalArgumentException, InvocationTargetException, URISyntaxException, IOException {

		KeyValueMap kvm = KeyValueMap.getInstance();
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

//		// Should result in an exception as the properties file does not exist
//		try {
//			runningForTest.invoke(kvm, " ");
//			Assert.fail();
//		} catch (InvocationTargetException e) {
//			Assert.assertEquals(e.getTargetException().getMessage(), "Properties file does not exist.");
//		}

		Assert.assertEquals(runningForTest.invoke(kvm, "/jps.properties"), Boolean.valueOf(kvm.get("test")));

//		runningForTest.invoke(kvm, "/jpstest.properties");
//		Assert.assertEquals(runningForTest.invoke(kvm, "/jpstest.properties"), false);


	}
//
//	public void testLoadProperties() throws NoSuchMethodException, SecurityException, IllegalAccessException,
//			IllegalArgumentException, InvocationTargetException, IOException {
//		KeyValueMap kvm = KeyValueMap.getInstance();
//		assertNotNull(kvm.getClass().getDeclaredMethod("loadProperties", String.class));
//		Method loadProperties = kvm.getClass().getDeclaredMethod("loadProperties", String.class);
//		loadProperties.setAccessible(true);
//		String fileName = null;
//
//		try {
//			loadProperties.invoke(kvm, fileName);
//		} catch (InvocationTargetException e) {
//			assertEquals(e.getTargetException().getMessage(), "Properties file does not exist.");
//		}
//
//		try {
//			fileName = " ";
//			loadProperties.invoke(kvm, fileName);
//		} catch (InvocationTargetException e) {
//			assertEquals(e.getTargetException().getMessage(), "Properties file does not exist.");
//		}
//
//		try {
//			fileName = "/jps.properties";
//			loadProperties.invoke(kvm, fileName);
//			assertTrue(kvm.get("dataset.scenario.url").contains("theworldavatar"));
//		} finally {
//			kvm.map.clear();
//		}
//
//		try {
//			fileName = "/jpstest.properties";
//			loadProperties.invoke(kvm, fileName);
//			assertTrue(kvm.get("dataset.scenario.url").contains("localhost"));
//		} finally {
//			kvm.map.clear();
//		}
//
//	}
//
//	public void testGetProperty() throws NoSuchMethodException, SecurityException {
//		KeyValueMap kvm = KeyValueMap.getInstance();
//		assertNotNull(kvm.getClass().getDeclaredMethod("getProperty", String.class, String.class));
//		String fileName = null;
//		String key = null;
//
//		try {
//			kvm.getProperty(fileName, key);
//		} catch (Exception e) {
//			assertEquals(e.getMessage(), "Properties file does not exist.");
//		}
//
//		try {
//			fileName = " ";
//			key = "test";
//			kvm.getProperty(fileName, key);
//		} catch (JPSRuntimeException e) {
//			assertEquals(e.getMessage(), "Properties file does not exist.");
//		}
//
//		try {
//			fileName = "/jps.properties";
//			key = null;
//			kvm.getProperty(fileName, key);
//		} catch (JPSRuntimeException e) {
//			assertEquals(e.getMessage(), "Null keys and values are not allowed.");
//			kvm.map.clear();
//		}
//
//		try {
//			fileName = "/jpstest.properties";
//			key = "test";
//			kvm.getProperty(fileName, key);
//			assertEquals(kvm.getProperty(fileName, key), "No such key in properties file.");
//		} finally {
//			kvm.map.clear();
//		}
//
//		try {
//			fileName = "/jps.properties";
//			key = "test";
//			kvm.getProperty(fileName, key);
//			assertNotNull(kvm.getProperty(fileName, key));
//			assertTrue(kvm.getProperty(fileName, key).length() > 0);
//			assertTrue(kvm.getProperty(fileName, "absdir.root").contains("TOMCAT"));
//		} finally {
//			kvm.map.clear();
//		}
//	}
	
}
