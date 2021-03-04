package uk.ac.cam.cares.jps.base.config.test;

import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.net.URISyntaxException;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.config.KeyValueMap;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

public class KeyValueMapTest extends TestCase {

	public void testGetInstance() throws IOException {
		KeyValueMap kvm = null;

		try {
			kvm = KeyValueMap.getInstance();
		} finally {
			assertNotNull(kvm);
			assertEquals(kvm.getClass(), KeyValueMap.class);
			assertEquals(kvm.getClass().getDeclaredFields().length, 7);
			assertEquals(kvm.getClass().getDeclaredMethods().length, 7);
		}
	}

	public void testGet() throws NoSuchMethodException, SecurityException {
		KeyValueMap kvm = KeyValueMap.getInstance();
		assertNotNull(kvm.getClass().getDeclaredMethod("get", String.class));
		String key = "key";
		String val = "val";
		kvm.put(key, val);
		assertEquals(kvm.get(key), val);

		try {
			key = null;
			kvm.get(key);
		} catch (JPSRuntimeException e) {
			assertEquals(e.getMessage(), "Null keys and values are not allowed.");
		}

		try {
			key = "new key";
			assertEquals(kvm.get(key), "No such key in properties file.");
		} finally {
			kvm.map.clear();
		}

	}

	public void testPut() throws NoSuchMethodException, SecurityException {
		KeyValueMap kvm = KeyValueMap.getInstance();
		assertNotNull(kvm.getClass().getDeclaredMethod("put", String.class, String.class));
		String key = "key";
		String val = "val";
		String newVal = "newVal";
		
		try {
			assertEquals(kvm.put(key, val), null);
		} finally {
			kvm.map.clear();
		}

		try {
			kvm.put(key, val);
			assertEquals(kvm.get(key), val);
			assertEquals(kvm.put(key, newVal), val);
			assertEquals(kvm.get(key), newVal);
		} finally {
			kvm.map.clear();
		}

		try {
			key = "key";
			val = null;
			kvm.put(key, val);
		} catch (JPSRuntimeException e) {
			assertEquals(e.getMessage(), "Null keys and values are not allowed.");
		}

		try {
			key = null;
			val = "val";
			kvm.put(key, val);
		} catch (JPSRuntimeException e) {
			assertEquals(e.getMessage(), "Null keys and values are not allowed.");
		}

		try {
			key = null;
			val = null;
			kvm.put(key, val);
		} catch (JPSRuntimeException e) {
			assertEquals(e.getMessage(), "Null keys and values are not allowed.");
		}

	}

	public void testInit() throws NoSuchMethodException, SecurityException, IOException, IllegalAccessException,
			IllegalArgumentException, InvocationTargetException {
		KeyValueMap kvm = KeyValueMap.getInstance();
		assertNotNull(kvm.getClass().getDeclaredMethod("init", Boolean.class));
		Method init = kvm.getClass().getDeclaredMethod("init", Boolean.class);
		init.setAccessible(true);
		Boolean runningForTest = false;

		try {
			init.invoke(kvm, runningForTest);
			assertTrue(kvm.get("dataset.meta.url").contains("theworldavatar"));
		} finally {
			kvm.map.clear();
		}

		try {
			runningForTest = true;
			init.invoke(kvm, runningForTest);
			assertTrue(kvm.get("dataset.meta.url").contains("localhost"));
		} finally {
			kvm.map.clear();
		}
	}

	public void testRunningForTest() throws NoSuchMethodException, SecurityException, IllegalAccessException,
			IllegalArgumentException, InvocationTargetException, URISyntaxException, IOException {
		KeyValueMap kvm = KeyValueMap.getInstance();
		assertNotNull(kvm.getClass().getDeclaredMethod("runningForTest", String.class));
		Method runningForTest = kvm.getClass().getDeclaredMethod("runningForTest", String.class);
		runningForTest.setAccessible(true);
		String fileName = null;
		
		try {
			runningForTest.invoke(kvm, fileName);
		} catch (InvocationTargetException e) {
			assertEquals(e.getTargetException().getMessage(), "Properties file does not exist.");
		}

		try {
			fileName = " ";
			runningForTest.invoke(kvm, fileName);
		} catch (InvocationTargetException e) {
			assertEquals(e.getTargetException().getMessage(), "Properties file does not exist.");
		}

		try {
			fileName = "/jps.properties";
			assertEquals(runningForTest.invoke(kvm, fileName), Boolean.valueOf(kvm.get("test")));
		} finally {
			kvm.map.clear();
		}

		try {
			fileName = "/jpstest.properties";
			runningForTest.invoke(kvm, fileName);
			assertEquals(runningForTest.invoke(kvm, fileName), false);
		} finally {
			kvm.map.clear();
		}

	}

	public void testLoadProperties() throws NoSuchMethodException, SecurityException, IllegalAccessException,
			IllegalArgumentException, InvocationTargetException, IOException {
		KeyValueMap kvm = KeyValueMap.getInstance();
		assertNotNull(kvm.getClass().getDeclaredMethod("loadProperties", String.class));
		Method loadProperties = kvm.getClass().getDeclaredMethod("loadProperties", String.class);
		loadProperties.setAccessible(true);
		String fileName = null;

		try {
			loadProperties.invoke(kvm, fileName);
		} catch (InvocationTargetException e) {
			assertEquals(e.getTargetException().getMessage(), "Properties file does not exist.");
		}

		try {
			fileName = " ";
			loadProperties.invoke(kvm, fileName);
		} catch (InvocationTargetException e) {
			assertEquals(e.getTargetException().getMessage(), "Properties file does not exist.");
		}

		try {
			fileName = "/jps.properties";
			loadProperties.invoke(kvm, fileName);
			assertTrue(kvm.get("dataset.scenario.url").contains("theworldavatar"));
		} finally {
			kvm.map.clear();
		}

		try {
			fileName = "/jpstest.properties";
			loadProperties.invoke(kvm, fileName);
			assertTrue(kvm.get("dataset.scenario.url").contains("localhost"));
		} finally {
			kvm.map.clear();
		}

	}

	public void testGetProperty() throws NoSuchMethodException, SecurityException {
		KeyValueMap kvm = KeyValueMap.getInstance();
		assertNotNull(kvm.getClass().getDeclaredMethod("getProperty", String.class, String.class));
		String fileName = null;
		String key = null;

		try {
			kvm.getProperty(fileName, key);
		} catch (Exception e) {
			assertEquals(e.getMessage(), "Properties file does not exist.");
		}

		try {
			fileName = " ";
			key = "test";
			kvm.getProperty(fileName, key);
		} catch (JPSRuntimeException e) {
			assertEquals(e.getMessage(), "Properties file does not exist.");
		}

		try {
			fileName = "/jps.properties";
			key = null;
			kvm.getProperty(fileName, key);
		} catch (JPSRuntimeException e) {
			assertEquals(e.getMessage(), "Null keys and values are not allowed.");
			kvm.map.clear();
		}

		try {
			fileName = "/jpstest.properties";
			key = "test";
			kvm.getProperty(fileName, key);
			assertEquals(kvm.getProperty(fileName, key), "No such key in properties file.");
		} finally {
			kvm.map.clear();
		}

		try {
			fileName = "/jps.properties";
			key = "test";
			kvm.getProperty(fileName, key);
			assertNotNull(kvm.getProperty(fileName, key));
			assertTrue(kvm.getProperty(fileName, key).length() > 0);
			assertTrue(kvm.getProperty(fileName, "absdir.root").contains("TOMCAT"));
		} finally {
			kvm.map.clear();
		}
	}
	
}
