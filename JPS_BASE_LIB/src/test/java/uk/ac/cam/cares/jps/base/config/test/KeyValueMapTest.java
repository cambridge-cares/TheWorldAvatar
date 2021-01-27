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
			assertEquals(e.getMessage(), KeyValueMap.NO_NULL_KEY_VALUE);
		}

		try {
			key = "new key";
		} finally {
			assertEquals(kvm.get(key), KeyValueMap.KEY_DOES_NOT_EXIST);
			kvm.map.clear();
		}

	}

	public void testPut() throws NoSuchMethodException, SecurityException {
		KeyValueMap kvm = KeyValueMap.getInstance();
		assertNotNull(kvm.getClass().getDeclaredMethod("put", String.class, String.class));
		String key = "key";
		String val = "val";

		try {
			kvm.put(key, val);
		} finally {
			assertEquals(kvm.get(key), val);
			kvm.map.clear();
		}

		try {
			key = "key";
			val = null;
			kvm.put(key, val);
		} catch (JPSRuntimeException e) {
			assertEquals(e.getMessage(), KeyValueMap.NO_NULL_KEY_VALUE);
		}

		try {
			key = null;
			val = "val";
			kvm.put(key, val);
		} catch (JPSRuntimeException e) {
			assertEquals(e.getMessage(), KeyValueMap.NO_NULL_KEY_VALUE);
		}

		try {
			key = null;
			val = null;
			kvm.put(key, val);
		} catch (JPSRuntimeException e) {
			assertEquals(e.getMessage(), KeyValueMap.NO_NULL_KEY_VALUE);
		}

	}

	public void testInit() throws NoSuchMethodException, SecurityException, IOException, IllegalAccessException,
			IllegalArgumentException, InvocationTargetException {
		KeyValueMap kvm = KeyValueMap.getInstance();
		assertNotNull(kvm.getClass().getDeclaredMethod("init", String.class));
		Method init = kvm.getClass().getDeclaredMethod("init", String.class);
		init.setAccessible(true);
		String runningForTest = "false";

		try {
			init.invoke(kvm, runningForTest);
		} finally {
			assertTrue(kvm.get("dataset.meta.url").contains("theworldavatar"));
			assertEquals(kvm.map.size(), 34);
			kvm.map.clear();
		}

		try {
			runningForTest = KeyValueMap.KEY_DOES_NOT_EXIST;
			init.invoke(kvm, runningForTest);
		} finally {
			assertTrue(kvm.get("dataset.meta.url").contains("theworldavatar"));
			kvm.map.clear();
		}

		try {
			runningForTest = "true";
			init.invoke(kvm, runningForTest);
		} finally {
			assertTrue(kvm.get("dataset.meta.url").contains("localhost"));
			kvm.map.clear();
		}
	}

	public void testRunningForTest() throws NoSuchMethodException, SecurityException, IllegalAccessException,
			IllegalArgumentException, InvocationTargetException, URISyntaxException {
		KeyValueMap kvm = KeyValueMap.getInstance();
		assertNotNull(kvm.getClass().getDeclaredMethod("runningForTest", String.class));
		Method runningForTest = kvm.getClass().getDeclaredMethod("runningForTest", String.class);
		runningForTest.setAccessible(true);
		String fileName = null;


		try {
			runningForTest.invoke(kvm, fileName);
		} catch (InvocationTargetException e) {
			assertEquals(e.getTargetException().getMessage(), KeyValueMap.FILE_DOES_NOT_EXIST);
		}

		try {
			fileName = " ";
			runningForTest.invoke(kvm, fileName);
		} catch (InvocationTargetException e) {
			assertEquals(e.getTargetException().getMessage(), KeyValueMap.FILE_DOES_NOT_EXIST);
		}

		try {
			fileName = KeyValueMap.propertiesFile;
			runningForTest.invoke(kvm, fileName);
		} finally {
			assertEquals(runningForTest.invoke(kvm, fileName), "true");
			kvm.map.clear();
		}

		try {
			fileName = KeyValueMap.testPropertiesFile;
			runningForTest.invoke(kvm, fileName);
		} finally {
			assertEquals(runningForTest.invoke(kvm, fileName), KeyValueMap.KEY_DOES_NOT_EXIST);
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
			assertEquals(e.getTargetException().getMessage(), KeyValueMap.FILE_DOES_NOT_EXIST);
		}

		try {
			fileName = " ";
			loadProperties.invoke(kvm, fileName);
		} catch (InvocationTargetException e) {
			assertEquals(e.getTargetException().getMessage(), KeyValueMap.FILE_DOES_NOT_EXIST);
		}

		try {
			fileName = KeyValueMap.propertiesFile;
			loadProperties.invoke(kvm, fileName);
		} finally {
			assertTrue(kvm.get("dataset.scenario.url").contains("theworldavatar"));
			kvm.map.clear();
		}

		try {
			fileName = KeyValueMap.testPropertiesFile;
			loadProperties.invoke(kvm, fileName);
		} finally {
			assertTrue(kvm.get("dataset.scenario.url").contains("localhost"));
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
			assertEquals(e.getMessage(), KeyValueMap.FILE_DOES_NOT_EXIST);
		}
		
		try {
			fileName = " ";
			kvm.getProperty(fileName, key);
		} catch (JPSRuntimeException e) {
			assertEquals(e.getMessage(), KeyValueMap.FILE_DOES_NOT_EXIST);
		}
		
		try {
			fileName = KeyValueMap.propertiesFile;
			kvm.getProperty(fileName, key);
		} catch (JPSRuntimeException e) {
			assertEquals(e.getMessage(), KeyValueMap.NO_NULL_KEY_VALUE);
			kvm.map.clear();
		}
		
		try {
			fileName = KeyValueMap.testPropertiesFile;
			key = "test";
			kvm.getProperty(fileName, key);
		} finally {
			assertEquals(kvm.getProperty(fileName, key), KeyValueMap.KEY_DOES_NOT_EXIST);
			kvm.map.clear();
		}
		
		try {
			fileName = KeyValueMap.propertiesFile;
			key = "test";
			kvm.getProperty(fileName, key);
		} finally {
			assertNotNull(kvm.getProperty(fileName, key));
			assertTrue(kvm.getProperty(fileName, key).length() > 0);
			assertTrue(kvm.getProperty(fileName, "absdir.root").contains("TOMCAT"));
			kvm.map.clear();
		}
	}

}
