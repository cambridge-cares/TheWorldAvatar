package uk.ac.cam.cares.jps.base.config;

import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.nio.file.Paths;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

public class AgentLocatorTest extends TestCase {

	public void testGetSingleton() {
		AgentLocator al = null;

		try {
			al = AgentLocator.getSingleton();
		} finally {
			assertNotNull(al);
			assertEquals(al.getClass(), AgentLocator.class);
			assertEquals(al.getClass().getDeclaredFields().length, 11);
			assertEquals(al.getClass().getDeclaredMethods().length, 10);
		}

	}

	public void testInit() throws NoSuchMethodException, SecurityException, IllegalAccessException,
			IllegalArgumentException, InvocationTargetException, NoSuchFieldException {
		AgentLocator al = AgentLocator.getSingleton();
		assertNotNull(al.getClass().getDeclaredMethod("init"));
		Method init = al.getClass().getDeclaredMethod("init");
		assertNotNull(al.getClass().getDeclaredField("jpsBaseDirectory"));
		Field jpsBaseDirectory = al.getClass().getDeclaredField("jpsBaseDirectory");
		assertNotNull(al.getClass().getDeclaredField("url"));
		Field url = al.getClass().getDeclaredField("url");
		init.setAccessible(true);
		jpsBaseDirectory.setAccessible(true);
		url.setAccessible(true);

		init.invoke(al);
		assertNotNull(jpsBaseDirectory.get(al));
		assertTrue(jpsBaseDirectory.get(al).toString().contains("JPS_BASE_LIB"));
		assertNotNull(url.get(al));
		assertTrue(url.get(al).toString().contains("http"));
		assertTrue(url.get(al).toString().contains("80"));

	}

	public void testGetCurrentJpsAppDirectory() throws NoSuchMethodException, SecurityException {
		AgentLocator al = AgentLocator.getSingleton();
		assertNotNull(al.getClass().getDeclaredMethod("getCurrentJpsAppDirectory", Object.class));
		Object obj = null;
		
		try {
			al.getCurrentJpsAppDirectory(obj);
		} catch (JPSRuntimeException e) {
			assertEquals(e.getMessage(), "Path could not be constructed.");
		}
		
		obj = this;
		assertNotNull(al.getCurrentJpsAppDirectory(obj));
		assertEquals(al.getCurrentJpsAppDirectory(obj), al.getCurrentJpsAppDirectory(KeyValueMap.getInstance()));
		assertTrue(al.getCurrentJpsAppDirectory(obj).contains("JPS_BASE_LIB"));
				
	}

	public void testGetJPSBaseDirectory() throws NoSuchMethodException, SecurityException {
		AgentLocator al = AgentLocator.getSingleton();
		assertNotNull(al.getClass().getDeclaredMethod("getJPSBaseDirectory"));
		assertTrue(al.getJPSBaseDirectory().contains("JPS_BASE_LIB"));
		assertEquals(al.getJPSBaseDirectory(), al.getCurrentJpsAppDirectory(KeyValueMap.getInstance()));

	}

	public void testIsJPSRunningForTest() throws NoSuchMethodException, SecurityException {
		AgentLocator al = AgentLocator.getSingleton();
		assertNotNull(al.getClass().getDeclaredMethod("isJPSRunningForTest"));
		assertNotNull(al.isJPSRunningForTest());
		assertEquals(Boolean.toString(al.isJPSRunningForTest()), KeyValueMap.getInstance().get("test"));

	}

	public void testGetNewPathToPythonScript() throws NoSuchMethodException, SecurityException {
		AgentLocator al = AgentLocator.getSingleton();
		assertNotNull(al.getClass().getDeclaredMethod("getNewPathToPythonScript", String.class, Object.class));
		String pyScript = null;
		Object obj = null;

		try {
			al.getNewPathToPythonScript(pyScript, obj);
		} catch (JPSRuntimeException e) {
			assertEquals(e.getMessage(), "Path could not be constructed.");
		}

		try {
			pyScript = "python_script.py";
			obj = null;
			al.getNewPathToPythonScript(pyScript, obj);
		} catch (JPSRuntimeException e) {
			assertEquals(e.getMessage(), "Path could not be constructed.");
		}

		try {
			pyScript = null;
			obj = this;
			al.getNewPathToPythonScript(pyScript, obj);
		} catch (JPSRuntimeException e) {
			assertEquals(e.getMessage(), "Path could not be constructed.");
		}

		pyScript = "python_script.py";
		obj = this;
		String substring = Paths.get("JPS_BASE_LIB/python/python_script.py").toString();
		assertTrue(al.getNewPathToPythonScript(pyScript, obj).contains(substring));

	}

	public void testGetPathToWorkingDir() throws NoSuchMethodException, SecurityException {
		AgentLocator al = AgentLocator.getSingleton();
		assertNotNull(al.getClass().getDeclaredMethod("getPathToWorkingDir", Object.class));
		Object obj = null;

		try {
			al.getPathToWorkingDir(obj);
		} catch (JPSRuntimeException e) {
			assertEquals(e.getMessage(), "Path could not be constructed.");
		}

		obj = this;
		String substring = Paths.get("JPS_BASE_LIB/workingdir").toString();
		assertTrue(al.getPathToWorkingDir(obj).contains(substring));

	}

	public void testGetPathToJpsWorkingDir() throws NoSuchMethodException, SecurityException {
		AgentLocator al = AgentLocator.getSingleton();
		assertNotNull(al.getClass().getDeclaredMethod("getPathToJpsWorkingDir"));
		String substring = Paths.get("JPS_DATA/workingdir").toString();
		assertTrue(al.getPathToJpsWorkingDir().contains(substring));

	}

	public void testGetProperty() throws NoSuchMethodException, SecurityException {
		AgentLocator al = AgentLocator.getSingleton();
		assertNotNull(al.getClass().getDeclaredMethod("getProperty", String.class));
		String key = null;

		try {
			al.getProperty(key);
		} catch (JPSRuntimeException e) {
			assertEquals(e.getMessage(), "Null keys and values are not allowed.");
		}

		key = "new key";
		assertEquals(al.getProperty(key), "No such key in properties file.");

		key = "reldir.python";
		assertEquals(al.getProperty(key), "python");

		key = "reldir.workingdir";
		assertEquals(al.getProperty(key), "workingdir");

		key = "absdir.jpsdata.workingdir";
		assertTrue(al.getProperty(key).contains("JPS_DATA/workingdir"));

	}

	public void testGetUrl() throws NoSuchMethodException, SecurityException, IllegalAccessException,
			IllegalArgumentException, InvocationTargetException {
		AgentLocator al = AgentLocator.getSingleton();
		assertNotNull(al.getClass().getDeclaredMethod("getUrl", String.class, String.class));
		Method getUrl = al.getClass().getDeclaredMethod("getUrl", String.class, String.class);
		getUrl.setAccessible(true);
		String host = null;
		String port = null;

		try {
			getUrl.invoke(al, host, port);
		} catch (InvocationTargetException e) {
			assertEquals(e.getTargetException().getMessage(), "URL could not be constructed.");
		}

		try {
			host = "www.theworldavatar.com";
			port = null;
			getUrl.invoke(al, host, port);
		} catch (InvocationTargetException e) {
			assertEquals(e.getTargetException().getMessage(), "URL could not be constructed.");
		}

		host = null;
		port = "80";
		assertEquals(getUrl.invoke(al, host, port), "http:");

		host = "www.theworldavatar.com";
		port = "80";
		assertEquals(getUrl.invoke(al, host, port), "http://www.theworldavatar.com:80");
		
		host = "localhost";
		port = "8080";
		assertEquals(getUrl.invoke(al, host, port), "http://localhost:8080");
		
	}

}
