package uk.ac.cam.cares.jps.base.config;

import java.io.FileInputStream;
import java.io.IOException;
import java.util.Map;
import java.util.Properties;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.log.LogServer;

public class KeyValueMap {

	private static KeyValueMap instance = null;
	private static Map<String, String> map = new ConcurrentHashMap<String, String>();
	
	public static synchronized KeyValueMap getInstance() {
		if (instance == null) {
			instance = new KeyValueMap();
		}
		return instance;
	}
	
	private KeyValueMap() {
		init();
	}
	
	String get(String key) {
		return map.get(key);
	}
	
	String put(String key, String value) {
		return map.put(key, value);
	}
	
	/**
	 * Loads the property files. There is only one source for the property files, namely the config directory
	 * within the deployed JPS_BASE app.
	 */
	private void init() {
		
//		String path = AgentLocator.getCurrentJpsAppDirectory(this);
//		boolean isJpsBaseDir = path.endsWith("/JPS_BASE") || path.endsWith("\\JPS_BASE");
//		if (! isJpsBaseDir) {
//			String message = "The current path is not within JPS_BASE directory, path=" + path;
//			LogServer.error(this, message);
//			throw new JPSRuntimeException(message);
//		}
		
		String path = AgentLocator.getJPSBaseDirectory();
		
		try {
			loadProperties(path + "/conf/jps.properties");
			if (! (path.startsWith("C:/TOMCAT/webapps/JPS_BASE") || path.startsWith("C:\\TOMCAT\\webapps\\JPS_BASE")))  {
				// if started on local server then overwrite values from jps.properties
				loadProperties(path + "/conf/jpstest.properties");
			}
		} catch (IOException exc) {
			LogServer.error(this, exc);
			throw new JPSRuntimeException(exc.getMessage(), exc);
		}
	}
	
	private void loadProperties(String propertyFile) throws IOException {
	    
		LogServer.info(this, "loading key-value pairs from " + propertyFile);
		
		FileInputStream inputStream = new FileInputStream(propertyFile);
		Properties props = new Properties();
		props.load(inputStream);	
		
		Set<String> keys = props.stringPropertyNames();
		for (String key : keys) {
			String value = props.getProperty(key);
			put(key, value);
			LogServer.info(this, key + " = " + value);
		}
	}
}
