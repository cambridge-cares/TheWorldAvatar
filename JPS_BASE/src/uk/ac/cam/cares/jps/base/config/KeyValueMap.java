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
		
		String path = AgentLocator.getCurrentJpsAppDirectory(this);
		boolean isJpsBaseDir = path.endsWith("/JPS_BASE") || path.endsWith("\\JPS_BASE");
		if (! isJpsBaseDir) {
			String message = "The current path is not withing JPS_BASE directory, path=" + path;
			LogServer.error(this, message);
			throw new JPSRuntimeException(message);
		}
				
		try {
			loadProperties(path);
		} catch (IOException exc) {
			LogServer.error(this, exc);
			throw new JPSRuntimeException(exc.getMessage(), exc);
		}
	}
	
	private void loadProperties(String jpsBaseDirectory) throws IOException {
	    
		String configPath = jpsBaseDirectory + "/conf/jps.properties";
		LogServer.info(this, "loading key-value pairs from " + configPath);
		
		FileInputStream inputStream = new FileInputStream(configPath);
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
