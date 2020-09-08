package uk.ac.cam.cares.jps.base.config;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.util.Map;
import java.util.Properties;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.log.JPSBaseLogger;

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
	
	public String get(String key) {
		return map.get(key);
	}
	
	public String put(String key, String value) {
		return map.put(key, value);
	}
	
	/**
	 * Loads the property files. There is only one source for the property files, namely the config directory
	 * within the deployed JPS_BASE app.
	 */
	private void init() {
		
		
		boolean runningForTest = AgentLocator.isJPSRunningForTest();
		JPSBaseLogger.info(this, "Tomcat is running for test = " + runningForTest);
		try {
			loadProperties("/jps.properties");
		}catch (FileNotFoundException exc) {
			JPSBaseLogger.error(this, exc);
			throw new JPSRuntimeException(exc.getMessage(), exc);
		}
		catch (IOException exc) {
			JPSBaseLogger.error(this, exc);
			throw new JPSRuntimeException(exc.getMessage(), exc);
		}
		
		if (runningForTest)  {
			try {
				// if started on local server then overwrite values from jps.properties
				loadProperties("/jpstest.properties");
			}catch (FileNotFoundException exc) {
				JPSBaseLogger.error(this, exc);
				throw new JPSRuntimeException(exc.getMessage(), exc);
			} catch (IOException exc) {
				JPSBaseLogger.info(this, "jpstest.properties was not found");
			}
		}
	}
	
	private void loadProperties(String propertyFile) throws IOException {
	    
		JPSBaseLogger.info(this, "loading key-value pairs from " + propertyFile);
		
		InputStream fin=null;
		Properties props = new Properties();
		fin=KeyValueMap.class.getResourceAsStream(propertyFile); //this is a static function
		props = new Properties();
		props.load(fin); 	
		
		Set<String> keys = props.stringPropertyNames();
		for (String key : keys) {
			String value = props.getProperty(key);
			put(key, value);
			JPSBaseLogger.info(this, key + " = " + value);
		}
	}
	public static String getProperty(String propertyFile, String getKey) {
		InputStream fin=null;
		Properties props = new Properties();
		fin=KeyValueMap.class.getResourceAsStream(propertyFile); //this is a static function
		props = new Properties();
		try {
		props.load(fin);
		fin.close();}
		catch (IOException exc) {
			JPSBaseLogger.info(KeyValueMap.class,  " properties was not loaded ");
		}
		String value = props.getProperty(getKey);
		return value;
	}
}
