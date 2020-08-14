package uk.ac.cam.cares.jps.base.config;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
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
		
		String path = AgentLocator.getJPSBaseDirectory();
		String libpath = path.replace("JPS_BASE","JPS_BASE_LIB" );
		System.out.println("LibPath " + libpath);
		boolean runningForTest = AgentLocator.isJPSRunningForTest();
		JPSBaseLogger.info(this, "Tomcat is running for test = " + runningForTest);
		try {
			loadProperties(path + "/conf/jps.properties");
			loadProperties(libpath + "/src/main/resources/config.properties");
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
				loadProperties(path + "/conf/jpstest.properties");
			} catch (IOException exc) {
				JPSBaseLogger.info(this, "jpstest.properties was not found");
			}
		}
	}
	
	private void loadProperties(String propertyFile) throws IOException {
	    //check if the file exists on the file system in string
		File tmpDir = new File(propertyFile);
		boolean fileExists = tmpDir.exists();
		if (!fileExists) {
			throw new FileNotFoundException(propertyFile + " is not found. ");
		}
		JPSBaseLogger.info(this, "loading key-value pairs from " + propertyFile);
		
		FileInputStream inputStream = new FileInputStream(propertyFile);
		Properties props = new Properties();
		props.load(inputStream);	
		
		Set<String> keys = props.stringPropertyNames();
		for (String key : keys) {
			String value = props.getProperty(key);
			put(key, value);
			JPSBaseLogger.info(this, key + " = " + value);
		}
	}
}
