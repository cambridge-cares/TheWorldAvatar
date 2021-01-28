package uk.ac.cam.cares.jps.base.config;

import java.io.InputStream;
import java.util.Map;
import java.util.Properties;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.log.JPSBaseLogger;

public class KeyValueMap {

	private static KeyValueMap instance = null;
	public static Map<String, String> map = new ConcurrentHashMap<String, String>();
	private static String propertiesFile = "/jps.properties";
	private static String testPropertiesFile = "/jpstest.properties";
	private static String FILE_DOES_NOT_EXIST = "Properties file does not exist.";
	private static String KEY_DOES_NOT_EXIST = "No such key in properties file.";
	private static String NO_NULL_KEY_VALUE = "Null keys and values are not allowed.";

	public static synchronized KeyValueMap getInstance() {
		if (instance == null) {
			instance = new KeyValueMap();
		}
		return instance;
	}

	private KeyValueMap() {
		init(runningForTest(propertiesFile));
	}

	/**
	 * Takes in a key and returns value if the key exists in map. Throws appropriate
	 * error message otherwise. - Russell
	 */
	public String get(String key) {
		String value = "";

		try {
			value = map.get(key);
		} catch (Exception e) {
			throw new JPSRuntimeException(NO_NULL_KEY_VALUE);
		}

		if (value == null) {
			value = KEY_DOES_NOT_EXIST;
		}

		return value;
	}
	
	/**
	 * Puts in a key-value pair into map. Returns previous value associated with key. 
	 * Returns null if there was no previous association with key.
	 * Throws appropriate error message otherwise. - Russell
	 */
	public String put(String key, String value) {	
		String oldValue = "";
		
		try {
			oldValue = map.put(key, value);
		} catch (Exception e) {
			throw new JPSRuntimeException(NO_NULL_KEY_VALUE);
		}
		
		return oldValue;
	}

	/**
	 * Loads either jps.properties or jpstest.properties in src/main/resources. -
	 * Russell
	 */
	private void init(Boolean runningForTest) {
		if (runningForTest) {
			loadProperties(testPropertiesFile);
		} else {
			loadProperties(propertiesFile);
		}

	}

	/**
	 * Checks properties file for test key and returns true or false. - Russell
	 */
	private Boolean runningForTest(String propertiesFile) {
		Boolean runningForTest = false;

		try {
			loadProperties(propertiesFile);
		} catch (Exception e) {
			throw new JPSRuntimeException(FILE_DOES_NOT_EXIST);
		}

		try {
			runningForTest = Boolean.valueOf(map.get("test"));
		} catch (Exception e) {
			throw new JPSRuntimeException(KEY_DOES_NOT_EXIST);
		}

		return runningForTest;
	}

	/**
	 * Reads properties file and loads key-value pairs into map. -Russell
	 */
	private void loadProperties(String propertiesFile) {

		JPSBaseLogger.info(this, "loading key-value pairs from " + propertiesFile);

		InputStream fin = null;
		Properties props = new Properties();

		try {
			fin = KeyValueMap.class.getResourceAsStream(propertiesFile); // this is a static function
			props.load(fin);
		} catch (Exception e) {
			throw new JPSRuntimeException(FILE_DOES_NOT_EXIST);
		} finally {
			Set<String> keys = props.stringPropertyNames();
			for (String key : keys) {
				String value = props.getProperty(key);
				put(key, value);
				JPSBaseLogger.info(this, key + " = " + value);
			}
		}
	}

	/**
	 * Reads properties file and gets value of requested key. - Russell
	 */
	public static String getProperty(String propertiesFile, String key) {
		InputStream fin = null;
		Properties props = new Properties();
		String value = "";

		try {
			fin = KeyValueMap.class.getResourceAsStream(propertiesFile); // this is a static function
			props.load(fin);
		} catch (Exception e) {
			JPSBaseLogger.info(KeyValueMap.class, " properties was not loaded ");
			throw new JPSRuntimeException(FILE_DOES_NOT_EXIST);
		}

		try {
			value = props.getProperty(key);
		} catch (Exception e) {
			throw new JPSRuntimeException(NO_NULL_KEY_VALUE);
		}

		if (value == null) {
			value = KEY_DOES_NOT_EXIST;
		}

		return value;
	}
}
