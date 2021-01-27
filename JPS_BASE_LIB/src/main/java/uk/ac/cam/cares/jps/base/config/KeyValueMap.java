package uk.ac.cam.cares.jps.base.config;

import java.io.IOException;
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
	private static String FILE_DOES_NOT_EXIST = "Property file does not exist.";
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
	 * Takes in a key and returns value if the key exists in map. Returns
	 * appropriate error message otherwise. - Russell
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
	 * Puts in a key-value pair in map. Returns appropriate error message otherwise.
	 * - Russell
	 */

	public void put(String key, String value) {
		try {
			map.put(key, value);
		} catch (Exception e) {
			throw new JPSRuntimeException(NO_NULL_KEY_VALUE);
		}

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
			;
		} catch (Exception e) {
			throw new JPSRuntimeException(KEY_DOES_NOT_EXIST);
		}

		return runningForTest;
	}

	/**
	 * load all key value pairs
	 * 
	 * @param propertyFile
	 * @throws IOException
	 */

	private void loadProperties(String propertyFile) {

		JPSBaseLogger.info(this, "loading key-value pairs from " + propertyFile);

		InputStream fin = null;
		Properties props = new Properties();

		try {
			fin = KeyValueMap.class.getResourceAsStream(propertyFile); // this is a static function
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
	 * static method of accessing a property from a given properties file in JPS
	 * BASE LIB
	 * 
	 * @param propertyFile
	 * @param getKey
	 * @return
	 */
	public static String getProperty(String propertyFile, String getKey) {
		InputStream fin = null;
		Properties props = new Properties();
		String value = "";

		try {
			fin = KeyValueMap.class.getResourceAsStream(propertyFile); // this is a static function
			props.load(fin);
		} catch (Exception e) {
			JPSBaseLogger.info(KeyValueMap.class, " properties was not loaded ");
			throw new JPSRuntimeException(FILE_DOES_NOT_EXIST);
		}

		try {
			value = props.getProperty(getKey);
		} catch (Exception e) {
			throw new JPSRuntimeException(NO_NULL_KEY_VALUE);
		}

		if (value == null) {
			value = KEY_DOES_NOT_EXIST;
		}

		return value;
	}
}
