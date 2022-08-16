package uk.ac.cam.cares.jps.base.config;

import java.net.URL;
import java.nio.file.Paths;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

public class AgentLocator {

     /**
     * Logger for error output.
     */
    private static final Logger LOGGER = LogManager.getLogger(AgentLocator.class);
    
	private static AgentLocator instance = null;
	private String jpsBaseDirectory = null;
	private String url = null;
	private static String JPS_CONFIG_ENVIRONMENT_TEST_KEY = "test";
	private static String REL_DIR_PYTHON = "reldir.python";
	private static String REL_DIR_WORKINGDIR = "reldir.workingdir";
	private static String ABS_JPSDATA_WORKINGDIR = "absdir.jpsdata.workingdir";
	private static String INVALID_PATH = "Path could not be constructed.";
	private static String INVALID_URL = "URL could not be constructed.";
	private static final String[] SUBDIRECTORIES = new String[] {"WEB-INF", "bin", "build", "target"};

	public static synchronized AgentLocator getSingleton() {
		if (instance == null) {
			instance = new AgentLocator();
		}

		return instance;

	}

	private AgentLocator() {
		init();
	}

	/**
	 * Assigns values to jpsBaseDirectory and url. - Russell
	 */
	private void init() {
		jpsBaseDirectory = getCurrentJpsAppDirectory(this);
		LOGGER.info("JPS_BASE directory = " + jpsBaseDirectory);
		url = getUrl(getProperty("host"), getProperty("port"));
	}

	/**
	 * Obtains the working/tomcat directory of given class. e.g.
	 * getCurrentJpsAppDirectory(this) returns
	 * /Users/russ1337/Desktop/JPS/workarea/JParkSimulator-git/JPS_BASE_LIB -
	 * Russell
	 */
	public static String getCurrentJpsAppDirectory(Object thisObject) {
        String path;
        
        try {
			path = Paths.get(thisObject.getClass().getClassLoader().getResource("").toURI()).toString();
		} catch (Exception e) {
			throw new JPSRuntimeException(INVALID_PATH);
		}
        
        for (String subDir : SUBDIRECTORIES) {
        	while (path.contains(subDir)) {
        		path = Paths.get(path).getParent().toString();
        	}
        }
        
        return path;
        
	}

	/**
	 * Obtains the working directory of JPS_BASE_LIB - Russell
	 */
	public static String getJPSBaseDirectory() {
		return getSingleton().jpsBaseDirectory;

	}

	/**
	 * Checks properties file for test key and returns true or false. - Russell
	 */
	public static boolean isJPSRunningForTest() {
		Boolean testMode = false;

		testMode = Boolean.valueOf(getProperty(JPS_CONFIG_ENVIRONMENT_TEST_KEY));

		return testMode;

	}

	/**
	 * Creates path to python scripts found in the python folder in working
	 * directory of given object. e.g. getNewPathToPythonScript("python_script.py",
	 * this) returns
	 * /Users/russ1337/Desktop/JPS/workarea/JParkSimulator-git/JPS_BASE_LIB/python/python_script.py
	 * - Russell
	 */
	public static String getNewPathToPythonScript(String pythonScriptName, Object thisObject) {
		String pathToPy;
		String relativePath = getProperty(REL_DIR_PYTHON);

		try {
			pathToPy = Paths.get(getCurrentJpsAppDirectory(thisObject), relativePath, pythonScriptName).toString();
		} catch (Exception e) {
			throw new JPSRuntimeException(INVALID_PATH);
		}

		return pathToPy;

	}

	/**
	 * Creates path to workingdir folder in working directory of given object. e.g.
	 * getPathToWorkingDir(this) returns
	 * /Users/russ1337/Desktop/JPS/workarea/JParkSimulator-git/JPS_BASE_LIB/python/python_script.py
	 * - Russell
	 */
	public static String getPathToWorkingDir(Object thisObject) {
		String pathToPyDir;
		String relativePath = getProperty(REL_DIR_WORKINGDIR);

		try {
			pathToPyDir = Paths.get(getCurrentJpsAppDirectory(thisObject), relativePath).toString();
		} catch (Exception e) {
			throw new JPSRuntimeException(INVALID_PATH);
		}

		return pathToPyDir;

	}

	/**
	 * Obtains path to JPS data working directory e.g. C:/JPS_DATA/workingdir -
	 * Russell
	 */
	public static String getPathToJpsWorkingDir() {
		String absPath = getProperty(ABS_JPSDATA_WORKINGDIR);
		String pathToJpsDir = Paths.get(absPath).toString();

		return pathToJpsDir;

	}

	/**
	 * Takes in a key and returns value if the key exists in map. Throws appropriate
	 * error message otherwise. - Russell
	 */
	public static String getProperty(String key) {
		String property = KeyValueMap.getInstance().get(key);

		return property;

	}

	/**
	 * Contructs url using host and port values from either jps.properties or
	 * jpstest.properties. The later argument(s) will only be appended accordingly
	 * if the previous argument(s) is not null. e.g. getUrl(null, "123") returns
	 * "http:" - Russell
	 */
	private String getUrl(String host, String port) {
		String protocol = "http";
		String path = "";
		String url;

		try {
			url = new URL(protocol, host, Integer.valueOf(port), path).toString();
			LOGGER.info("created url from properties: " + url);
		} catch (Exception e) {
			throw new JPSRuntimeException(INVALID_URL);
		}

		return url;
	}

}
