package uk.ac.cam.cares.jps.base.config;

import java.io.FileInputStream;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.net.URLDecoder;
import java.util.Map.Entry;
import java.util.Properties;

import org.apache.http.HttpResponse;
import org.apache.http.client.ClientProtocolException;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.methods.HttpUriRequest;
import org.apache.http.impl.client.HttpClientBuilder;
import org.apache.http.util.EntityUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

public class AgentLocator {

	private static AgentLocator instance = null;
	
	private static Logger logger = LoggerFactory.getLogger(AgentLocator.class);
	private String jpsBaseDirectory = null;
	private Properties jpsProps = null;
	private Properties jpsTestProps = null;
	private String url = null;

	private AgentLocator() {
	}

	private static synchronized AgentLocator getSingleton() {
		if (instance == null) {
			instance = new AgentLocator();
			instance.init();
		}
		return instance;
	}

	/**
	 * Loads the property files. There is only one source for the property files, namely the config directory
	 * within the deployed JPS_BASE app.
	 */
	private void init() {
		
		String path = getCurrentJpsAppDirectory(this);
		boolean isJpsBaseDir = path.endsWith("/JPS_BASE") || path.endsWith("\\JPS_BASE");

		if (!isJpsBaseDir) {
			int index = path.lastIndexOf("/JPS");
			if (index == -1) {
				index = path.lastIndexOf("\\JPS");
				path = path.substring(0, index) + "\\JPS_BASE";
			} else {
				path = path.substring(0, index) + "/JPS_BASE";
			}
		}
		
		jpsBaseDirectory = path;
		logger.info("JPS_BASE directory = " + jpsBaseDirectory);
		
		try {
			jpsProps = loadProperties("jps.properties");
			if (isJpsBaseDir) {
				logProperties(jpsProps);
			}
			// else no need to log again
		} catch (IOException exc) {
			logger.error(exc.getMessage(), exc);
		}
		
		try {
			jpsTestProps = loadProperties("jpstest.properties");
			if (isJpsBaseDir) {
				logProperties(jpsTestProps);
			}
			// else no need to log again
		} catch (IOException exc) {
			// this is no error. jpstest.properties should not be available on production system.
			logger.info("jpstest.properties not found");
		}			

		url = getProperty("host") + ":" + getProperty("port");
		logger.info("created url from properties: " + url);
	}
	
	private Properties loadProperties(String fileName) throws IOException {
	      
		String configPath = getJPSBaseDirectory() + "/conf/" + fileName;
		logger.info("loading " + configPath);
		
		FileInputStream inputStream = new FileInputStream(configPath);
		Properties props = new Properties();
		props.load(inputStream);
		
		return props;
	}
	
	private void logProperties(Properties properties) {
		for (Entry<Object, Object> current : properties.entrySet()) {
			logger.info(current.toString());
		}
	}

	public static String getCurrentJpsAppDirectory(Object thisObject) {
		
		String classDir = thisObject.getClass().getClassLoader().getResource("").getPath();
		String path;
		try {
			path = URLDecoder.decode(classDir, "UTF-8");
		} catch (UnsupportedEncodingException e) {
			throw new JPSRuntimeException(e);
		
		}
		if ((path.indexOf("/") == 0) || (path.indexOf("\\") == 0)) {
			path = path.substring(1);
		}
		int index = path.lastIndexOf("/WEB-INF/classes/");
		if (index == -1) {
			index = path.lastIndexOf("\\WEB-INF/classes/");
			if (index == -1) {
				index = path.lastIndexOf("/bin/");
				if (index == -1) {
					index = path.lastIndexOf("\\bin\\");
				}
			}
		}
		if (index == -1) {
			String message = "current JPS app directory was not found, class directory = " + classDir;
			logger.error(message);
			throw new JPSRuntimeException(message);
		}
		path = path.substring(0, index);
		logger.info("current JPS app directory = " + path + " , class directory = " + classDir);
		return path;
	}
	
	private static String getJPSBaseDirectory() {
		return getSingleton().jpsBaseDirectory;
	}
	
	public static String getAbsolutePath(String keyForRelativePath, Object thisObject) {
		String relativePath = getProperty(keyForRelativePath);
		return getCurrentJpsAppDirectory(thisObject) + "/" + relativePath;
	}
	
	/**
	 * @param pythonScriptName (including package name followed by script name and .py extension, e.g. caresjpsadmsinputs/ADMSGeoJsonGetter.py)
	 * @return
	 */
	//TODO-AE replace original methods after Janusz checked that this method is working
	public static String getNewPathToPythonScript(String pythonScriptName, Object thisObject) {
		String relativePath = getProperty("reldir.python");
		return getCurrentJpsAppDirectory(thisObject) + "/" + relativePath + "/" + pythonScriptName;
	}
	
	public static String getPathToWorkingDir(Object thisObject) {
		String relativePath = getProperty("reldir.workingdir");
		return getCurrentJpsAppDirectory(thisObject) + "/" + relativePath;
	}
	
	public static String getPathToJpsDataKnowledgeDir() {
		return getProperty("absdir.jpsdata.knowledgebase");
	}
	
	public static String getPathToJpsDataOntologyDir() {
		return getProperty("absdir.jpsdata.ontology");
	}
	
	public static String getPathToJpsWorkingDir() {
		return getProperty("absdir.jpsdata.workingdir");
	}
	
	/**
	 * If there is a test property file with the key then its value is returned.
	 * Otherwise the value specified in the application property file or null is
	 * returned.
	 * 
	 * @param key
	 * @return
	 */
	public static String getProperty(String key) {
		String result = null;

		Properties testProps = getSingleton().jpsTestProps;
		if (testProps != null) {
			result = (String) testProps.get(key);
		}
		if ((testProps == null) || (result == null)) {
			result = (String) getSingleton().jpsProps.getProperty(key);
		}

		return result;
	}

	public static String callAgent(String agentKey) throws ClientProtocolException, IOException {
		return getSingleton().callAgentInternally(agentKey);
	}
	
	private String callAgentInternally(String agentKey)  throws ClientProtocolException, IOException {
		String combinedUrl = url + AgentLocator.getProperty(agentKey);
		logger.info("calling agent " + combinedUrl);
		HttpUriRequest request = new HttpGet(combinedUrl);
		HttpResponse httpResponse = HttpClientBuilder.create().build().execute(request);
		String response = EntityUtils.toString(httpResponse.getEntity());
		return response;
	}
}
