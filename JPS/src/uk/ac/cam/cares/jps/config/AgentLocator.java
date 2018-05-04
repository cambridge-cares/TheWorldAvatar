package uk.ac.cam.cares.jps.config;

import java.io.FileInputStream;
import java.io.IOException;
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

public class AgentLocator {

	private static AgentLocator instance = null;
	
	private Logger logger = LoggerFactory.getLogger(AgentLocator.class);
	private String jpsRootDirectory = null;
	private Properties jpsProps = null;
	private Properties jpsTestProps = null;
	private String url = null;

	private AgentLocator() {
	}

	public static synchronized AgentLocator getSingleton() {
		if (instance == null) {
			instance = new AgentLocator();
			instance.init();
		}
		return instance;
	}

	private void init() {
		
		String path = Thread.currentThread().getContextClassLoader().getResource("").getPath();
		if ((path.indexOf("/") == 0) || (path.indexOf("\\") == 0)) {
			path = path.substring(1);
		}
		int index = path.lastIndexOf("/JPS");
		if (index == -1) {
			index = path.lastIndexOf("\\JPS");
		}
		if (index == -1) {
			String message = "The root directory for JPS was not found, path = " + path;
			logger.error(message);
			throw new RuntimeException(message);
		}
		
		jpsRootDirectory = path.substring(0, index + 4);
		logger.info("jpsRootDirectory = " + jpsRootDirectory);
		
		try {
			jpsProps = loadProperties("jps.properties");
			logProperties(jpsProps);
		} catch (IOException exc) {
			logger.error(exc.getMessage(), exc);
		}
		
		try {
			jpsTestProps = loadProperties("jpstest.properties");
			logProperties(jpsTestProps);
		} catch (IOException exc) {
			// this is no error. jpstest.properties should not be available on production system.
			logger.info("jpstest.properties not found");
		}			

		url = getProperty("host") + ":" + getProperty("port");
		logger.info("created url from properties: " + url);
	}
	
	private Properties loadProperties(String fileName) throws IOException {
	      
		String configPath = getJPSRootDirectory() + "/conf/" + fileName;
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
	
	private static String getJPSRootDirectory() {
		return getSingleton().jpsRootDirectory;
	}
	
	public static String getAbsolutePath(String keyForRelativePath) {
		String relativePath = getProperty(keyForRelativePath);
		return getJPSRootDirectory() + "/" + relativePath;
	}
	
	/**
	 * @param pythonScriptName (including package name followed by script name and .py extension, e.g. caresjpsadmsinputs/ADMSGeoJsonGetter.py)
	 * @return
	 */
	public static String getPathToPythonScript(String pythonScriptName) {
		String relativePath = getProperty("reldir.python");
		return getJPSRootDirectory() + "/" + relativePath + "/" + pythonScriptName;
	}
	
	public static String getPathToWorkingDir() {
		String relativePath = getProperty("reldir.workingdir");
		return getJPSRootDirectory() + "/" + relativePath;
	}
	
	public static String getPathToJpsDataKnowledgeBaseDir() {
		return getProperty("absdir.jpsdata.knowledgebase");
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
		logger.info("calling agent " + url);
		HttpUriRequest request = new HttpGet(combinedUrl);
		HttpResponse httpResponse = HttpClientBuilder.create().build().execute(request);
		String response = EntityUtils.toString(httpResponse.getEntity());
		return response;
	}
}
