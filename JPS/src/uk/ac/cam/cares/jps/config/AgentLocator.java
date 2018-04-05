package uk.ac.cam.cares.jps.config;

import java.io.FileInputStream;
import java.io.IOException;
import java.util.Properties;

import org.apache.http.HttpResponse;
import org.apache.http.client.ClientProtocolException;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.methods.HttpUriRequest;
import org.apache.http.impl.client.HttpClientBuilder;
import org.apache.http.util.EntityUtils;

public class AgentLocator {

	private static AgentLocator instance = null;
	
	private String jpsRootDirectory = null;
	private Properties jpsProps = null;
	private Properties jpsTestProps = null;
	private String url = null;

	private AgentLocator() {
	}

	// TODO-AE check Singleton Pattern and multi threading --> synchronize
	public static AgentLocator getSingleton() {
		if (instance == null) {
			instance = new AgentLocator();
			instance.init();
		}
		return instance;
	}

	private void init() {
		
		String path = Thread.currentThread().getContextClassLoader().getResource("").getPath();
		int index = path.lastIndexOf("/JPS");
		if (index == -1) {
			index = path.lastIndexOf("\\JPS");
		}
		if (index == -1) {
			throw new RuntimeException("The root directory for JSP was not found, path = " + path);
		}
		
		jpsRootDirectory = path.substring(0, index + 4);
		// TODO-AE log
		System.out.println("jspRootDirectory = " + jpsRootDirectory);
		
//		String current;
//		try {
//			current = new File( "." ).getCanonicalPath();
//			System.out.println("Current dir:"+current);
//	        String currentDir = System.getProperty("user.dir");
//	        System.out.println("Current dir using System:" +currentDir);
//		} catch (IOException e1) {
//			// TODO Auto-generated catch block
//			e1.printStackTrace();
//		}
		
		
		jpsProps = loadProperties("jps.properties");
		if (jpsProps == null) {
			// TODO-AE Fatal error, stop application
			throw new RuntimeException("jps.properties not found");
		}

		// TODO-AE log all properties of jps and jpstest to log file

		jpsTestProps = loadProperties("jpstest.properties");
		if (jpsTestProps == null) {
			// TODO-AE its no error, just log it
		}

		url = getProperty("host") + ":" + getProperty("port");
	}
	
	private Properties loadProperties(String fileName) {
	      
		//TODO-AE putting the properties file to java package is not that nice
		String configPath = getJPSRootDirectory() + "/conf/" + fileName;
		System.out.println("Loading " + configPath);
		
		try {
			FileInputStream inputStream = new FileInputStream(configPath);
			Properties props = new Properties();
			//props.load(getClass().getClassLoader().getResourceAsStream(configPath));
			props.load(inputStream);
			return props;
		} catch (IOException e1) {
			// TODO-AE log
			e1.printStackTrace();
		}		

//		try {
//			Properties props = new Properties();
//			props.load(new FileInputStream(configPath));
//			return props;
//		} catch (IOException e) {
//			// TODO-AE log
//		}

		return null;
	}
	
	private static String getJPSRootDirectory() {
		return getSingleton().jpsRootDirectory;
	}
	
	public static String getAbsolutePath(String keyForRelativePath) {
		String relativePath = getProperty(keyForRelativePath);
		return getJPSRootDirectory() + "/" + relativePath;
	}
	
	/**
	 * @param pythonScriptName (including the .py extension)
	 * @return
	 */
	public static String getPathToPythonScript(String pythonScriptName) {
		String relativePath = getProperty("reldir.python");
		return getJPSRootDirectory() + "/" + relativePath + "/" + pythonScriptName;
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
		String url = getSingleton().url + AgentLocator.getProperty(agentKey);
		// TODO-AE logging
		System.out.println("calling agent " + url);
		HttpUriRequest request = new HttpGet(url);
		HttpResponse httpResponse = HttpClientBuilder.create().build().execute(request);
		String response = EntityUtils.toString(httpResponse.getEntity());

		return response;
	}

}
