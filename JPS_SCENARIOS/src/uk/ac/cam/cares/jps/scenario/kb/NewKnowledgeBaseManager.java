package uk.ac.cam.cares.jps.scenario.kb;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.util.HashMap;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.config.IKeys;
import uk.ac.cam.cares.jps.base.config.JPSConstants;
import uk.ac.cam.cares.jps.base.config.KeyValueManager;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.interfaces.KnowledgeBaseClientInterface;
import uk.ac.cam.cares.jps.base.util.MiscUtil;

public class NewKnowledgeBaseManager {
	
	private static Logger logger = LoggerFactory.getLogger(NewKnowledgeBaseManager.class);
	
	private static NewKnowledgeBaseManager instance = null;
	private static final String JPS_WORKING_DIR = AgentLocator.getPathToJpsWorkingDir();
	private Map<String, KnowledgeBaseClientInterface> kbmap = new HashMap<String, KnowledgeBaseClientInterface>();
	
	private NewKnowledgeBaseManager() {
	}
	
	public static synchronized NewKnowledgeBaseManager getInstance() {
		if (instance == null) {
			instance = new NewKnowledgeBaseManager();
		}
		return instance;
	}

	public static synchronized KnowledgeBaseClientInterface getKnowledgeBase(String datasetUrl) {
		return getInstance().getKnowledgeBaseInternal(datasetUrl);
	}
	
	public synchronized void clear() {
		kbmap = new HashMap<String, KnowledgeBaseClientInterface>();
	}
	
	private synchronized KnowledgeBaseClientInterface getKnowledgeBaseInternal(String datasetUrl) {
		
		KnowledgeBaseClientInterface result = kbmap.get(datasetUrl);
		if (result != null) {
			return result;
		}
		
		//TODO: KGRouter
		
		String datasetName = getDatasetName(datasetUrl);
		String datasetNameForConfiguration = datasetName;
		if (datasetUrl.contains("/scenario/")) {
			datasetNameForConfiguration = "scenario";
		}	
		
		String key = MiscUtil.format(IKeys.DATASET_TEMPLATE_URL, datasetNameForConfiguration);
		String datasetUrlConfigured = KeyValueManager.get(key);
		key = MiscUtil.format(IKeys.DATASET_TEMPLATE_KBCLASS, datasetNameForConfiguration);
		String knowledgeBaseClass = KeyValueManager.get(key);
		key = MiscUtil.format(IKeys.DATASET_TEMPLATE_ENDPOINT_URL, datasetNameForConfiguration);		
		String endpointUrl = KeyValueManager.get(key);
		
		logger.info("instantiating knowledge base for class=" + knowledgeBaseClass +
				", datasetUrl=" + datasetUrl + ", datasetName=" + datasetName + ", endpointUrl=" + endpointUrl);
		
		// TODO remove, this will be performed by KGRouter
		result = instantiateKnowledgeBase(knowledgeBaseClass, datasetUrl, datasetName, endpointUrl);
		
		if (result != null) {
			kbmap.put(datasetUrl, result);
			return result;
		}
		
		throw new JPSRuntimeException("unknown datasetUrl = " + datasetUrl);
	}
	
	/**
	 * @param requestUrl is of the form http:// host:port/<jps>/data/<datasetname>/...
	 * @return
	 */
	public static String getDatasetUrl(String requestUrl) {
		//TODO: here
		String jps = "/" + JPSConstants.KNOWLEDGE_BASE_JPS + "/";
		int i = requestUrl.indexOf(jps) + jps.length();
		String rest = requestUrl.substring(i);
		if (rest.startsWith("data/")) {
			i += "data/".length();
		}  else if (rest.startsWith("dataset/")) {
			i += "dataset/".length();
		} else if (rest.startsWith("kb/")) {
			i += "kb/".length();
		} else if (rest.startsWith("scenario/")) {
			i += "scenario/".length();
		}
		String datasetUrl = requestUrl.substring(0, i);
		rest = requestUrl.substring(i);
		i = rest.indexOf("/");
		if (i >= 0) {
			datasetUrl += rest.substring(0, i);
		} else {
			datasetUrl = requestUrl;
		}
		return datasetUrl;
	}
	
	public static String getDatasetName(String datasetUrl) {
		int i = datasetUrl.lastIndexOf("/");
		return datasetUrl.substring(i+1);
	}
	
	public static String getDataSetWorkingDir() {
		return JPS_WORKING_DIR + "/JPS_SCENARIO/dataset";
	}
	
	//TODO: remove
	private static KnowledgeBaseClientInterface instantiateKnowledgeBase(String knowledgeBaseClass, String datasetUrl, String datasetName, String endpointUrl) {
		try {
			Class<?> cl = Class.forName(knowledgeBaseClass);
			Constructor<?> constr = cl.getConstructor(String.class, String.class, String.class);
			return (KnowledgeBaseClientInterface) constr.newInstance(datasetUrl, datasetName, endpointUrl);
		} catch (ClassNotFoundException | InstantiationException | IllegalAccessException | IllegalArgumentException | InvocationTargetException | NoSuchMethodException | SecurityException e) {
			throw new JPSRuntimeException("Unable to create a knowledge base base for class=" + 
					knowledgeBaseClass + ", datasetUrl=" + datasetUrl + ", datasetName=" + datasetName + ", endpointUrl=" + endpointUrl);
		}
	}
}
