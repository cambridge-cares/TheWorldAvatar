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
import uk.ac.cam.cares.jps.base.util.MiscUtil;

public class KnowledgeBaseManager {
	
	private static Logger logger = LoggerFactory.getLogger(KnowledgeBaseManager.class);
	
	private static KnowledgeBaseManager instance = null;
	private static final String JPS_WORKING_DIR = AgentLocator.getPathToJpsWorkingDir();
	private Map<String, KnowledgeBaseAbstract> kbmap = new HashMap<String, KnowledgeBaseAbstract>();
	
	private KnowledgeBaseManager() {
	}
	
	public static synchronized KnowledgeBaseManager getInstance() {
		if (instance == null) {
			instance = new KnowledgeBaseManager();
		}
		return instance;
	}

	public static synchronized KnowledgeBaseAbstract getKnowledgeBase(String datasetUrl) {
		return getInstance().getKnowledgeBaseInternal(datasetUrl);
	}
	
	public synchronized void clear() {
		kbmap = new HashMap<String, KnowledgeBaseAbstract>();
	}
	
	private synchronized KnowledgeBaseAbstract getKnowledgeBaseInternal(String datasetUrl) {
		
		KnowledgeBaseAbstract result = kbmap.get(datasetUrl);
		if (result != null) {
			return result;
		}
		
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
		
		result = instantiateKnowledgeBase(knowledgeBaseClass, datasetUrl, datasetName, endpointUrl);
		
//		if (datasetUrl.contains("/scenario/")) {
//
//			
//			result = new KnowledgeBaseFileBased(datasetUrl);
//		} else if (datasetUrl.endsWith("/meta")) {
//				//String endpointUrl = KeyValueManager.get(IKeys.URL_RDF_METADATA);
//				datasetName = "myjpsmetadatasetinmemory";
//				result = new KnowledgeBaseRdf4jServer(datasetUrl, datasetName);
//				//result = new KnowledgeBaseFuseki(datasetUrl, endpointUrl);
////		} else if (datasetUrl.contains("/testnative")) {
////			result =  KnowledgeBaseRdf4jLocalInMemory.getInstance();
////		} else if (datasetUrl.contains("/perfinmemory")) {
////			result = new KnowledgeBaseRdf4jServer(datasetName);
//		} else if (datasetUrl.contains("/testrdf4jinmemory")) {
//			result = new KnowledgeBaseRdf4jServer(datasetUrl, datasetName);
//		} else if (datasetUrl.contains("/testrdf4jnative")) {
//			result = new KnowledgeBaseRdf4jServer(datasetUrl, datasetName);
//		} else if (datasetUrl.contains("/testrdf4jmeta")) {
//			result = new KnowledgeBaseRdf4jServer(datasetUrl, datasetName);
//		} else if (datasetUrl.contains("/testfuseki")) {
//			String endpointUrl = "http://localhost:3030/myscenariotestfuseki";
//			result = new KnowledgeBaseFuseki(datasetUrl, endpointUrl);
//		} else if (datasetUrl.contains("/testblazegraph")) {
//			result = new KnowledgeBaseBlazegraph(datasetUrl, datasetName);
//		} else {
//			String[] preexistingDatasets = new String[] {"misc", "test", "testfilebased"};
//			for (String current: preexistingDatasets) {
//				if (datasetUrl.endsWith("/" + current)) {
//					result = new KnowledgeBaseFileBased(datasetUrl);
//					break;
//				}
//			} 
//		}
		
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
	
	private static KnowledgeBaseAbstract instantiateKnowledgeBase(String knowledgeBaseClass, String datasetUrl, String datasetName, String endpointUrl) {
		try {
			Class<?> cl = Class.forName(knowledgeBaseClass);
			Constructor<?> constr = cl.getConstructor(String.class, String.class, String.class);
			return (KnowledgeBaseAbstract) constr.newInstance(datasetUrl, datasetName, endpointUrl);
		} catch (ClassNotFoundException | InstantiationException | IllegalAccessException | IllegalArgumentException | InvocationTargetException | NoSuchMethodException | SecurityException e) {
			throw new JPSRuntimeException("Unable to create a knowledge base base for class=" + 
					knowledgeBaseClass + ", datasetUrl=" + datasetUrl + ", datasetName=" + datasetName + ", endpointUrl=" + endpointUrl);
		}
	}
}
