package uk.ac.cam.cares.jps.scenario;

import java.util.HashMap;
import java.util.Map;

import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.config.JPSConstants;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

public class KnowledgeBaseManager {
	
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

	public static KnowledgeBaseAbstract getKnowledgeBase(String datasetUrl) {
		return getInstance().getKnowledgeBaseInternal(datasetUrl);
	}
	
	private KnowledgeBaseAbstract getKnowledgeBaseInternal(String datasetUrl) {
		
		KnowledgeBaseAbstract result = kbmap.get(datasetUrl);
		if (result != null) {
			return result;
		}
		
		String datasetName = getDatasetName(datasetUrl);
	
		if (datasetUrl.contains("/scenario/")) {		
			result = new KnowledgeBaseFileBased(datasetUrl);
//		} else if (datasetUrl.contains("/testnative")) {
//			result =  KnowledgeBaseRdf4jLocalInMemory.getInstance();
//		} else if (datasetUrl.contains("/perfinmemory")) {
//			result = new KnowledgeBaseRdf4jServer(datasetName);
		} else if (datasetUrl.contains("/testrdf4jinmemory")) {
			result = new KnowledgeBaseRdf4jServer(datasetUrl, datasetName);
		} else if (datasetUrl.contains("/testrdf4jnative")) {
			result = new KnowledgeBaseRdf4jServer(datasetUrl, datasetName);
		} else if (datasetUrl.contains("/meta")) {
			result = new KnowledgeBaseRdf4jServer(datasetUrl, datasetName);
		} else if (datasetUrl.contains("/testrdf4jmeta")) {
			result = new KnowledgeBaseRdf4jServer(datasetUrl, datasetName);
		} else {
			String[] preexistingDatasets = new String[] {"misc", "test", "testfilebased"};
			for (String current: preexistingDatasets) {
				if (datasetUrl.endsWith(current)) {
					result = new KnowledgeBaseFileBased(datasetUrl);
					break;
				}
			} 
		}
		
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
}
