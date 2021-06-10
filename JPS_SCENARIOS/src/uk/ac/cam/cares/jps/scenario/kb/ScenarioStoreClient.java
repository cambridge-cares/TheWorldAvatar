package uk.ac.cam.cares.jps.scenario.kb;

import java.io.File;

import org.apache.jena.riot.Lang;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import uk.ac.cam.cares.jps.base.query.FileBasedKnowledgeBaseClient;
import uk.ac.cam.cares.jps.base.scenario.BucketHelper;


public class ScenarioStoreClient {

	private static Logger logger = LoggerFactory.getLogger(ScenarioStoreClient.class);
	
	private String scenarioUrl;

	/**
	 * Constructor
	 * @param scenarioUrl
	 */
	public ScenarioStoreClient(String scenarioUrl){
		this.scenarioUrl = scenarioUrl; 
	}

	/**
	 * Create new file and insert contents
	 * @param resourceUrl
	 * @param content
	 * @param contentType
	 */
	public void put(String resourceUrl, String content, String contentType) {
		logger.info("put resourceUrl=" + resourceUrl + " (kb url=" + scenarioUrl + ")");
		String filePath = BucketHelper.getLocalPath(resourceUrl, scenarioUrl);
		
		FileBasedKnowledgeBaseClient kbClient = new FileBasedKnowledgeBaseClient();
		kbClient.setAutoWrite(false);
		kbClient.put(resourceUrl, content, contentType);
		kbClient.writeToFile(null, filePath, Lang.RDFXML);
	}

	public void update(String resourceUrl, String sparql) {
		logger.info("update resourceUrl=" + resourceUrl + " (kb url=" + scenarioUrl + ")");
		String filePath = BucketHelper.getLocalPath(resourceUrl, scenarioUrl);
		
		FileBasedKnowledgeBaseClient kbClient = new FileBasedKnowledgeBaseClient(filePath);
		kbClient.executeUpdate(sparql);
	}

	public String get(String resourceUrl, String accept) {
		logger.info("get resourceUrl=" + resourceUrl + " (kb url=" + scenarioUrl + ")");
		String filePath = BucketHelper.getLocalPath(resourceUrl, scenarioUrl);

		FileBasedKnowledgeBaseClient kbClient = new FileBasedKnowledgeBaseClient(filePath);
		String result = kbClient.get(resourceUrl, accept);
		return result;
	}

	public String query(String resourceUrl, String sparql) {
		logger.info("query resourceUrl=" + resourceUrl + " (kb url=" + scenarioUrl + ")");
		String filePath = BucketHelper.getLocalPath(resourceUrl, scenarioUrl);

		FileBasedKnowledgeBaseClient kbClient = new FileBasedKnowledgeBaseClient(filePath);
		String result = kbClient.execute(sparql);
		return result;
	}
	
	/**
	 * Does the file already exist in the scenario folder
	 * @param resourceUrl
	 * @return
	 */
	public boolean exists(String resourceUrl) {
		String filePath = BucketHelper.getLocalPath(resourceUrl, scenarioUrl);
		boolean exists = new File(filePath).exists();
		logger.info("exists=" + exists + " for resourceUrl=" + resourceUrl + " (kb url=" + scenarioUrl + ")");
		return exists;
	}
	
	public String getScenarioUrl() {
		return scenarioUrl;
	}
}
