package uk.ac.cam.cares.jps.scenario;

import java.io.File;

import org.apache.jena.riot.Lang;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import uk.ac.cam.cares.jps.base.query.FileBasedStoreClient;
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

	public String getScenarioUrl() {
		return scenarioUrl;
	}
	
	public String getFilePath(String resourceUrl) {
		return BucketHelper.getLocalPath(resourceUrl, scenarioUrl);
	}
	
	/**
	 * Does the file already exist in the scenario folder?
	 * @param resourceUrl
	 * @return
	 */
	public boolean exists(String resourceUrl) {
		String filePath = getFilePath(resourceUrl);
		boolean exists = new File(filePath).exists();
		logger.info("exists=" + exists + " for resourceUrl=" + resourceUrl + " (kb url=" + scenarioUrl + ")");
		return exists;
	}
	
	/**
	 * Create a new file (resource) in the scenario folder and insert content with given content type
	 * @param resourceUrl
	 * @param content
	 * @param contentType
	 */
	public void put(String resourceUrl, String content, String contentType) {
		logger.info("put resourceUrl=" + resourceUrl + " (kb url=" + scenarioUrl + ")");
		String filePath = getFilePath(resourceUrl);
		
		FileBasedStoreClient kbClient = new FileBasedStoreClient();
		kbClient.setAutoWrite(false);
		kbClient.insert(null, content, contentType); //Put to default graph
		kbClient.writeToFile(null, filePath, Lang.RDFXML);
	}

	/**
	 * Perform a sparql update on a resource (file) in the scenario folder
	 * @param resourceUrl
	 * @param sparql
	 */
	public void update(String resourceUrl, String sparql) {
		logger.info("update resourceUrl=" + resourceUrl + " (kb url=" + scenarioUrl + ")");
		String filePath = getFilePath(resourceUrl);
		
		FileBasedStoreClient kbClient = new FileBasedStoreClient(filePath);
		kbClient.executeUpdate(sparql);
	}

	/**
	 * Get content of a resource (file) in the scenario folder
	 * @param resourceUrl
	 * @param accept
	 * @return
	 */
	public String get(String resourceUrl, String accept) {
		logger.info("get resourceUrl=" + resourceUrl + " (kb url=" + scenarioUrl + ")");
		String filePath = getFilePath(resourceUrl);

		FileBasedStoreClient kbClient = new FileBasedStoreClient(filePath); //load to default graph
		String result = kbClient.get(null, accept);
		return result;
	}

	/**
	 * Perform a sparql query on a resource (file) in the scenario folder
	 * @param resourceUrl
	 * @param sparql
	 * @return
	 */
	public String query(String resourceUrl, String sparql) {
		logger.info("query resourceUrl=" + resourceUrl + " (kb url=" + scenarioUrl + ")");
		String filePath = getFilePath(resourceUrl);

		FileBasedStoreClient kbClient = new FileBasedStoreClient(filePath);
		String result = kbClient.execute(sparql);
		return result;
	}
}
