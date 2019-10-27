package uk.ac.cam.cares.jps.scenario.kb.test;

import uk.ac.cam.cares.jps.base.query.KnowledgeBaseClient;
import uk.ac.cam.cares.jps.scenario.kb.KnowledgeBaseAbstract;
import uk.ac.cam.cares.jps.scenario.kb.KnowledgeBaseManager;

public class KnowledgeBaseSwitchClient {

	private String datasetUrl = null;
	private boolean direct = true;
	private KnowledgeBaseAbstract kb = null;
	
	public KnowledgeBaseSwitchClient(String datasetUrl, boolean direct) {
		this.datasetUrl = datasetUrl;
		this.direct = direct;
		if (direct) {
			kb = KnowledgeBaseManager.getKnowledgeBase(datasetUrl);
		}
	}
	
	public void put(String resourceUrl, String content, String contentType) {
		if (direct) {
			kb.put(resourceUrl, content, contentType);
		} else {
			KnowledgeBaseClient.put(datasetUrl, resourceUrl, content, contentType);
		}
	}
	
	public void update(String resourceUrl, String sparql) {
		if (direct) {
			kb.update(resourceUrl, sparql);
		} else {
			KnowledgeBaseClient.update(datasetUrl, resourceUrl, sparql);
		}
	}
	
	public String get(String resourceUrl, String accept) {
		if (direct) {
			return kb.get(resourceUrl, accept);
		} else {
			return KnowledgeBaseClient.get(datasetUrl, resourceUrl, accept);
		}
	}
	
	public String query(String resourceUrl, String sparql) {
		if (direct) {
			return kb.query(resourceUrl, sparql);
		} else {
			return KnowledgeBaseClient.query(datasetUrl, resourceUrl, sparql);
		}
	}
}
