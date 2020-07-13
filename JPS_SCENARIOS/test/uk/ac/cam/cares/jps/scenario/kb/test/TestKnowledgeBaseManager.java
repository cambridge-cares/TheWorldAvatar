package uk.ac.cam.cares.jps.scenario.kb.test;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.config.IKeys;
import uk.ac.cam.cares.jps.base.config.JPSConstants;
import uk.ac.cam.cares.jps.base.config.KeyValueManager;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.util.MiscUtil;
import uk.ac.cam.cares.jps.scenario.kb.KnowledgeBaseAbstract;
import uk.ac.cam.cares.jps.scenario.kb.KnowledgeBaseManager;

public class TestKnowledgeBaseManager extends TestCase {

	public void testGetDatasetUrl() {
		String datasetUrl = "http://localhost:80/" + JPSConstants.KNOWLEDGE_BASE_JPS + "/data/fancyName";
		String requestedUrl = datasetUrl;
		String result = KnowledgeBaseManager.getDatasetUrl(requestedUrl);
		assertEquals(datasetUrl, result);
		
		requestedUrl = datasetUrl + "/some/further/path/xxx.owl";
		result = KnowledgeBaseManager.getDatasetUrl(requestedUrl);
		assertEquals(datasetUrl, result);
		
		datasetUrl = "http://www.twa.com/"  + JPSConstants.KNOWLEDGE_BASE_JPS + "/kb/fancyName";
		requestedUrl = datasetUrl + "/yyy.csv";
		result = KnowledgeBaseManager.getDatasetUrl(requestedUrl);
		assertEquals(datasetUrl, result);
		
		datasetUrl = "http://localhost:8081/jps/scenario/test1234567d";
		requestedUrl = datasetUrl;
		result = KnowledgeBaseManager.getDatasetUrl(requestedUrl);
		assertEquals(datasetUrl, result);
	}
	
	public void testClearAndCreate() {
		
		KnowledgeBaseManager.getInstance().clear();
		
		String key = MiscUtil.format(IKeys.DATASET_TEMPLATE_URL, "testfilebased");
		String datasetUrl = KeyValueManager.get(key);
		KnowledgeBaseAbstract kb = KnowledgeBaseManager.getKnowledgeBase(datasetUrl);
		assertNotNull(kb);
		
		boolean caught = false;
		try {
			KnowledgeBaseManager.getKnowledgeBase("http://example.com/fancydataset");
		} catch (JPSRuntimeException e){
			caught = true;
		}
		assertTrue(caught);
		
	}
}
