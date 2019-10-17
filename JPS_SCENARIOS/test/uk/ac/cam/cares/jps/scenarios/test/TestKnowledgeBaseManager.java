package uk.ac.cam.cares.jps.scenarios.test;

import java.util.UUID;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.config.KeyValueManager;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.base.util.FileUtil;

public class TestKnowledgeBaseManager extends TestCase {

	public void testPutAndGetNonRDFFile() {

		String path = KeyValueManager.getServerAddress() + "/jps/data/test/testputandget";
		String body = UUID.randomUUID().toString();
		new QueryBroker().put(path, body);

		String result = new QueryBroker().readFile(path);
		assertEquals(body, result);
	}
	
	public void testPutAndGetRDFFileWithAcceptFormat() {
		
		String filePath = AgentLocator.getCurrentJpsAppDirectory(this) + "/testres" + "/E-303load.owl";
		String body = FileUtil.readFileLocally(filePath);
		String path = KeyValueManager.getServerAddress() + "/jps/kb/test/testE-303load.owl";
		new QueryBroker().put(path, body);

		// TODO accept type is missing here so far
		String result = new QueryBroker().readFile(path);
		assertEquals(body, result);
	}
}
