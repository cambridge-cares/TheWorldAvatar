package uk.ac.cam.cares.jps.agent.bmsquery;

import org.junit.Test;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

import static org.junit.Assert.assertNotNull;

public class BMSQueryAgentTest {
    BMSQueryAgent agent;

    @Test
    public void testSetRSClient() {
        agent = new BMSQueryAgent();
<<<<<<< HEAD
        RemoteStoreClient labRsClient = new RemoteStoreClient();
        RemoteStoreClient officeRsClient = new RemoteStoreClient();

        agent.setRSClient(labRsClient, officeRsClient);
        assertNotNull(agent.labRsClient);
=======
        RemoteStoreClient rsClient = new RemoteStoreClient();
        List<String> endpoints = new ArrayList<>();

        agent.setRSClient(rsClient, endpoints);
        assertNotNull(agent.kgUrls);
        assertNotNull(agent.rsClient);
>>>>>>> parent of 4fae184ea3 (Merge branch 'main' of https://github.com/cambridge-cares/TheWorldAvatar into main)
    }
}
