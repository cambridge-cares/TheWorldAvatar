package uk.ac.cam.cares.jps.agent.bmsquery;

import org.junit.Test;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

import static org.junit.Assert.assertNotNull;

public class BMSQueryAgentTest {
    BMSQueryAgent agent;

    @Test
    public void testSetRSClient() {
        agent = new BMSQueryAgent();
        RemoteStoreClient labRsClient = new RemoteStoreClient();
        RemoteStoreClient officeRsClient = new RemoteStoreClient();

        agent.setRSClient(labRsClient, officeRsClient);
        assertNotNull(agent.labRsClient);
    }
}
