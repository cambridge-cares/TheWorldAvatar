package uk.ac.cam.cares.jps.agent.bmsquery;

import org.junit.Test;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

import java.util.ArrayList;
import java.util.List;

import static org.junit.Assert.assertNotNull;

public class BMSQueryAgentTest {
    BMSQueryAgent agent;

    @Test
    public void testSetRSClient() {
        agent = new BMSQueryAgent();
        RemoteStoreClient rsClient = new RemoteStoreClient();

        agent.setRSClient(rsClient);
        assertNotNull(agent.rsClient);
    }
}
