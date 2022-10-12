package uk.ac.cam.cares.jps.base.agent.test;

import java.util.UUID;

import org.junit.Assert;
import org.junit.Test;

import uk.ac.cam.cares.jps.base.agent.DerivationAgent;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.interfaces.StoreClientInterface;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

public class DerivationAgentTest {

    @Test
    public void testCheckIfDerivationClientInitialised() {
        DerivationAgent derivationAgent = new DerivationAgent();
        // should throw error as this.devClient is not initialised yet
		JPSRuntimeException e = Assert.assertThrows(JPSRuntimeException.class,
            () -> derivationAgent.checkIfDerivationClientInitialised());
        Assert.assertTrue(e.getMessage()
            .contains(DerivationAgent.DERIVATION_CLIENT_NOT_INITIALISED));

        // should work just fine with a dummy storeClient
        String endpoint = "http://localhost:8080/blazegraph/namespace/kb/sparql";
        StoreClientInterface storeClient = new RemoteStoreClient(endpoint, endpoint);
        DerivationAgent anotherDerivationAgent = new DerivationAgent(storeClient, "http://" + UUID.randomUUID().toString());
        anotherDerivationAgent.checkIfDerivationClientInitialised();
    }
}
