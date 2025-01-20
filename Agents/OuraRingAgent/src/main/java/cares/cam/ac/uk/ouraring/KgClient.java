package cares.cam.ac.uk.ouraring;

import com.cmclinnovations.stack.clients.blazegraph.BlazegraphClient;

import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

public class KgClient {
    RemoteStoreClient remoteStoreClient;

    public KgClient() {
        remoteStoreClient = BlazegraphClient.getInstance().getRemoteStoreClient(Config.NAMESPACE);
    }

    String getUserIri(String userId) {
        return null;
    }
}
