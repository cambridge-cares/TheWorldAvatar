package uk.ac.cam.cares.jps.agent.gfaagent;

import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

public class GFAkg {
    private RemoteStoreClient kgClient;
    private String kgurl;

    public GFAkg (String kgUrl) {
        this.kgurl = kgUrl;
        this.kgClient = new RemoteStoreClient(kgUrl,kgUrl,null,null);
    }
}
