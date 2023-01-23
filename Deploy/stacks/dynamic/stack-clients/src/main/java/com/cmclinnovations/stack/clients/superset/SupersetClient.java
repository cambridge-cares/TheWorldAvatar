package com.cmclinnovations.stack.clients.superset;

import com.cmclinnovations.stack.clients.core.ClientWithEndpoint;
import com.cmclinnovations.stack.clients.core.EndpointNames;
import com.cmclinnovations.stack.clients.docker.ContainerClient;

import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

public class SupersetClient extends ContainerClient implements ClientWithEndpoint {

    private static SupersetClient instance = null;

    public static SupersetClient getInstance() {
        if (null == instance) {
            instance = new SupersetClient();
        }
        return instance;
    }

    private final SupersetEndpointConfig supersetEndpoint;

    private SupersetClient() {
        supersetEndpoint = readEndpointConfig(EndpointNames.SUPERSET, SupersetEndpointConfig.class);
    }

    @Override
    public SupersetEndpointConfig getEndpoint() {
        return supersetEndpoint;
    }

    public RemoteStoreClient getRemoteStoreClient() {
        String url = supersetEndpoint.getUrl();
        return new RemoteStoreClient(url, url,
                supersetEndpoint.getUsername(),
                supersetEndpoint.getPassword());
    }

}
