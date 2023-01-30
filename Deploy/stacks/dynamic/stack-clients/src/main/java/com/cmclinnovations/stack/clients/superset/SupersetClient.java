package com.cmclinnovations.stack.clients.superset;

import com.cmclinnovations.stack.clients.core.ClientWithEndpoint;
import com.cmclinnovations.stack.clients.core.EndpointNames;
import com.cmclinnovations.stack.clients.docker.ContainerClient;
import com.cmclinnovations.stack.clients.postgis.PostGISEndpointConfig;
import com.cmclinnovations.swagger.superset.ApiClient;
import com.cmclinnovations.swagger.superset.ApiException;
import com.cmclinnovations.swagger.superset.Configuration;
import com.cmclinnovations.swagger.superset.api.DatabaseApi;
import com.cmclinnovations.swagger.superset.model.DatabaseRestApiPost;

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
    private final ApiClient apiClient;

    private SupersetClient() {
        supersetEndpoint = readEndpointConfig(EndpointNames.SUPERSET, SupersetEndpointConfig.class);
        apiClient = new ApiClient();
        apiClient.setBasePath(supersetEndpoint.getUrl());
        Configuration.setDefaultApiClient(apiClient);
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

    private void setAccessToken() {
        apiClient.setAccessToken(supersetEndpoint.getAccessToken());
    }

    public ApiClient getApiClient() {
        return apiClient;
    }

}
