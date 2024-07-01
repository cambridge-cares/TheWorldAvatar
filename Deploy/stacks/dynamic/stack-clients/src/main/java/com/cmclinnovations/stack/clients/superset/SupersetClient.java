package com.cmclinnovations.stack.clients.superset;

import com.cmclinnovations.stack.clients.core.ClientWithEndpoint;
import com.cmclinnovations.stack.clients.core.EndpointNames;
import com.cmclinnovations.swagger.superset.ApiClient;
import com.cmclinnovations.swagger.superset.Configuration;

import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

public class SupersetClient extends ClientWithEndpoint<SupersetEndpointConfig> {

    private static SupersetClient instance = null;

    public static SupersetClient getInstance() {
        if (null == instance) {
            instance = new SupersetClient();
        }
        return instance;
    }

    private final ApiClient apiClient;

    private SupersetClient() {
        super(EndpointNames.SUPERSET, SupersetEndpointConfig.class);
        apiClient = new ApiClient();
        apiClient.setBasePath(getEndpointConfig().getUrl());
        Configuration.setDefaultApiClient(apiClient);
    }

    public RemoteStoreClient getRemoteStoreClient() {
        SupersetEndpointConfig endpoint = getEndpointConfig();
        String url = endpoint.getUrl();
        return new RemoteStoreClient(url, url,
                endpoint.getUsername(),
                endpoint.getPassword());
    }

    private void setAccessToken() {
        apiClient.setAccessToken(getEndpointConfig().getAccessToken());
    }

    public ApiClient getApiClient() {
        return apiClient;
    }

    public void refreshAccessToken() {
        apiClient.setAccessToken(getEndpointConfig().getAccessToken());
    }

}
