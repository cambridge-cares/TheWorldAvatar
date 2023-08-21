package com.cmclinnovations.stack.services;

import com.cmclinnovations.stack.clients.core.BasicEndpointConfig;
import com.cmclinnovations.stack.clients.core.StackClient;
import com.cmclinnovations.stack.services.config.ServiceConfig;

public class BasicAgentService extends ContainerService {

    public static final String TYPE = "basic-agent";

    public BasicAgentService(String stackName, ServiceConfig config) {
        super(stackName, config);

        String name = this.getName();
        this.getConfig().getEndpoints().entrySet().forEach(entry -> {
            String endpointConfigName = StackClient.removeStackName(name) + "-" + entry.getKey();
            String url = entry.getValue().getUrl().toString().replace("localhost", name);
            BasicEndpointConfig endpointConfig = new BasicEndpointConfig(endpointConfigName, url);

            addEndpointConfig(endpointConfig);
        });
    }
}