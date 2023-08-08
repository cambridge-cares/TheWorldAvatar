package com.cmclinnovations.stack.services;

import java.util.HashMap;
import java.util.Map;

import com.cmclinnovations.stack.clients.core.BasicEndpointConfig;
import com.cmclinnovations.stack.clients.core.StackClient;
import com.cmclinnovations.stack.services.config.ServiceConfig;

public class BasicAgentService extends ContainerService {

    public static final String TYPE = "basic-agent";

    private Map<String, BasicEndpointConfig> endpointConfigMap;

    public BasicAgentService(String stackName, ServiceConfig config) {
        super(stackName, config);

        endpointConfigMap = new HashMap<>();

        this.getConfig().getEndpoints().entrySet().forEach(entry -> {
            String endpointConfigName = StackClient.removeStackName(config.getName()) + "-" + entry.getKey();
            String url = entry.getValue().getUrl().toString().replace("localhost", this.getName());
            BasicEndpointConfig endpointConfig = new BasicEndpointConfig(endpointConfigName, url);
            endpointConfigMap.put(entry.getKey(), endpointConfig);
        });
    }

    @Override
    public void doPostStartUpConfiguration() {
        endpointConfigMap.entrySet().forEach(entry -> writeEndpointConfig(entry.getValue()));
    }
}