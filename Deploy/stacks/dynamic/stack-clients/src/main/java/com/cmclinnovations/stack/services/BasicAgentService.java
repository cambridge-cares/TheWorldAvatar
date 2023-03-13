package com.cmclinnovations.stack.services;

import java.util.HashMap;
import java.util.Map;

import com.cmclinnovations.stack.clients.core.BasicEndpointConfig;
import com.cmclinnovations.stack.services.config.ServiceConfig;

public class BasicAgentService extends ContainerService {

    public static final String TYPE = "basic-agent";

    private Map<String, BasicEndpointConfig> endpointConfigMap;

    public BasicAgentService(String stackName, ServiceManager serviceManager, ServiceConfig config) {
        super(stackName, serviceManager, config);

        endpointConfigMap = new HashMap<>();

        this.getConfig().getEndpoints().entrySet().forEach(entry -> {
            String endpointConfigName = config.getName().substring(stackName.length() + 1) + "-" + entry.getKey();
            String url = entry.getValue().getUrl().toString().replace("localhost", this.getName());
            BasicEndpointConfig endpointConfig = new BasicEndpointConfig(endpointConfigName, url);
            endpointConfigMap.put(endpointConfigName, endpointConfig);
        });
    }

    @Override
    public void doPostStartUpConfiguration() {
        endpointConfigMap.entrySet().forEach(entry -> writeEndpointConfig(entry.getValue()));
    }
}