package com.cmclinnovations.stack.services;

import java.util.Map;
import java.util.Map.Entry;

import com.cmclinnovations.stack.services.config.Connection;
import com.cmclinnovations.stack.services.config.ServiceConfig;
import com.github.odiszapc.nginxparser.NgxBlock;

public class FeatureInfoAgentService extends ContainerService {

    public static final String TYPE = "feature-info-agent";

    public FeatureInfoAgentService(String stackName, ServiceConfig config) {
        super(stackName, config);
    }

    @Override
    public void addServerSpecificNginxSettingsToLocationBlock(NgxBlock locationBlock, Map<String, String> upstreams,
            Entry<String, Connection> endpoint) {
        NginxService.addOpenCORSForGetNginxSettings(locationBlock);
    }
}
