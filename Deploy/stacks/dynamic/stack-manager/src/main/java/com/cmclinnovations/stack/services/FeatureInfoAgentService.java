package com.cmclinnovations.stack.services;

import java.util.Map;
import java.util.Map.Entry;

import com.cmclinnovations.stack.services.config.Connection;
import com.cmclinnovations.stack.services.config.ServiceConfig;
import com.github.odiszapc.nginxparser.NgxBlock;
import com.github.odiszapc.nginxparser.NgxParam;

public class FeatureInfoAgentService extends ContainerService {

    public static final String TYPE = "feature-info-agent";

    public FeatureInfoAgentService(String stackName, ServiceManager serviceManager, ServiceConfig config) {
        super(stackName, serviceManager, config);
    }

    @Override
    public void addServerSpecificNginxSettingsToLocationBlock(NgxBlock locationBlock, Map<String, String> upstreams,
            Entry<String, Connection> endpoint) {

        NgxParam allowOriginHeaderParam = new NgxParam();
        allowOriginHeaderParam.addValue("add_header");
        allowOriginHeaderParam.addValue("'Access-Control-Allow-Origin'");
        allowOriginHeaderParam.addValue("'*'");
        allowOriginHeaderParam.addValue("always");
        locationBlock.addEntry(allowOriginHeaderParam);
    }
}
