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
        locationBlock.addEntry(allowOriginHeaderParam);

        NgxParam allowCredentialsHeaderParam = new NgxParam();
        allowCredentialsHeaderParam.addValue("add_header");
        allowCredentialsHeaderParam.addValue("'Access-Control-Allow-Credentials'");
        allowCredentialsHeaderParam.addValue("'true'");
        locationBlock.addEntry(allowCredentialsHeaderParam);

        NgxParam allowMethodsHeaderParam = new NgxParam();
        allowMethodsHeaderParam.addValue("add_header");
        allowMethodsHeaderParam.addValue("'Access-Control-Allow-Methods'");
        allowMethodsHeaderParam.addValue("'GET, POST, OPTIONS'");
        locationBlock.addEntry(allowMethodsHeaderParam);

        NgxParam allowHeadersHeaderParam = new NgxParam();
        allowHeadersHeaderParam.addValue("add_header");
        allowHeadersHeaderParam.addValue("'Access-Control-Allow-Headers'");
        allowHeadersHeaderParam.addValue(
                "'DNT,X-CustomHeader,Keep-Alive,User-Agent,X-Requested-With,If-Modified-Since,Cache-Control,Content-Type'");
        locationBlock.addEntry(allowHeadersHeaderParam);
    }
}
