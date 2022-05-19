package com.cmclinnovations.services;

public class NginxService extends ContainerService implements ReverseProxyService {

    public NginxService(String stackName, ServiceConfig config) {
        super(stackName, config);
    }

}
