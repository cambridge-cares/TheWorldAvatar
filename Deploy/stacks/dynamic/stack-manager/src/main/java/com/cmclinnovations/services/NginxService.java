package com.cmclinnovations.services;

public class NginxService extends ContainerService implements ReverseProxyService {

    static final String TYPE = "nginx";

    public NginxService(String stackName, ServiceManager serviceManager, ServiceConfig config) {
        super(stackName, serviceManager, config);
    }

    public void addService(Service service) {

    }

}
