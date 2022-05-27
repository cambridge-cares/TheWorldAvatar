package com.cmclinnovations.services;

public class NginxService extends ContainerService implements ReverseProxyService {

    public static final String TYPE = "nginx";

    public NginxService(String stackName, ServiceManager serviceManager, ServiceConfig config) {
        super(stackName, serviceManager, config);
    }

    public void addService(Service service) {

    }

}
