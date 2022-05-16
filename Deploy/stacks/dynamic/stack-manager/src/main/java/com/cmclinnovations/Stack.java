package com.cmclinnovations;

import com.cmclinnovations.services.DockerService;
import com.cmclinnovations.services.ReverseProxyService;
import com.cmclinnovations.services.ServiceManager;

public class Stack {

    private final ServiceManager manager;
    private final DockerService docker;
    private final ReverseProxyService proxy;

    public Stack(ServiceManager manager, DockerService docker, ReverseProxyService proxy) {
        this.manager = manager;
        this.docker = docker;
        this.proxy = proxy;

        docker.startContainer(proxy.getConfig());
    }

}
