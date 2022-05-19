package com.cmclinnovations;

import java.io.IOException;

import com.cmclinnovations.services.ContainerService;
import com.cmclinnovations.services.DockerService;
import com.cmclinnovations.services.PostgreSQLService;
import com.cmclinnovations.services.ReverseProxyService;
import com.cmclinnovations.services.ServiceManager;

public class Stack<P extends ContainerService & ReverseProxyService> {

    private final String name;
    private final ServiceManager manager;
    private final DockerService docker;
    private final P proxy;

    public Stack(String name, ServiceManager manager, DockerService docker, P proxy) throws IOException {
        this.name = name;
        this.manager = manager;
        this.docker = docker;
        this.proxy = proxy;

        docker.createNetwork(name);

        docker.startContainer(proxy);

        docker.startContainer(new PostgreSQLService(name, manager.getServiceConfig("postgis")));
    }

}
