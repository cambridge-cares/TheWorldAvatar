package com.cmclinnovations;

import java.net.URISyntaxException;

import com.cmclinnovations.services.DockerService;
import com.cmclinnovations.services.NginxService;
import com.cmclinnovations.services.ReverseProxyService;
import com.cmclinnovations.services.ServiceManager;

/**
 * Hello world!
 *
 */
public class App {
    public static void main(String[] args) throws URISyntaxException {
        ServiceManager manager = new ServiceManager();
        DockerService docker = new DockerService(manager.getServiceConfig("docker"));
        ReverseProxyService proxy = new NginxService(manager.getServiceConfig("nginx"));

        Stack stack = new Stack(manager, docker, proxy);
    }
}
