package com.cmclinnovations;

import java.io.IOException;
import java.net.URISyntaxException;

import com.cmclinnovations.services.DockerService;
import com.cmclinnovations.services.NginxService;
import com.cmclinnovations.services.ServiceManager;

/**
 * Hello world!
 *
 */
public class App {
    public static void main(String[] args) throws URISyntaxException, IOException {
        String stackName = "stack1";
        ServiceManager manager = new ServiceManager();
        DockerService docker = new DockerService(manager.getServiceConfig("docker"));
        NginxService proxy = new NginxService(stackName, manager.getServiceConfig("nginx"));

        Stack<NginxService> stack = new Stack<>(stackName, manager, docker, proxy);
        stack.hashCode();
    }
}
