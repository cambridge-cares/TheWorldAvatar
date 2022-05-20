package com.cmclinnovations;

import java.io.IOException;
import java.net.URISyntaxException;
import java.net.URL;

import com.cmclinnovations.services.ServiceManager;

public class Stack {

    private final String name;
    private final URL hostURL;

    private final ServiceManager manager;

    public Stack(String name, URL hostURL, ServiceManager manager) throws IOException, URISyntaxException {
        this.name = name;
        this.hostURL = hostURL;

        this.manager = manager;

        manager.initialiseService(name, "docker");

        manager.initialiseService(name, "nginx");

        manager.initialiseService(name, "blazegraph");

        manager.initialiseService(name, "postgis");

    }

}
