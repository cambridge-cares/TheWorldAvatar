package com.cmclinnovations.stack;

import java.net.URL;

import com.cmclinnovations.stack.services.ServiceManager;

public class Stack {

    private final String name;
    private final URL hostURL;

    private final ServiceManager manager;

    public Stack(String name, URL hostURL, ServiceManager manager) {
        this.name = name;
        this.hostURL = hostURL;

        this.manager = manager;

        manager.initialiseService(name, "docker");

        manager.initialiseService(name, "nginx");

        manager.initialiseService(name, "blazegraph");

        manager.initialiseService(name, "postgis");

        manager.initialiseService(name, "adminer");

        manager.initialiseService(name, "ontop");

        manager.initialiseService(name, "gdal");

        manager.initialiseService(name, "geoserver");

        manager.initialiseUserServices(name);
    }

}
