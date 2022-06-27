package com.cmclinnovations.services;

import java.io.IOException;

import org.junit.BeforeClass;

import com.cmclinnovations.services.config.ServiceConfig;

public class DockerServiceTest {

    private static DockerService service;
    private static ServiceManager manager;

    @BeforeClass
    public static void setup() throws IOException {
        manager = new ServiceManager();
        manager.loadConfig(ServiceConfig.class.getResource("defaults/docker.json"));
        service = manager.initialiseService("TestStack", "docker");
    }
}