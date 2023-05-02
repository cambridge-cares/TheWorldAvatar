package com.cmclinnovations.stack.services;

import com.cmclinnovations.stack.clients.citydb.CityTilerClient;
import com.cmclinnovations.stack.clients.docker.DockerClient;
import com.cmclinnovations.stack.services.config.ServiceConfig;

public final class CityTilerService extends ContainerService {

    public static final String TYPE = "citytiler";

    public CityTilerService(String stackName, ServiceManager serviceManager, ServiceConfig config) {
        super(stackName, serviceManager, config);
    }

    @Override
    public void doPostStartUpConfiguration() {

        DockerClient dockerClient = DockerClient.getInstance();

        dockerClient.executeSimpleCommand(dockerClient.getContainerId(TYPE), "cp",
                CityTilerClient.COLOUR_CONFIG_FILE, CityTilerClient.DEFAULT_COLOUR_CONFIG_FILE);
    }
}
