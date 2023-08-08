package com.cmclinnovations.stack.services;

import java.net.URI;

import com.cmclinnovations.stack.clients.docker.DockerClient;

interface ContainerManagerService<C extends DockerClient> {

    C initClient(URI dockerUri);

    C getClient();
}
