package com.cmclinnovations.stack.clients.docker;

import com.github.dockerjava.api.DockerClient;
import com.github.dockerjava.core.DockerClientConfig;
import com.github.dockerjava.transport.DockerHttpClient;

public interface ContainerManager<C extends DockerClient> {

    public C buildInternalClient(DockerClientConfig dockerConfig,
            DockerHttpClient httpClient);

    public C getInternalClient();
}
