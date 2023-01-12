package com.cmclinnovations.stack.clients.docker;

import java.net.URI;

public class PodmanClient extends DockerClient {

    protected PodmanClient() {
    }

    public PodmanClient(URI dockerUri) {
        super(dockerUri);
    }

}
