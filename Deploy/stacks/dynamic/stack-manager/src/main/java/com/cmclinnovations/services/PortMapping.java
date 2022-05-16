package com.cmclinnovations.services;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.github.dockerjava.api.model.PortBinding;

public class PortMapping {

    private final PortBinding portBinding;

    @JsonCreator
    public PortMapping(String serialized) {
        portBinding = PortBinding.parse(serialized);
    }

    public PortBinding getPortBinding() {
        return portBinding;
    }

    // The port exposed by the container within the network.
    public Integer getInternalPort() {
        return portBinding.getExposedPort().getPort();
    }

    // The protocol of the connection: tcp, udp or sctp.
    public String getProtocol() {
        return portBinding.getExposedPort().getProtocol().toString();
    }

    // The port exposed by the container outside the network.
    public Integer getExternalPort() {
        return Integer.parseInt(portBinding.getBinding().getHostPortSpec());
    }

    // IP address on host machine that the port is attached to.
    // Only required on machines with muliple IP addresses.
    public String getExternalIP() {
        return portBinding.getBinding().getHostIp();
    }
}
