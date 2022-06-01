package com.cmclinnovations.services;

import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.nio.file.Path;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;

import com.cmclinnovations.services.config.ServiceConfig;
import com.github.dockerjava.api.model.HostConfig;

public class ContainerService extends AbstractService {

    public static final String TYPE = "container";

    private final String stackName;
    private String containerId;

    private DockerService dockerService;

    public ContainerService(String stackName, ServiceManager serviceManager, ServiceConfig config) {
        super(serviceManager, config);
        Objects.requireNonNull(stackName, "A 'stackName' must be provided for all container-based services.");
        this.stackName = stackName;
    }

    String getContainerName() {
        return stackName + "_" + getName();
    }

    String getImage() {
        return getConfig().getImage();
    }

    HostConfig getHostConfig() {
        return getConfig().getDockerHostConfig();
    }

    List<String> getEnvironment() {
        return getConfig().getEnvironment().entrySet().stream()
                .map(entry -> entry.getKey() + "=" + entry.getValue())
                .collect(Collectors.toList());
    }

    void setContainerId(String containerId) {
        this.containerId = containerId;
    }

    void setDockerService(DockerService dockerService) {
        this.dockerService = dockerService;
    }

    public void sendFiles(Map<String, byte[]> files, String remotePath) throws IOException {
        dockerService.sendFiles(containerId, files, remotePath);
    }

    public void executeCommand(String... cmd) {
        dockerService.executeCommand(containerId, cmd);
    }

    protected final void downloadFileAndSendItToContainer(URL url, String folderPath,
            String filename,
            boolean overwrite) {
        Path filePath = Path.of(folderPath, filename);
        if (overwrite || !dockerService.fileExists(containerId, filePath.toString())) {
            try (InputStream downloadStream = url.openStream()) {
                byte[] bytes = downloadStream.readAllBytes();
                Map<String, byte[]> files = Map.of(filename, bytes);
                dockerService.sendFiles(containerId, files, folderPath);
            } catch (IOException ex) {
                throw new RuntimeException("Failed to download file from '" + url + "' and send it to '"
                        + folderPath + "' in the container '" + getName() + "'.", ex);
            }
        }
    }

}
