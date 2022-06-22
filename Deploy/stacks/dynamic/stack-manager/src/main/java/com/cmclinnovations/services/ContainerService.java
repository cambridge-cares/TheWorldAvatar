package com.cmclinnovations.services;

import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.nio.file.Path;
import java.util.Map;
import java.util.Objects;

import com.cmclinnovations.apis.AbstractEndpointConfig;
import com.cmclinnovations.apis.DockerClient;
import com.cmclinnovations.apis.StackClient;
import com.cmclinnovations.apis.DockerClient.ComplexCommand;
import com.cmclinnovations.services.config.ServiceConfig;
import com.github.dockerjava.api.model.ContainerSpec;
import com.github.dockerjava.api.model.ServiceSpec;
import com.github.dockerjava.api.model.TaskSpec;

public class ContainerService extends AbstractService {

    public static final String TYPE = "container";

    private String containerId;

    private DockerClient dockerClient;

    public ContainerService(String stackName, ServiceManager serviceManager, ServiceConfig config) {
        super(serviceManager, config);
        Objects.requireNonNull(stackName, "A 'stackName' must be provided for all container-based services.");
        config.getDockerServiceSpec().withName(StackClient.prependStackName(config.getDockerServiceSpec().getName()));
        setEnvironmentVariable(StackClient.STACK_NAME_KEY, stackName);
    }

    final String getContainerName() {
        return getName();
    }

    final String getImage() {
        return getConfig().getImage();
    }

    final ServiceSpec getServiceSpec() {
        return getConfig().getDockerServiceSpec();
    }

    public TaskSpec getTaskTemplate() {
        return getConfig().getTaskTemplate();
    }

    public ContainerSpec getContainerSpec() {
        return getConfig().getContainerSpec();
    }

    final void setContainerId(String containerId) {
        this.containerId = containerId;
    }

    final void setDockerClient(DockerClient dockerClient) {
        this.dockerClient = dockerClient;
    }

    public void doPreStartUpConfiguration() {
        // Do nothing by default, override if container needs pre-startup configuration
    }

    public void doPostStartUpConfiguration() {
        // Do nothing by default, override if container needs post-startup configuration
    }

    public final String getEnvironmentVariable(String key) {
        return getConfig().getEnvironmentVariable(key);
    }

    protected final void setEnvironmentVariable(String key, String value) {
        getConfig().setEnvironmentVariable(key, value);
    }

    protected final void setEnvironmentVariableIfAbsent(String key, String value) {
        if (!getConfig().hasEnvironmentVariable(key)) {
            setEnvironmentVariable(key, value);
        }
    }

    protected final void checkEnvironmentVariableNonNull(String key) {
        String value = getEnvironmentVariable(key);
        Objects.requireNonNull(value,
                "The service '" + getName() + "' requires the environment variable '" + key
                        + "' to be specified in its config file.");
    }

    public final void sendFiles(Map<String, byte[]> files, String remotePath) throws IOException {
        dockerClient.sendFiles(containerId, files, remotePath);
    }

    public final void executeCommand(String... cmd) {
        dockerClient.executeSimpleCommand(containerId, cmd);
    }

    public final ComplexCommand createComplexCommand(String... cmd) {
        return dockerClient.createComplexCommand(containerId, cmd);
    }

    protected final void downloadFileAndSendItToContainer(URL url, String folderPath,
            String filename,
            boolean overwrite) {
        Path filePath = Path.of(folderPath, filename);
        if (overwrite || !dockerClient.fileExists(containerId, filePath.toString())) {
            try (InputStream downloadStream = url.openStream()) {
                byte[] bytes = downloadStream.readAllBytes();
                Map<String, byte[]> files = Map.of(filename, bytes);
                dockerClient.sendFiles(containerId, files, folderPath);
            } catch (IOException ex) {
                throw new RuntimeException("Failed to download file from '" + url + "' and send it to '"
                        + folderPath + "' in the container '" + getName() + "'.", ex);
            }
        }
    }

    public <E extends AbstractEndpointConfig> void writeEndpointConfig(E endpointConfig) {
        dockerClient.writeEndpointConfig(endpointConfig);
    }

    public <E extends AbstractEndpointConfig> E readEndpointConfig(String endpointName, Class<E> endpointConfigClass) {
        return dockerClient.readEndpointConfig(endpointName, endpointConfigClass);
    }

}
