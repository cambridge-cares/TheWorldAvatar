package com.cmclinnovations.stack.services;

import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Objects;

import com.cmclinnovations.stack.clients.core.EndpointConfig;
import com.cmclinnovations.stack.clients.core.StackClient;
import com.cmclinnovations.stack.clients.docker.DockerClient;
import com.cmclinnovations.stack.clients.docker.DockerClient.ComplexCommand;
import com.cmclinnovations.stack.services.config.Connection;
import com.cmclinnovations.stack.services.config.ServiceConfig;
import com.github.dockerjava.api.model.ContainerSpec;
import com.github.dockerjava.api.model.ServiceSpec;
import com.github.dockerjava.api.model.TaskSpec;
import com.github.odiszapc.nginxparser.NgxBlock;
import com.github.odiszapc.nginxparser.NgxConfig;

public class ContainerService extends AbstractService {

    public static final String TYPE = "container";

    private String containerId;

    private DockerClient dockerClient;

    List<EndpointConfig> endpointConfigs = new ArrayList<>();

    public ContainerService(String stackName, ServiceConfig config) {
        super(config);
        Objects.requireNonNull(stackName, "A 'stackName' must be provided for all container-based services.");
        config.getDockerServiceSpec().withName(StackClient.prependStackName(config.getDockerServiceSpec().getName())
                // See comments in the getHostName method
                .replace('_', '-'));
        setEnvironmentVariable(StackClient.STACK_NAME_KEY, stackName);
    }

    final String getContainerName() {
        return getName();
    }

    public String getHostName() {
        // Officially hostnames can't contain "_" characters but Docker uses them.
        // Currently Docker only seems to add the service name as an alias,
        // it ignores any manually added ones, so have to replace "_" characters in the
        // service name rather than just here.
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

    protected void doPreStartUpConfiguration() {
        // Do nothing by default, override if container needs pre-startup configuration
    }

    protected final void addEndpointConfig(EndpointConfig endpointConfig) {
        endpointConfigs.add(endpointConfig);
    }

    public final void writeEndpointConfigs() {
        endpointConfigs.forEach(this::writeEndpointConfig);
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

    public void removeEnvironmentVariable(String key) {
        getConfig().removeEnvironmentVariable(key);
    }

    protected final void checkEnvironmentVariableNonNull(String key) {
        String value = getEnvironmentVariable(key);
        Objects.requireNonNull(value,
                "The service '" + getName() + "' requires the environment variable '" + key
                        + "' to be specified in its config file.");
    }

    public final void sendFiles(Map<String, byte[]> files, String remoteDirPath) {
        dockerClient.sendFilesContent(containerId, files, remoteDirPath);
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
                dockerClient.sendFilesContent(containerId, files, folderPath);
            } catch (IOException ex) {
                throw new RuntimeException("Failed to download file from '" + url + "' and send it to '"
                        + folderPath + "' in the container '" + getName() + "'.", ex);
            }
        }
    }

    public boolean endpointConfigExists(String configName) {
        return dockerClient.configExists(configName);
    }

    private <E extends EndpointConfig> void writeEndpointConfig(E endpointConfig) {
        dockerClient.writeEndpointConfig(endpointConfig);
    }

    public <E extends EndpointConfig> E readEndpointConfig(String endpointName, Class<E> endpointConfigClass) {
        return dockerClient.readEndpointConfig(endpointName, endpointConfigClass);
    }

    /**
     * If the named secret doesn't exist then add a dummy one, this is needed as
     * Docker requires that all secrets a container references exist.
     * 
     * @param secretName
     */
    protected boolean ensureOptionalSecret(String secretName) {
        // Check whether the secret exists in Docker/Podman
        boolean secretExists = dockerClient.secretExists(secretName);
        if (secretExists) {
            // Check to see if the secret also exists in this container
            secretExists = Files.exists(Path.of("/run/secrets", secretName));
        } else {
            // Add a dummy secret to Docker/Podman
            dockerClient.addSecret(secretName, "UNUSED");
        }
        return secretExists;
    }

    protected void addSecret(String secretName, String data) {
        dockerClient.addSecret(secretName, data);
    }

    public void addServerSpecificNginxLocationBlocks(NgxConfig locationConfigOut,
            Map<String, String> upstreams, Entry<String, Connection> endpoint) {
        // Do nothing by default, override if container needs alteration to a NGINX
        // location configuration block
    }

    public void addServerSpecificNginxSettingsToLocationBlock(NgxBlock locationBlock,
            Map<String, String> upstreams, Entry<String, Connection> endpoint) {
        // Do nothing by default, override if container needs to add an NGINX location
        // configuration block
    }

}
