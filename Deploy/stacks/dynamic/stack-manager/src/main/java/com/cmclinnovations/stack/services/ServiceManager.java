package com.cmclinnovations.stack.services;

import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.nio.file.Path;
import java.util.HashMap;
import java.util.Map;

import com.cmclinnovations.stack.services.config.ServiceConfig;
import com.cmclinnovations.stack.clients.utils.FileUtils;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.ObjectMapper;

public final class ServiceManager {

    private static final ObjectMapper objectMapper = new ObjectMapper();

    @JsonProperty(value = "services")
    private final Map<String, ServiceConfig> serviceConfigs = new HashMap<>();

    @JsonIgnore
    private final Map<String, Service> services = new HashMap<>();

    public ServiceManager() {
        try {
            URL url = ServiceManager.class.getResource("defaults");
            loadConfigs(url);
        } catch (IOException | URISyntaxException ex) {
            throw new RuntimeException("Failed to load default service configs.", ex);
        }
    }

    public void loadConfig(URL url) throws IOException {
        serviceConfigs.put(FileUtils.getFileNameWithoutExtension(url),
                objectMapper.readValue(url, ServiceConfig.class));
    }

    public void loadConfig(URI uri) throws IOException {
        loadConfig(uri.toURL());
    }

    public void loadConfig(Path path) throws IOException {
        loadConfig(path.toUri());
    }

    public void loadConfigs(Path configDir) throws IOException, URISyntaxException {
        loadConfigs(configDir.toUri().toURL());
    }

    public void loadConfigs(URL dirURL) throws IOException, URISyntaxException {
        for (URI uri : FileUtils.listFiles(dirURL, ".json")) {
            loadConfig(uri);
        }
    }

    public ServiceConfig getServiceConfig(String serviceName) {
        return serviceConfigs.get(serviceName);
    }

    public <S extends Service> S initialiseService(String stackName, String serviceName) {
        ServiceConfig config = serviceConfigs.get(serviceName);
        String type = config.getType();

        Class<S> typeClass = AbstractService.getTypeClass(type.toLowerCase());
        if (null == typeClass) {
            throw new IllegalArgumentException("Service '" + serviceName + "' is of type '" + type
                    + "', which does not have a specific class defined.");
        }

        final Service newService;
        try {
            newService = typeClass
                    .getConstructor(String.class, ServiceManager.class, ServiceConfig.class)
                    .newInstance(stackName, this, config);
        } catch (InstantiationException | IllegalAccessException | IllegalArgumentException
                | InvocationTargetException | NoSuchMethodException | SecurityException ex) {
            throw new IllegalArgumentException("Service '" + serviceName + "', of type '" + type
                    + "', could not be created.", ex);
        }

        if (newService instanceof ContainerService) {
            ContainerService newContainerService = (ContainerService) newService;

            DockerService dockerService = this.<DockerService>getService("docker");
            if (null != dockerService) {
                dockerService.doPreStartUpConfiguration(newContainerService);
                dockerService.startContainer(newContainerService);
                dockerService.doPostStartUpConfiguration(newContainerService);
            }

            ReverseProxyService reverseProxyService = this.<ReverseProxyService>getService("nginx");
            if (null != reverseProxyService) {
                reverseProxyService.addService(newContainerService);
            }
        }

        services.put(serviceName, newService);

        return typeClass.cast(newService);
    }

    <S extends Service> S getService(String otherServiceName) {
        return (S) services.get(otherServiceName);
    }

}
