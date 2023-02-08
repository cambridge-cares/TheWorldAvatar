package com.cmclinnovations.stack.services;

import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.cmclinnovations.stack.clients.utils.FileUtils;
import com.cmclinnovations.stack.services.config.ServiceConfig;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.ObjectMapper;

public final class ServiceManager {

    private static final ObjectMapper objectMapper = new ObjectMapper();

    @JsonProperty(value = "services")
    private final Map<String, ServiceConfig> serviceConfigs = new HashMap<>();

    @JsonIgnore
    private final Map<String, Service> services = new HashMap<>();

    private final List<String> defaultServices;
    private final List<String> userServices;

    public ServiceManager() {
        try {
            URL url = ServiceManager.class.getResource("defaults");
            defaultServices = loadConfigs(url);
        } catch (IOException | URISyntaxException ex) {
            throw new RuntimeException("Failed to load default service configs.", ex);
        }

        try {
            Path configDir = Path.of("/inputs/config");
            if (Files.exists(configDir)) {
                userServices = loadConfigs(configDir);
                userServices.removeAll(defaultServices);
            } else {
                userServices = Collections.emptyList();
            }
        } catch (IOException | URISyntaxException ex) {
            throw new RuntimeException("Failed to load user specified service configs.", ex);
        }
    }

    public String loadConfig(URL url) throws IOException {
        String serviceName = FileUtils.getFileNameWithoutExtension(url);
        serviceConfigs.put(serviceName, objectMapper.readValue(url, ServiceConfig.class));
        return serviceName;
    }

    public String loadConfig(URI uri) throws IOException {
        return loadConfig(uri.toURL());
    }

    public String loadConfig(Path path) throws IOException {
        return loadConfig(path.toUri());
    }

    public List<String> loadConfigs(Path configDir) throws IOException, URISyntaxException {
        return loadConfigs(configDir.toUri().toURL());
    }

    public List<String> loadConfigs(URL dirURL) throws IOException, URISyntaxException {
        List<String> serviceNames = new ArrayList<>();
        for (URI uri : FileUtils.listFiles(dirURL, ".json")) {
            serviceNames.add(loadConfig(uri));
        }
        return serviceNames;
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

    public void initialiseUserServices(String stackName) {
        userServices.forEach(serviceName -> initialiseService(stackName, serviceName));
    }

}
