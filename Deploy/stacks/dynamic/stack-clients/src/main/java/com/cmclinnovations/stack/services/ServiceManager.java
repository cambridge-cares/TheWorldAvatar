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
import java.util.NoSuchElementException;
import java.util.Optional;

import com.cmclinnovations.stack.clients.core.StackClient;
import com.cmclinnovations.stack.clients.utils.FileUtils;
import com.cmclinnovations.stack.services.config.ServiceConfig;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.ObjectMapper;

public final class ServiceManager {

    private static final ObjectMapper objectMapper = new ObjectMapper();

    private static final URL BUILTIN_CONFIG_DIR = ServiceManager.class.getResource("built-ins");
    private static final Path USER_CONFIG_DIR = StackClient.STACK_CONFIG_DIR.resolve("services");

    @JsonProperty(value = "services")
    private final Map<String, ServiceConfig> serviceConfigs = new HashMap<>();

    @JsonIgnore
    private final Map<String, Service> services = new HashMap<>();

    private final List<String> builtinServices;
    private final List<String> userServices;

    public ServiceManager() {
        this(true);
    }

    public ServiceManager(boolean loadUserConfigs) {
        try {
            builtinServices = loadConfigs(BUILTIN_CONFIG_DIR);
        } catch (IOException | URISyntaxException ex) {
            throw new RuntimeException("Failed to load default service configs.", ex);
        }
        if (loadUserConfigs) {
            try {
                if (Files.exists(USER_CONFIG_DIR)) {
                    userServices = loadConfigs(USER_CONFIG_DIR);
                    userServices.removeAll(builtinServices);
                } else {
                    userServices = Collections.emptyList();
                }
            } catch (IOException | URISyntaxException ex) {
                throw new RuntimeException("Failed to load user specified service configs.", ex);
            }
        } else {
            userServices = Collections.emptyList();
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

    public ServiceConfig duplicateServiceConfig(String oldServiceName, String newServiceName) {

        if (serviceConfigs.containsKey(newServiceName)) {
            return serviceConfigs.get(newServiceName);
        } else {
            ServiceConfig newServiceConfig;
            if (builtinServices.contains(oldServiceName)) {
                try {
                    Optional<URI> configFile = FileUtils.listFiles(BUILTIN_CONFIG_DIR).stream()
                            .filter(uri -> uri.toString().endsWith("/" + oldServiceName + ".json"))
                            .findFirst();

                    newServiceConfig = objectMapper
                            .readValue(configFile.get().toURL(), ServiceConfig.class);

                } catch (IOException | URISyntaxException | NoSuchElementException ex) {
                    throw new RuntimeException("Failed to load default service config '" + oldServiceName + "'.", ex);
                }
                // Ignore user specified services for now as their files clash with the dataset
                // config files.
                // } else if (userServices.contains(oldServiceName)) {
                // try {
                // newServiceConfig = objectMapper.readValue(
                // USER_CONFIG_DIR.resolve(newServiceName + ".json").toUri().toURL(),
                // ServiceConfig.class);
                // } catch (IOException ex) {
                // throw new RuntimeException("Failed to load user service config '" +
                // oldServiceName + "'.", ex);
                // }
            } else {
                throw new RuntimeException("No service config with name '" + oldServiceName + "'.");
            }

            serviceConfigs.put(newServiceName, newServiceConfig);

            newServiceConfig.setName(newServiceName);

            return newServiceConfig;
        }
    }

    public ServiceConfig getServiceConfig(String serviceName) {
        ServiceConfig serviceConfig = serviceConfigs.get(serviceName);
        if (null == serviceConfig) {
            throw new RuntimeException("No service config loaded with name '" + serviceName + "'.");
        }
        return serviceConfig;
    }

    public <S extends Service> S initialiseService(String stackName, String serviceName) {
        ServiceConfig config = getServiceConfig(serviceName);
        String type = config.getType();

        Class<S> typeClass = AbstractService.getTypeClass(type.toLowerCase());
        if (null == typeClass) {
            throw new IllegalArgumentException("Service '" + serviceName + "' is of type '" + type
                    + "', which does not have a specific class defined.");
        }

        final Service newService;
        try {
            newService = typeClass
                    .getConstructor(String.class, ServiceConfig.class)
                    .newInstance(stackName, config);
        } catch (InstantiationException | IllegalAccessException | IllegalArgumentException
                | InvocationTargetException | NoSuchMethodException | SecurityException ex) {
            throw new IllegalArgumentException("Service '" + serviceName + "', of type '" + type
                    + "', could not be created.", ex);
        }

        if (newService instanceof ContainerService) {
            ContainerService newContainerService = (ContainerService) newService;

            DockerService dockerService = getOrInitialiseService(stackName, StackClient.getContainerEngineName());
            dockerService.doPreStartUpConfiguration(newContainerService);
            if (dockerService.startContainer(newContainerService)) {
                dockerService.doPostStartUpConfiguration(newContainerService);
            }
            dockerService.writeEndpointConfigs(newContainerService);

            if (!NginxService.TYPE.equals(serviceName)) {
                ReverseProxyService reverseProxyService = getOrInitialiseService(stackName, NginxService.TYPE);
                reverseProxyService.addService(newContainerService);
            }
        }

        services.put(serviceName, newService);

        return typeClass.cast(newService);
    }

    <S extends Service> S getService(String otherServiceName) {
        return (S) services.get(otherServiceName);
    }

    <S extends Service> S getOrInitialiseService(String stackName, String otherServiceName) {
        if (!services.containsKey(otherServiceName)) {
            initialiseService(stackName, otherServiceName);
        }
        return getService(otherServiceName);
    }

    public void initialiseAllUserServices(String stackName) {
        userServices.forEach(serviceName -> initialiseService(stackName, serviceName));
    }

}
