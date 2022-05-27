package com.cmclinnovations.services;

import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.nio.file.Path;
import java.util.HashMap;
import java.util.Map;

import com.cmclinnovations.FileUtils;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.ObjectMapper;

public class ServiceManager {

    private static final ObjectMapper objectMapper = new ObjectMapper();

    @JsonProperty(value = "services")
    private final Map<String, ServiceConfig> serviceConfigs = new HashMap<>();

    @JsonIgnore
    private final Map<String, Service> services = new HashMap<>();

    public ServiceManager() throws IOException, URISyntaxException {
        try {
            URL url = ServiceManager.class.getResource("defaults");
            loadConfigs(url);
        } catch (IOException ex) {
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

    public void loadConfigs(Path configDir) throws IOException {
        loadConfigs(configDir.toUri().toURL());
    }

    public void loadConfigs(URL dirURL) throws IOException {
        for (URI uri : FileUtils.listFiles(dirURL, ".json")) {
            loadConfig(uri);
        }
    }

    public ServiceConfig getServiceConfig(String serviceName) {
        return serviceConfigs.get(serviceName);
    }

    public <S extends Service> S initialiseService(String stackName, String serviceName)
            throws URISyntaxException, IOException {
        ServiceConfig config = serviceConfigs.get(serviceName);
        String type = config.getType();

        final Service newService;
        switch (type.toLowerCase()) {
            case DockerService.TYPE:
                newService = new DockerService(this, config);
                ((DockerService) newService).createNetwork(stackName);
                break;
            case NginxService.TYPE:
                newService = new NginxService(stackName, this, config);
                break;
            case PostGISService.TYPE:
                newService = new PostGISService(stackName, this, config);
                break;
            default:
                if (null != config.getImage()) {
                    newService = new ContainerService(stackName, this, config);
                    break;
                } else {
                    throw new IllegalArgumentException("Service '" + serviceName + "' is of type '" + type
                            + "', which does not have a specific class defined, and no Docker 'image' was specified.");
                }
        }

        if (newService instanceof ContainerService) {
            this.<DockerService>getService("docker").startContainer((ContainerService) newService);
        }

        services.put(serviceName, newService);

        return (S) newService;
    }

    <S extends Service> S getService(String otherServiceName) {
        return (S) services.get(otherServiceName);
    }

}
