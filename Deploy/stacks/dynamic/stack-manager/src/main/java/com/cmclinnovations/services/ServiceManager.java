package com.cmclinnovations.services;

import java.io.IOException;
import java.net.URISyntaxException;
import java.net.URL;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.Map;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;
import java.util.stream.Stream;

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
        } catch (URISyntaxException | IOException ex) {
            throw new RuntimeException("Failed to load default service configs.", ex);
        }
    }

    public void loadConfig(Path path) {
        try {
            serviceConfigs.put(FileUtils.getFileNameWithoutExtension(path),
                    objectMapper.readValue(path.toFile(), ServiceConfig.class));
        } catch (IOException ex) {
            throw new RuntimeException("Failed to read service config from file '" + path + "'.", ex);
        }
    }

    public void loadConfig(URL url) {
        try {
            serviceConfigs.put(FileUtils.getFileNameWithoutExtension(url),
                    objectMapper.readValue(url, ServiceConfig.class));
        } catch (IOException ex) {
            throw new RuntimeException("Failed to read service config from file '" + url + "'.", ex);
        }
    }

    public void loadConfigs(Path configDir) throws IOException {
        try (Stream<Path> stream = Files.list(configDir)) {
            stream.filter(path -> FileUtils.filterOnExtension(path, ".json")).forEach(this::loadConfig);
        }
    }

    public void loadConfigs(URL dirURL) throws URISyntaxException, IOException {

        if (dirURL != null) {
            switch (dirURL.getProtocol()) {
                case "file":
                    // A file path: easy enough
                    loadConfigs(Paths.get(dirURL.toURI()));
                    return;
                case "jar":
                    // A JAR path
                    loadConfigsFromJar(dirURL);
                    return;
                default:
            }
        }

        throw new UnsupportedOperationException(
                "Cannot load config files from URL '" + dirURL + "'. Only 'file' and 'jar' protocols are supported.");
    }

    private void loadConfigsFromJar(URL dirURL) throws IOException {
        // strip out only the JAR file
        String dirPath = dirURL.getPath();
        String jarPath = dirPath.substring(5, dirPath.indexOf("!"));
        String path = dirPath.substring(dirPath.indexOf("!") + 2) + "/";
        try (JarFile jar = new JarFile(jarPath)) {
            Enumeration<JarEntry> entries = jar.entries(); // gives ALL entries in jar
            while (entries.hasMoreElements()) {
                String name = entries.nextElement().getName();
                if (name.startsWith(path)) { // filter according to the path
                    String entry = name.substring(path.length());
                    if (!entry.isEmpty()) {
                        int checkSubdir = entry.indexOf("/");
                        if (checkSubdir == -1) {
                            loadConfig(new URL(dirURL.toString() + "/" + entry));
                        }
                    }
                }
            }
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
