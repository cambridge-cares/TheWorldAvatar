package com.cmclinnovations.stack;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.URL;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import com.cmclinnovations.stack.clients.core.StackClient;
import com.cmclinnovations.stack.services.DockerService;
import com.cmclinnovations.stack.services.ServiceManager;
import com.cmclinnovations.stack.services.config.ServiceConfig;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.github.dockerjava.api.model.ContainerSpec;
import com.github.dockerjava.api.model.Mount;
import com.github.dockerjava.api.model.MountType;

public class Stack {

    private static final String DEFAULT_SERVICES_FILE = "defaults.txt";

    private static final ObjectMapper objectMapper = new ObjectMapper();

    private static final String VOLUME_POPULATOR_SERVICE_NAME = "volume-populator";

    public static Stack create(String stackName, URL hostURL, ServiceManager manager, Path stackConfigDir) {
        try (Stream<Path> stackConfigs = Files.find(stackConfigDir, 1,
                (path, basicFileAttributes) -> basicFileAttributes.isRegularFile())) {
            Optional<Path> stackSpecificConfig = stackConfigs
                    .filter(path -> path.getFileName().toString().matches("^" + stackName + ".json$"))
                    .findAny();
            if (stackSpecificConfig.isPresent()) {
                return createFromConfig(stackName, hostURL, manager, stackSpecificConfig.get());
            } else {
                return createDefault(stackName, hostURL, manager);
            }
        } catch (IOException ex) {
            throw new RuntimeException("Error occurred whilst looking for stack config files.", ex);
        }
    }

    private static Stack createDefault(String stackName, URL hostURL, ServiceManager manager) {
        return new Stack(stackName, hostURL, manager);
    }

    private static Stack createFromConfig(String stackName, URL hostURL, ServiceManager manager,
            Path stackSpecificConfig) {
        try {
            StackConfig stackConfig = objectMapper.readValue(stackSpecificConfig.toFile(),
                    StackConfig.class);
            return new Stack(stackName, hostURL, manager, stackConfig);
        } catch (IOException ex) {
            throw new RuntimeException(
                    "Error occurred whilst reading stack config file'" + stackSpecificConfig + "'.", ex);
        }
    }

    private final String name;

    private final URL hostURL;

    private final ServiceManager manager;

    private final StackConfig config;

    private Stack(String name, URL hostURL, ServiceManager manager) {
        this(name, hostURL, manager, null);
    }

    private Stack(String name, URL hostURL, ServiceManager manager, StackConfig config) {
        this.name = name;
        this.hostURL = hostURL;

        this.manager = manager;

        this.config = config;
    }

    public void initialiseServices() {
        List<String> defaultServices = getDefaultServicesNames();

        manager.<DockerService>initialiseService(name, StackClient.getContainerEngineName()).initialise();

        // Check to see if services have been specified through a config file
        if (null == config) {
            // Load all defaults
            defaultServices.forEach(serviceName -> manager.initialiseService(name, serviceName));

            // Load all user supplied services
            manager.initialiseAllUserServices(name);
        } else {
            handleUserSuppliedData(defaultServices);

            List<String> selectedServices = calculateSelectedServicesFromConfig(defaultServices);

            // Initialise all of the selected services
            selectedServices.forEach(serviceName -> manager.initialiseService(name, serviceName));
        }
    }

    private List<String> getDefaultServicesNames() {
        try (InputStream is = Stack.class.getResourceAsStream(DEFAULT_SERVICES_FILE);
                InputStreamReader isr = new InputStreamReader(is);
                BufferedReader reader = new BufferedReader(isr)) {
            return reader.lines().collect(Collectors.toList());
        } catch (IOException ex) {
            throw new RuntimeException(
                    "failed to read in list of default services from the file '" + DEFAULT_SERVICES_FILE + "'.", ex);
        }
    }

    private List<String> calculateSelectedServicesFromConfig(List<String> defaultServices) {
        // Start with the defaults
        List<String> selectedServices = new ArrayList<>(defaultServices);

        // Check that none of the defaults have been explicitly included.
        String explicitDefaults = config.getIncludedServices().stream()
                .filter(defaultServices::contains)
                .collect(Collectors.joining(", "))
                // Replace the last comma delimiter with "and"
                .replaceFirst(", ([^, ]+)$", " and $1");
        if (!explicitDefaults.isEmpty()) {
            throw new IllegalStateException("Default service(s), " + explicitDefaults
                    + ", explicitly included by user. Please remove them from the \"includes\" list in the stack config file.");
        }

        // Add user specified services
        selectedServices.addAll(config.getIncludedServices());
        // Remove any excluded services (default and user specified)
        selectedServices.removeAll(config.getExcludedServices());
        return selectedServices;
    }

    private void handleUserSuppliedData(List<String> selectedServices) {

        Map<String, String> volumes = config.getVolumes();
        if (!volumes.isEmpty()) {
            if (!selectedServices.contains(VOLUME_POPULATOR_SERVICE_NAME)) {
                selectedServices.add(0, VOLUME_POPULATOR_SERVICE_NAME);
            }

            ServiceConfig volumePopulator = manager.getServiceConfig(VOLUME_POPULATOR_SERVICE_NAME);

            ContainerSpec containerSpec = volumePopulator.getContainerSpec();

            List<Mount> existingMounts = containerSpec.getMounts();
            final List<Mount> mounts;
            if (null == existingMounts) {
                mounts = new ArrayList<>();
                containerSpec.withMounts(mounts);
            } else {
                mounts = existingMounts;
            }

            List<String> envs = containerSpec.getEnv();
            Map<String, String> envsMap = envs.stream().map(env -> env.split("=", 2))
                    .collect(Collectors.toMap(envA -> envA[0], envA -> envA[1]));
            Path internalHostDir = Path.of(envsMap.get("HOST_DIR"));
            Path internalVolumeDir = Path.of(envsMap.get("VOLUME_DIR"));

            Path dataDir = StackClient.getAbsDataPath();

            volumes.forEach((volumeName, hostDir) -> {
                mounts.add(new Mount()
                        .withType(MountType.BIND)
                        .withSource(dataDir.resolve(hostDir).toString())
                        .withTarget(internalHostDir.resolve(volumeName).toString()));
                mounts.add(new Mount()
                        .withType(MountType.VOLUME)
                        .withSource(volumeName)
                        .withTarget(internalVolumeDir.resolve(volumeName).toString()));
            });
        }
    }
}
