package com.cmclinnovations.stack.services;

import java.io.File;
import java.io.IOException;
import java.net.URI;
import java.nio.file.FileVisitResult;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.SimpleFileVisitor;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import com.cmclinnovations.stack.clients.core.StackClient;
import com.cmclinnovations.stack.clients.docker.DockerClient;
import com.cmclinnovations.stack.services.config.Connection;
import com.cmclinnovations.stack.services.config.ServiceConfig;
import com.github.dockerjava.api.command.CreateNetworkCmd;
import com.github.dockerjava.api.command.CreateServiceCmd;
import com.github.dockerjava.api.command.CreateServiceResponse;
import com.github.dockerjava.api.command.InfoCmd;
import com.github.dockerjava.api.command.InitializeSwarmCmd;
import com.github.dockerjava.api.command.ListNetworksCmd;
import com.github.dockerjava.api.command.ListServicesCmd;
import com.github.dockerjava.api.command.ListTasksCmd;
import com.github.dockerjava.api.command.PullImageCmd;
import com.github.dockerjava.api.command.PullImageResultCallback;
import com.github.dockerjava.api.command.RemoveServiceCmd;
import com.github.dockerjava.api.model.Config;
import com.github.dockerjava.api.model.Container;
import com.github.dockerjava.api.model.ContainerSpec;
import com.github.dockerjava.api.model.ContainerSpecConfig;
import com.github.dockerjava.api.model.ContainerSpecFile;
import com.github.dockerjava.api.model.ContainerSpecSecret;
import com.github.dockerjava.api.model.LocalNodeState;
import com.github.dockerjava.api.model.Mount;
import com.github.dockerjava.api.model.MountType;
import com.github.dockerjava.api.model.Network;
import com.github.dockerjava.api.model.NetworkAttachmentConfig;
import com.github.dockerjava.api.model.Secret;
import com.github.dockerjava.api.model.Service;
import com.github.dockerjava.api.model.ServiceRestartCondition;
import com.github.dockerjava.api.model.ServiceRestartPolicy;
import com.github.dockerjava.api.model.ServiceSpec;
import com.github.dockerjava.api.model.SwarmInfo;
import com.github.dockerjava.api.model.SwarmSpec;
import com.github.dockerjava.api.model.Task;
import com.github.dockerjava.api.model.TaskState;
import com.github.dockerjava.api.model.TaskStatus;
import com.github.dockerjava.api.model.VolumeOptions;

public class DockerService extends AbstractService
        implements ContainerManagerService<DockerClient> {

    // External path to socket on host
    private static final String API_SOCK = "API_SOCK";
    // Internal path to socket in containers
    protected static final String DOCKER_SOCKET_PATH = "/var/run/docker.sock";

    public static final String TYPE = "docker";

    protected final DockerClient dockerClient;

    protected Network network;

    public DockerService(String stackName, ServiceConfig config) {
        super(config);

        Connection endpoint = getEndpoint("dockerHost");
        URI dockerUri;
        if (null == endpoint) {
            dockerUri = null;
        } else {
            dockerUri = endpoint.getUri();
        }
        dockerClient = initClient(dockerUri);

        createNetwork(stackName);
    }

    public DockerClient initClient(URI dockerUri) {
        return new DockerClient(dockerUri);
    }

    @Override
    public DockerClient getClient() {
        return dockerClient;
    }

    public void initialise() {
        startDockerSwarm();

        addStackSecrets();

        addStackConfigs();
    }

    private void startDockerSwarm() {
        try (InfoCmd infoCmd = dockerClient.getInternalClient().infoCmd()) {

            SwarmInfo swarmInfo = infoCmd.exec().getSwarm();
            if (null == swarmInfo) {
                throw new RuntimeException("SwarmInfo returned by Docker infoCMD is 'null'.");
            }

            LocalNodeState nodeState = swarmInfo.getLocalNodeState();
            if (null == nodeState) {
                throw new RuntimeException("LocalNodeState returned by Docker infoCMD is 'null'.");
            }

            switch (nodeState) {
                case INACTIVE:
                case PENDING:
                    try (InitializeSwarmCmd initializeSwarmCmd = dockerClient.getInternalClient()
                            .initializeSwarmCmd(new SwarmSpec())) {
                        initializeSwarmCmd.exec();
                    }
                    break;
                case ACTIVE:
                    break;
                default:
                    throw new IllegalStateException("Docker swarm is in a bad state '" + nodeState + "'.");
            }
        }
    }

    protected void addStackConfigs() {
        List<Config> existingStackConfigs = dockerClient.getConfigs();

        try {
            Files.walkFileTree(Path.of("/inputs/config"), new SimpleFileVisitor<Path>() {

                @Override
                public FileVisitResult visitFile(Path file, BasicFileAttributes attrs) throws IOException {
                    try {
                        if (Files.isReadable(file) && !file.getFileName().toString().startsWith(".git")) {
                            String configName = file.getFileName().toString();
                            Optional<Config> currentConfig = dockerClient.getConfig(existingStackConfigs, configName);
                            if (currentConfig.isEmpty()) {
                                try (Stream<String> lines = Files.lines(file)) {
                                    String data = lines.collect(Collectors.joining("\n"));
                                    dockerClient.addConfig(configName, data);
                                }
                            } else {
                                existingStackConfigs.remove(currentConfig.get());
                            }
                        }
                        return FileVisitResult.CONTINUE;
                    } catch (IOException ex) {
                        throw new IOException("Failed to load config file '" + file + "'.", ex);
                    }
                }
            });
            for (Config oldConfig : existingStackConfigs) {
                dockerClient.removeConfig(oldConfig);
            }
        } catch (IOException ex) {
            throw new RuntimeException("Failed to load configs.", ex);
        }
    }

    public void addStackSecrets() {
        List<Secret> existingStackSecrets = dockerClient.getSecrets();

        for (File secretFile : Path.of("/run/secrets").toFile()
                .listFiles(file -> file.isFile() && !file.getName().startsWith(".git"))) {
            try (Stream<String> lines = Files.lines(secretFile.toPath())) {
                String data = lines.collect(Collectors.joining("\n"));
                String secretName = secretFile.getName();

                Optional<Secret> currentSecret = dockerClient.getSecret(existingStackSecrets, secretName);
                if (currentSecret.isEmpty()) {
                    dockerClient.addSecret(secretName, data);
                } else {
                    existingStackSecrets.remove(currentSecret.get());
                }
            } catch (IOException ex) {
                throw new RuntimeException("Failed to load secret file '" + secretFile.getAbsolutePath() + "'.",
                        ex);
            }
        }

        for (Secret oldSecret : existingStackSecrets) {
            dockerClient.removeSecret(oldSecret);
        }
    }

    protected void createNetwork(String name) {
        Optional<Network> potentialNetwork;
        try (ListNetworksCmd listNetworksCmd = dockerClient.getInternalClient().listNetworksCmd()) {
            potentialNetwork = listNetworksCmd.withNameFilter(name).exec().stream().findAny();
        }
        if (potentialNetwork.isEmpty()) {
            try (CreateNetworkCmd createNetworkCmd = dockerClient.getInternalClient().createNetworkCmd()) {
                createNetworkCmd.withName(name)
                        .withAttachable(true)
                        .withCheckDuplicate(true)
                        .withLabels(StackClient.getStackNameLabelMap())
                        .exec();
                try (ListNetworksCmd listNetworksCmd = dockerClient.getInternalClient().listNetworksCmd()) {
                    potentialNetwork = listNetworksCmd.withNameFilter(name).exec().stream().findAny();
                }
            }
        }
        potentialNetwork.ifPresent(nw -> this.network = nw);
    }

    protected Optional<Service> getSwarmService(ContainerService service) {
        try (ListServicesCmd listServicesCmd = dockerClient.getInternalClient().listServicesCmd()) {
            return listServicesCmd.withNameFilter(List.of(service.getContainerName()))
                    .exec().stream().findAny();
        }
    }

    protected Optional<Container> getContainerFromID(String containerId) {
        return dockerClient.getContainerFromID(containerId);
    }

    public void doPreStartUpConfiguration(ContainerService service) {
        service.setDockerClient(dockerClient);
        service.doPreStartUpConfiguration();
    }

    public void doPostStartUpConfiguration(ContainerService service) {
        service.doPostStartUpConfiguration();
    }

    public void writeEndpointConfigs(ContainerService service){
        service.writeEndpointConfigs();
    }

    public boolean startContainer(ContainerService service) {

        Optional<Container> container = dockerClient
                .getContainer(StackClient.removeStackName(service.getContainerName()));

        boolean notAlreadyRunning = container.isEmpty() || !container.get().getState().equalsIgnoreCase("running");
        if (notAlreadyRunning) {
            // No container matching that config

            pullImage(service);

            container = configureContainerWrapper(service);
        }

        final String containerId;

        if (container.isPresent()) {
            // Get required details of the existing/new container
            containerId = container.get().getId();
        } else {
            throw new RuntimeException("Failed to start container for service with name '" + service.getName() + "'.");
        }

        service.setContainerId(containerId);

        return notAlreadyRunning;
    }

    protected Optional<Container> configureContainerWrapper(ContainerService service) {
        Optional<Container> container;
        removeSwarmService(service);

        container = startSwarmService(service);
        return container;
    }

    private Optional<Container> startSwarmService(ContainerService service) {

        ServiceSpec serviceSpec = configureServiceSpec(service);

        try (CreateServiceCmd createServiceCmd = dockerClient.getInternalClient().createServiceCmd(serviceSpec)) {
            CreateServiceResponse createServiceResponse = createServiceCmd.exec();

            return getContainerIfCreated(service.getContainerName());
        }
    }

    protected Optional<Container> getContainerIfCreated(String containerName) {
        TaskStatus taskStatus = new TaskStatus();
        TaskState taskState = null;
        do {
            try (ListTasksCmd listTasksCmd = dockerClient.getInternalClient().listTasksCmd()) {
                Optional<Task> task = listTasksCmd.withServiceFilter(containerName)
                        .exec().stream().findFirst();
                if (task.isPresent()) {
                    taskStatus = task.get().getStatus();
                    taskState = taskStatus.getState();
                }
            }
        } while (null == taskState || TaskState.RUNNING.compareTo(taskState) > 0);

        switch (taskState) {
            case RUNNING:
            case COMPLETE:
                String containerId = taskStatus.getContainerStatus().getContainerID();
                return getContainerFromID(containerId);
            default:
                String errMessage = taskStatus.getErr();
                if (null != errMessage) {
                    errMessage = taskStatus.getMessage();
                }
                throw new RuntimeException("Failed to start service '" + containerName
                        + "'. Error message is:\n" + errMessage);
        }
    }

    private void removeSwarmService(ContainerService service) {
        Optional<Service> swarmService = getSwarmService(service);

        if (swarmService.isPresent()) {
            try (RemoveServiceCmd removeServiceCmd = dockerClient.getInternalClient()
                    .removeServiceCmd(swarmService.get().getId())) {
                removeServiceCmd.exec();
            }
        }
    }

    protected ServiceSpec configureServiceSpec(ContainerService service) {

        ServiceSpec serviceSpec = service.getServiceSpec()
                .withName(service.getContainerName())
                .withLabels(StackClient.getStackNameLabelMap());
        service.getTaskTemplate()
                .withRestartPolicy(new ServiceRestartPolicy().withCondition(ServiceRestartCondition.NONE))
                .withNetworks(List.of(new NetworkAttachmentConfig().withTarget(network.getId())));
        ContainerSpec containerSpec = service.getContainerSpec()
                .withLabels(StackClient.getStackNameLabelMap())
                .withHostname(service.getName());

        interpolateEnvironmentVariables(containerSpec);

        interpolateVolumes(containerSpec);

        interpolateConfigs(containerSpec);

        interpolateSecrets(containerSpec);

        return serviceSpec;
    }

    protected void interpolateEnvironmentVariables(ContainerSpec containerSpec) {
        List<String> env = new ArrayList<>(containerSpec.getEnv());
        env.add(API_SOCK + "=" + System.getenv(API_SOCK));
        env.add(StackClient.EXECUTABLE_KEY + "=" + System.getenv(StackClient.EXECUTABLE_KEY));
        containerSpec.withEnv(env);
    }

    protected void interpolateVolumes(ContainerSpec containerSpec) {
        List<Mount> mounts = addDefaultMounts(containerSpec);

        mounts.forEach(this::interpolateMount);
    }

    private List<Mount> addDefaultMounts(ContainerSpec containerSpec) {
        List<Mount> mounts = containerSpec.getMounts();

        // Ensure that "mounts" is not "null"
        if (null == mounts) {
            mounts = new ArrayList<>();
            containerSpec.withMounts(mounts);
        }

        // Add stack scratch volume to all containers
        mounts.add(new Mount()
                .withType(MountType.VOLUME)
                .withSource("scratch")
                .withTarget(StackClient.SCRATCH_DIR));

        // Add the Docker API socket as a bind mount
        // This is required for a container to make Docker API calls
        Mount dockerSocketMount = new Mount()
                .withType(MountType.BIND)
                .withSource(System.getenv(API_SOCK))
                .withTarget(DOCKER_SOCKET_PATH);
        if (!mounts.contains(dockerSocketMount)) {
            mounts.add(dockerSocketMount);
        }
        return mounts;
    }

    private void interpolateMount(Mount mount) {
        // The default mount type is "bind" so set it explicitly if it is missing
        MountType mountType = mount.getType();
        if (null == mountType) {
            mountType = MountType.BIND;
            mount.withType(mountType);
        }

        switch (mountType) {
            case BIND:
                // Handle relative source paths for bind mounts
                String bindSource = mount.getSource();
                if (null != bindSource && !bindSource.startsWith("/")) {
                    mount.withSource(StackClient.getAbsDataPath().resolve(bindSource).toString());
                }
                break;
            case VOLUME:
                // Append the stack name to the volume name
                mount.withSource(StackClient.prependStackName(mount.getSource()));

                // Add stack specific labels
                VolumeOptions volumeOptions = mount.getVolumeOptions();
                if (null == volumeOptions) {
                    volumeOptions = new VolumeOptions().withLabels(StackClient.getStackNameLabelMap());
                    mount.withVolumeOptions(volumeOptions);
                } else {
                    Map<String, String> labels = volumeOptions.getLabels();
                    if (null == labels) {
                        volumeOptions.withLabels(StackClient.getStackNameLabelMap());
                    } else {
                        labels.putAll(StackClient.getStackNameLabelMap());
                    }
                }
                break;
            default:
        }
    }

    protected void interpolateConfigs(ContainerSpec containerSpec) {
        List<ContainerSpecConfig> containerSpecConfigs = containerSpec.getConfigs();
        if (null != containerSpecConfigs && !containerSpecConfigs.isEmpty()) {
            List<Config> configs = dockerClient.getConfigs();
            for (ContainerSpecConfig containerSpecConfig : containerSpecConfigs) {
                interpolateConfigFileSpec(containerSpecConfig);
                interpolateConfigId(configs, containerSpecConfig);
                // The stack name needs to be prepended to the name after the file spec is
                // interpolated so that the file name is not modified.
                containerSpecConfig.withConfigName(StackClient.prependStackName(containerSpecConfig.getConfigName()));
            }
        }
    }

    protected void interpolateConfigFileSpec(ContainerSpecConfig containerSpecConfig) {
        ContainerSpecFile configFileSpec = containerSpecConfig.getFile();
        if (null == configFileSpec) {
            configFileSpec = new ContainerSpecFile();
            containerSpecConfig.withFile(configFileSpec);
        }
        if (null == configFileSpec.getName()) {
            configFileSpec.withName(containerSpecConfig.getConfigName());
        }
        if (null == configFileSpec.getGid()) {
            configFileSpec.withGid("0");
        }
        if (null == configFileSpec.getUid()) {
            configFileSpec.withUid("0");
        }
        if (null == configFileSpec.getMode()) {
            configFileSpec.withMode(0444l);
        }
    }

    protected void interpolateConfigId(List<Config> configs, ContainerSpecConfig containerSpecConfig) {
        if (null == containerSpecConfig.getConfigID()) {
            Optional<String> configID = dockerClient.getConfig(configs, containerSpecConfig.getConfigName())
                    .map(Config::getId);
            if (configID.isPresent()) {
                containerSpecConfig.withConfigID(configID.get());
            } else {
                throw new RuntimeException("Failed to find Config with name '"
                        + containerSpecConfig.getConfigName() + "'.");
            }
        }
    }

    protected void interpolateSecrets(ContainerSpec containerSpec) {
        List<ContainerSpecSecret> containerSpecSecrets = containerSpec.getSecrets();
        if (null != containerSpecSecrets && !containerSpecSecrets.isEmpty()) {
            List<Secret> secrets = dockerClient.getSecrets();
            for (ContainerSpecSecret containerSpecSecret : containerSpecSecrets) {
                interpolateSecretFileSpec(containerSpecSecret);
                interpolateSecretId(secrets, containerSpecSecret);
                // The stack name needs to be prepended to the name after the file spec is
                // interpolated so that the file name is not modified.
                containerSpecSecret.withSecretName(StackClient.prependStackName(containerSpecSecret.getSecretName()));
            }
        }
    }

    protected void interpolateSecretFileSpec(ContainerSpecSecret containerSpecSecret) {
        ContainerSpecFile secretFileSpec = containerSpecSecret.getFile();
        if (null == secretFileSpec) {
            secretFileSpec = new ContainerSpecFile();
            containerSpecSecret.withFile(secretFileSpec);
        }
        if (null == secretFileSpec.getName()) {
            secretFileSpec.withName(containerSpecSecret.getSecretName());
        }
        if (null == secretFileSpec.getGid()) {
            secretFileSpec.withGid("0");
        }
        if (null == secretFileSpec.getUid()) {
            secretFileSpec.withUid("0");
        }
        if (null == secretFileSpec.getMode()) {
            secretFileSpec.withMode(0444l);
        }
    }

    protected void interpolateSecretId(List<Secret> secrets, ContainerSpecSecret containerSpecSecret) {
        if (null == containerSpecSecret.getSecretId()) {
            Optional<String> secretID = dockerClient.getSecret(secrets, containerSpecSecret.getSecretName())
                    .map(Secret::getId);
            if (secretID.isPresent()) {
                containerSpecSecret.withSecretId(secretID.get());
            } else {
                throw new RuntimeException("Failed to find Secret with name '"
                        + containerSpecSecret.getSecretName() + "''.");
            }
        }
    }

    protected void pullImage(ContainerService service) {
        String image = service.getImage();
        if(!image.contains(":")){
            throw new RuntimeException("Docker image '"+ image +"' must include a version.");
        }
        if (dockerClient.getInternalClient().listImagesCmd().withReferenceFilter(image).exec().isEmpty()) {
            // No image with the requested image ID, so try to pull image
            try (PullImageCmd pullImageCmd = dockerClient.getInternalClient().pullImageCmd(image)) {
                pullImageCmd
                        .exec(new PullImageResultCallback())
                        .awaitCompletion();
            } catch (InterruptedException ex) {
                Thread.currentThread().interrupt();
                throw new RuntimeException("Docker image pull command interupted", ex);
            }
        }
    }

}
