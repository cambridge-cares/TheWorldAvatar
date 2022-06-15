package com.cmclinnovations.services;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.nio.file.FileVisitResult;
import java.nio.file.FileVisitor;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Optional;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.apache.commons.compress.archivers.tar.TarArchiveEntry;
import org.apache.commons.compress.archivers.tar.TarArchiveOutputStream;
import org.apache.commons.compress.compressors.gzip.GzipCompressorOutputStream;

import com.cmclinnovations.services.config.ServiceConfig;
import com.github.dockerjava.api.DockerClient;
import com.github.dockerjava.api.async.ResultCallback;
import com.github.dockerjava.api.async.ResultCallback.Adapter;
import com.github.dockerjava.api.command.ConnectToNetworkCmd;
import com.github.dockerjava.api.command.CopyArchiveToContainerCmd;
import com.github.dockerjava.api.command.CreateConfigCmd;
import com.github.dockerjava.api.command.CreateNetworkCmd;
import com.github.dockerjava.api.command.CreateSecretCmd;
import com.github.dockerjava.api.command.CreateServiceCmd;
import com.github.dockerjava.api.command.CreateServiceResponse;
import com.github.dockerjava.api.command.ExecCreateCmd;
import com.github.dockerjava.api.command.ExecStartCmd;
import com.github.dockerjava.api.command.InfoCmd;
import com.github.dockerjava.api.command.InitializeSwarmCmd;
import com.github.dockerjava.api.command.InspectExecCmd;
import com.github.dockerjava.api.command.InspectExecResponse;
import com.github.dockerjava.api.command.InspectNetworkCmd;
import com.github.dockerjava.api.command.ListConfigsCmd;
import com.github.dockerjava.api.command.ListContainersCmd;
import com.github.dockerjava.api.command.ListNetworksCmd;
import com.github.dockerjava.api.command.ListSecretsCmd;
import com.github.dockerjava.api.command.ListServicesCmd;
import com.github.dockerjava.api.command.ListTasksCmd;
import com.github.dockerjava.api.command.PullImageCmd;
import com.github.dockerjava.api.command.PullImageResultCallback;
import com.github.dockerjava.api.command.RemoveConfigCmd;
import com.github.dockerjava.api.command.RemoveSecretCmd;
import com.github.dockerjava.api.command.RemoveServiceCmd;
import com.github.dockerjava.api.command.StartContainerCmd;
import com.github.dockerjava.api.model.Config;
import com.github.dockerjava.api.model.Container;
import com.github.dockerjava.api.model.ContainerSpec;
import com.github.dockerjava.api.model.ContainerSpecSecret;
import com.github.dockerjava.api.model.LocalNodeState;
import com.github.dockerjava.api.model.Network;
import com.github.dockerjava.api.model.Secret;
import com.github.dockerjava.api.model.SecretSpec;
import com.github.dockerjava.api.model.Service;
import com.github.dockerjava.api.model.ServiceRestartCondition;
import com.github.dockerjava.api.model.ServiceRestartPolicy;
import com.github.dockerjava.api.model.ServiceSpec;
import com.github.dockerjava.api.model.SwarmInfo;
import com.github.dockerjava.api.model.SwarmSpec;
import com.github.dockerjava.api.model.Task;
import com.github.dockerjava.api.model.TaskState;
import com.github.dockerjava.api.model.TaskStatus;
import com.github.dockerjava.core.DefaultDockerClientConfig;
import com.github.dockerjava.core.DefaultDockerClientConfig.Builder;
import com.github.dockerjava.core.DockerClientBuilder;
import com.github.dockerjava.core.DockerClientConfig;
import com.github.dockerjava.core.RemoteApiVersion;
import com.github.dockerjava.httpclient5.ApacheDockerHttpClient;
import com.github.dockerjava.transport.DockerHttpClient;

public final class DockerService extends AbstractService {

    public static final String TYPE = "docker";

    private final DockerClient dockerClient;
    private Network network;

    private Map<String, String> stackNameLabelMap;

    public DockerService(String stackName, ServiceManager serviceManager, ServiceConfig config) {
        super(serviceManager, config);

        Builder dockerConfigBuilder = DefaultDockerClientConfig.createDefaultConfigBuilder();

        URI endpoint = getEndpoint("dockerHost").getUri();
        if (null != endpoint) {
            dockerConfigBuilder.withDockerHost(endpoint.toString());
            // TODO need to set up TLS so that the unsecured Docker port "2375" doesn't need
            // to be opened.
            // dockerConfigBuilder.withDockerTlsVerify(true);
        }

        DockerClientConfig dockerConfig = dockerConfigBuilder
                .withApiVersion(RemoteApiVersion.VERSION_1_40)
                .build();

        DockerHttpClient httpClient = new ApacheDockerHttpClient.Builder()
                .dockerHost(dockerConfig.getDockerHost())
                .sslConfig(dockerConfig.getSSLConfig())
                .build();

        dockerClient = DockerClientBuilder.getInstance(dockerConfig).withDockerHttpClient(httpClient).build();

        stackNameLabelMap = Collections.singletonMap("stack", stackName);

        startDockerSwarm();

        addStackSecrets();

        clearStackConfigs();

        addStackConfigs();

        createNetwork(stackName);
    }

    private void startDockerSwarm() {
        try (InfoCmd infoCmd = dockerClient.infoCmd()) {

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
                    try (InitializeSwarmCmd initializeSwarmCmd = dockerClient
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

    private void addStackSecrets() {
        try (ListSecretsCmd listSecretsCmd = dockerClient.listSecretsCmd()) {
            List<Secret> existingStackSecrets = listSecretsCmd
                    .withLabelFilter(stackNameLabelMap).exec();

            for (File secretFile : Path.of("/run/secrets").toFile()
                    .listFiles(file -> file.isFile() && !file.getName().startsWith(".git"))) {
                try (Stream<String> lines = Files.lines(secretFile.toPath())) {
                    String data = lines.collect(Collectors.joining("\n"));
                    SecretSpec secretSpec = new SecretSpec()
                            .withName(secretFile.getName())
                            .withData(data)
                            .withLabels(stackNameLabelMap);
                    Optional<Secret> currentSecret = existingStackSecrets.stream().filter(
                            existingSecret -> existingSecret.getSpec().getName().equals(secretFile.getName()))
                            .findFirst();
                    if (currentSecret.isEmpty()) {
                        try (CreateSecretCmd createSecretCmd = dockerClient.createSecretCmd(secretSpec)) {
                            createSecretCmd.exec();
                        }
                    } else {
                        existingStackSecrets.remove(currentSecret.get());
                    }

                    for (Secret oldSecret : existingStackSecrets) {
                        try (RemoveSecretCmd removeSecretCmd = dockerClient.removeSecretCmd(oldSecret.getId())) {
                            removeSecretCmd.exec();
                        }
                    }
                } catch (IOException ex) {
                    throw new RuntimeException("Failed to load secret file '" + secretFile.getAbsolutePath() + "'.",
                            ex);
                }
            }
        }
    }

    private void clearStackConfigs() {
        try (ListConfigsCmd listConfigsCmd = dockerClient.listConfigsCmd()) {
            List<Config> stackConfigs = listConfigsCmd.withFilters(
                    stackNameLabelMap.entrySet().stream()
                            .collect(Collectors.toMap(entry -> "label",
                                    entry -> List.of(entry.getKey() + "=" + entry.getValue()))))
                    .exec();
            for (Config config : stackConfigs) {
                try (RemoveConfigCmd removeConfigCmd = dockerClient.removeConfigCmd(config.getId())) {
                    removeConfigCmd.exec();
                }
            }
        }
    }

    private void addStackConfigs() {
        try {
            Files.walkFileTree(Path.of("/inputs/config"), new FileVisitor<Path>() {

                @Override
                public FileVisitResult preVisitDirectory(Path dir, BasicFileAttributes attrs) throws IOException {
                    return FileVisitResult.CONTINUE;
                }

                @Override
                public FileVisitResult visitFile(Path file, BasicFileAttributes attrs) {
                    try {
                        if (Files.isReadable(file) && !file.getFileName().toString().startsWith(".git")) {
                            try (Stream<String> lines = Files.lines(file)) {
                                String data = lines.collect(Collectors.joining("\n"));
                                try (CreateConfigCmd createConfigCmd = dockerClient.createConfigCmd()) {
                                    createConfigCmd
                                            .withName(file.getFileName().toString())
                                            .withData(data.getBytes())
                                            .withLabels(stackNameLabelMap)
                                            .exec();
                                }
                            }
                        }
                        return FileVisitResult.CONTINUE;
                    } catch (IOException ex) {
                        throw new RuntimeException("Failed to load config file '" + file + "'.", ex);
                    }
                }

                @Override
                public FileVisitResult visitFileFailed(Path file, IOException exc) throws IOException {
                    return FileVisitResult.CONTINUE;
                }

                @Override
                public FileVisitResult postVisitDirectory(Path dir, IOException exc) throws IOException {
                    return FileVisitResult.CONTINUE;
                }

            });
        } catch (IOException ex) {
            throw new RuntimeException("Failed to load configs.", ex);
        }
    }

    private void createNetwork(String name) {
        Optional<Network> potentialNetwork;
        try (ListNetworksCmd listNetworksCmd = dockerClient.listNetworksCmd()) {
            potentialNetwork = listNetworksCmd.withNameFilter(name).exec().stream().findAny();
        }
        if (potentialNetwork.isEmpty()) {
            try (CreateNetworkCmd createNetworkCmd = dockerClient.createNetworkCmd()) {
                createNetworkCmd.withName(name).withAttachable(true).withCheckDuplicate(true).exec();
                try (ListNetworksCmd listNetworksCmd = dockerClient.listNetworksCmd()) {
                    potentialNetwork = listNetworksCmd.withNameFilter(name).exec().stream().findAny();
                }
            }
        }
        potentialNetwork.ifPresent(nw -> this.network = nw);
    }

    private Optional<Service> getSwarmService(ContainerService service) {
        try (ListServicesCmd listServicesCmd = dockerClient.listServicesCmd()) {
            return listServicesCmd.withNameFilter(List.of(service.getContainerName()))
                    .exec().stream().findAny();
        }
    }

    private Optional<Container> getContainer(ContainerService service) {
        try (ListContainersCmd listContainersCmd = dockerClient.listContainersCmd()) {
            // Setting "showAll" to "true" ensures non-running containers are also returned
            return listContainersCmd.withNameFilter(List.of(service.getContainerName()))
                    .withShowAll(true).exec()
                    .stream().findAny();
        }
    }

    private Optional<Container> getContainer(String containerId) {
        try (ListContainersCmd listContainersCmd = dockerClient.listContainersCmd()) {
            // Setting "showAll" to "true" ensures non-running containers are also returned
            return listContainersCmd.withIdFilter(List.of(containerId))
                    .withShowAll(true).exec()
                    .stream().findAny();
        }
    }

    public boolean isContainerUp(ContainerService service) {
        try (ListContainersCmd listContainersCmd = dockerClient.listContainersCmd()) {
            // Don't need to filter for "running" state as this is the default setting
            return !listContainersCmd.withNameFilter(List.of(service.getContainerName())).exec().isEmpty();
        }
    }

    public String getContainerID(ContainerService service) {
        return getContainer(service).map(Container::getId).orElseThrow();
    }

    public void startContainer(ContainerService service) {

        Optional<Container> container = getContainer(service);

        if (container.isEmpty()) {
            // No container matching that config

            pullImage(service);

            removeSwarmService(service);

            container = startSwarmService(service);
        }

        final String containerId;
        final String containerState;

        if (container.isPresent()) {
            // Get required details of the existing/new container
            containerId = container.get().getId();
            containerState = container.get().getState();
        } else {
            throw new RuntimeException("Failed to start container for service with name '" + service.getName() + "'.");
        }

        switch (containerState) {
            case "running":
                // The container is already running, all is fine.
                break;
            case "created":
            case "exited":
                // The container is not running, start it.
                try (StartContainerCmd startContainerCmd = dockerClient.startContainerCmd(containerId)) {
                    startContainerCmd.exec();
                }
                break;
            default:
                // TODO Need to consider actions for other states
                throw new IllegalStateException("Container '" + containerId + "' in a state (" + containerState
                        + ") that is currently unsupported in the DockerService::startContainer method.");

        }

        // Add container to the stack's network, if not already added
        try (InspectNetworkCmd inspectNetworkCmd = dockerClient.inspectNetworkCmd()) {
            if (null == inspectNetworkCmd.withNetworkId(network.getId()).exec().getContainers().get(containerId)) {
                try (ConnectToNetworkCmd connectToNetworkCmd = dockerClient.connectToNetworkCmd()) {
                    connectToNetworkCmd.withContainerId(containerId).withNetworkId(network.getId()).exec();
                }
            }
        }

        service.setContainerId(containerId);
        service.setDockerService(this);
    }

    private Optional<Container> startSwarmService(ContainerService service) {

        ServiceSpec serviceSpec = configureServiceSpec(service);

        try (CreateServiceCmd createServiceCmd = dockerClient.createServiceCmd(serviceSpec)) {
            CreateServiceResponse createServiceResponse = createServiceCmd.exec();

            TaskStatus taskStatus = new TaskStatus();
            TaskState taskState = TaskState.FAILED;
            do {
                try (ListTasksCmd listTasksCmd = dockerClient.listTasksCmd()) {
                    Optional<Task> task = listTasksCmd.withServiceFilter(service.getContainerName())
                            .exec().stream().findFirst();
                    if (task.isPresent()) {
                        taskStatus = task.get().getStatus();
                        taskState = taskStatus.getState();
                    }
                }
            } while (TaskState.RUNNING.compareTo(taskState) > 0);

            String errMessage = taskStatus.getErr();
            if (null != errMessage) {
                try (RemoveServiceCmd removeServiceCmd = dockerClient
                        .removeServiceCmd(createServiceResponse.getId())) {
                    removeServiceCmd.exec();
                }
                throw new RuntimeException("Failed to start service '" + service.getContainerName()
                        + "'. Error message is:\n" + errMessage);
            } else {
                String containerId = taskStatus.getContainerStatus().getContainerID();
                return getContainer(containerId);
            }
        }
    }

    private void removeSwarmService(ContainerService service) {
        Optional<Service> swarmService = getSwarmService(service);

        if (swarmService.isPresent()) {
            try (RemoveServiceCmd removeServiceCmd = dockerClient
                    .removeServiceCmd(swarmService.get().getId())) {
                removeServiceCmd.exec();
            }
        }
    }

    private ServiceSpec configureServiceSpec(ContainerService service) {
        List<Secret> secrets;
        try (ListSecretsCmd listSecretsCmd = dockerClient.listSecretsCmd()) {
            secrets = listSecretsCmd.exec();
        }

        ServiceSpec serviceSpec = service.getServiceSpec()
                .withName(service.getContainerName());
        service.getTaskTemplate()
                .withRestartPolicy(new ServiceRestartPolicy().withCondition(ServiceRestartCondition.NONE));
        ContainerSpec containerSpec = service.getContainerSpec()
                .withHostname(service.getName());
        List<ContainerSpecSecret> containerSecrets = containerSpec.getSecrets();
        if (null != containerSecrets) {
            containerSecrets.forEach(secretRef -> secrets.stream()
                    .filter(secret -> secret.getSpec().getName().equals(secretRef.getSecretName()))
                    .forEach(secret -> secretRef.withSecretId(secret.getId())));
        }
        return serviceSpec;
    }

    private void pullImage(ContainerService service) {
        String image = service.getImage();
        if (dockerClient.listImagesCmd().withImageNameFilter(image).exec().isEmpty()) {
            // No image with the requested image ID, so try to pull image
            try (PullImageCmd pullImageCmd = dockerClient.pullImageCmd(image)) {
                pullImageCmd
                        .exec(new PullImageResultCallback())
                        .awaitCompletion();
            } catch (InterruptedException ex) {
                Thread.currentThread().interrupt();
                throw new RuntimeException("Docker image pull command interupted", ex);
            }
        }
    }

    public String executeCommand(String containerId, String... cmd) {
        return executeCommand(containerId, true, cmd);
    }

    public String executeCommand(String containerId, boolean wait, String... cmd) {
        try (ExecCreateCmd execCreateCmd = dockerClient.execCreateCmd(containerId)) {
            String execId = execCreateCmd.withCmd(cmd)
                    .withAttachStdin(true)
                    .withAttachStderr(true)
                    .exec().getId();
            try (ExecStartCmd execStartCmd = dockerClient.execStartCmd(execId)) {
                Adapter<?> result = execStartCmd.exec(new ResultCallback.Adapter<>());
                if (wait) {
                    result.awaitCompletion(60, TimeUnit.SECONDS);
                } else {
                    result.awaitStarted(10, TimeUnit.SECONDS);
                }
            } catch (InterruptedException ex) {
                Thread.currentThread().interrupt();
                throw new RuntimeException("Docker exec command '" + Arrays.toString(cmd) + "' interupted", ex);
            }

            return execId;
        }
    }

    public long getCommandErrorCode(String execId) {
        try (InspectExecCmd inspectExecCmd = dockerClient.inspectExecCmd(execId)) {
            InspectExecResponse inspectExecResponce = inspectExecCmd.exec();
            Long exitCode = inspectExecResponce.getExitCodeLong();
            return (null != exitCode) ? exitCode : 1;
        }
    }

    public boolean fileExists(String containerId, String filePath) {
        return 0 == getCommandErrorCode(executeCommand(containerId, "test", "-f", filePath));
    }

    public void deleteFile(String containerId, String filePath) {
        executeCommand(containerId, "rm", filePath);
    }

    public void deleteDirectory(String containerId, String directoryPath) {
        executeCommand(containerId, "rm -r", directoryPath);
    }

    public void sendFiles(String containerId, Map<String, byte[]> files, String remotePath) throws IOException {

        makeDir(containerId, remotePath);

        byte[] byteArray;

        try (ByteArrayOutputStream bos = new ByteArrayOutputStream();
                GzipCompressorOutputStream gzo = new GzipCompressorOutputStream(bos);
                TarArchiveOutputStream tar = new TarArchiveOutputStream(gzo)) {
            for (Entry<String, byte[]> file : files.entrySet()) {
                String filePath = file.getKey().replace('\\', '/');
                byte[] fileContent = file.getValue();
                TarArchiveEntry entry = new TarArchiveEntry(filePath);
                entry.setSize(fileContent.length);
                entry.setMode(0755);
                tar.putArchiveEntry(entry);
                tar.write(fileContent);
                tar.closeArchiveEntry();
            }
            tar.finish();
            gzo.finish();
            byteArray = bos.toByteArray();
        }

        try (InputStream is = new ByteArrayInputStream(byteArray);
                CopyArchiveToContainerCmd copyArchiveToContainerCmd = dockerClient
                        .copyArchiveToContainerCmd(containerId)) {
            copyArchiveToContainerCmd.withTarInputStream(is)
                    .withRemotePath(remotePath).exec();

        }
    }

    private void makeDir(String containerId, String remotePath) {
        executeCommand(containerId, "mkdir", "-p", remotePath);
    }
}
