package com.cmclinnovations.stack.services;

import java.io.IOException;
import java.net.URI;
import java.nio.file.FileVisitResult;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.SimpleFileVisitor;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import com.cmclinnovations.stack.clients.docker.PodmanClient;
import com.cmclinnovations.stack.services.config.ServiceConfig;
import com.cmclinnovations.swagger.podman.model.ListPodsReport;
import com.github.dockerjava.api.command.ListPodsCmd;
import com.github.dockerjava.api.command.RemovePodCmd;
import com.github.dockerjava.api.model.Container;


public class PodmanService extends DockerService {

    public static final String TYPE = "podman";

    public PodmanService(String stackName, ServiceManager serviceManager, ServiceConfig config) {
        super(stackName, serviceManager, config);
    }

    @Override
    public PodmanClient initClient(URI dockerUri) {
        return new PodmanClient(dockerUri);
    }

    @Override
    public PodmanClient getClient() {
        return (PodmanClient) super.getClient();
    }

    @Override
    protected void initialise(String stackName) {

        addStackSecrets();

        // addStackConfigs();

        createNetwork(stackName);
    }

    @Override
    protected void addStackConfigs() {

        try {
            Files.walkFileTree(Path.of("/inputs/config"), new SimpleFileVisitor<Path>() {

                @Override
                public FileVisitResult visitFile(Path file, BasicFileAttributes attrs) throws IOException {
                    try {
                        if (Files.isReadable(file) && !file.getFileName().toString().startsWith(".git")) {
                            String configName = file.getFileName().toString();

                            try (Stream<String> lines = Files.lines(file)) {
                                String data = lines.collect(Collectors.joining("\n"));
                                dockerClient.addSecret(configName, data);
                            }
                        }
                        return FileVisitResult.CONTINUE;
                    } catch (IOException ex) {
                        throw new IOException("Failed to load config file '" + file + "'.", ex);
                    }
                }
            });
        } catch (IOException ex) {
            throw new RuntimeException("Failed to load configs.", ex);
        }
    }

    @Override
    protected Optional<Container> configureContainerWrapper(ContainerService service) {
        Optional<Container> container;
        removePod(service);

        container = startPod(service);
        return container;
    }

    private Optional<ListPodsReport> getPod(ContainerService service) {
        try (ListPodsCmd listPodsCmd = getClient().getInternalClient().listPodsCmd()) {
            return listPodsCmd.withNameFilter(List.of(service.getContainerName()))
                    .exec().stream().findAny();
        }
    }

    private void removePod(ContainerService service) {
        Optional<ListPodsReport> pod = getPod(service);

        if (pod.isPresent()) {
            try (RemovePodCmd removePodCmd = getClient().getInternalClient()
                    .removePodCmd(pod.get().getId())) {
                removePodCmd.exec();
            }
        }
    }

    private Optional<Container> startPod(ContainerService service) {
        return null;
    }


}
