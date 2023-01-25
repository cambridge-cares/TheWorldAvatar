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
import com.github.dockerjava.jaxrs.ApiClientExtension;

import com.cmclinnovations.stack.clients.core.StackClient;
import com.cmclinnovations.stack.clients.docker.PodmanClient;
import com.cmclinnovations.stack.services.config.ServiceConfig;
import com.cmclinnovations.swagger.podman.ApiException;
import com.cmclinnovations.swagger.podman.api.ContainersApi;
import com.cmclinnovations.swagger.podman.model.BindOptions;
import com.cmclinnovations.swagger.podman.model.ContainerCreateResponse;
import com.cmclinnovations.swagger.podman.model.ListPodsReport;
import com.cmclinnovations.swagger.podman.model.Mount;
import com.cmclinnovations.swagger.podman.model.Secret;
import com.cmclinnovations.swagger.podman.model.SpecGenerator;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.github.dockerjava.api.command.ListPodsCmd;
import com.github.dockerjava.api.command.RemovePodCmd;
import com.github.dockerjava.api.model.BindPropagation;
import com.github.dockerjava.api.model.Container;
import com.github.dockerjava.api.model.ContainerSpec;
import com.github.dockerjava.api.model.ContainerSpecConfig;
import com.github.dockerjava.api.model.ContainerSpecFile;
import com.github.dockerjava.api.model.ContainerSpecSecret;

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

    private String getPodName(String containerName) {
        return containerName + "_pod";
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
            return listPodsCmd.withNameFilter(List.of(getPodName(service.getContainerName())))
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
        String containerName = service.getContainerName();
        ContainerSpec containerSpec = service.getContainerSpec();

        SpecGenerator generator = new SpecGenerator();

        generator.setName(containerName);
        generator.setPod(containerName);
        generator.setImage(service.getImage());
        generator.setEnv(service.getConfig().getEnvironment());
        generator.setCommand(containerSpec.getCommand());
        List<ContainerSpecSecret> secrets = containerSpec.getSecrets();
        if (null != secrets) {
            generator.setSecrets(secrets.stream()
                    .map(dockerSecret -> {
                        ContainerSpecFile file = dockerSecret.getFile();
                        return new Secret()
                                .source(file.getName())
                                .GID(Integer.parseInt(file.getGid()))
                                .UID(Integer.parseInt(file.getUid()))
                                .mode(file.getMode().intValue());
                    })
                    .collect(Collectors.toList()));
        }
        List<ContainerSpecConfig> configs = containerSpec.getConfigs();
        if (null != configs) {
            configs.forEach(dockerConfig -> {
                ContainerSpecFile file = dockerConfig.getFile();
                if (null != file) {
                    Long mode = file.getMode();
                    generator.addSecretsItem(new Secret()
                            .source(file.getName())
                            .target("/" + dockerConfig.getConfigName())
                            .GID(Integer.parseInt(file.getGid()))
                            .UID(Integer.parseInt(file.getUid()))
                            .mode(mode == null ? null : Math.toIntExact(mode)));
                }
            });
        }
        List<com.github.dockerjava.api.model.Mount> dockerMounts = containerSpec.getMounts();
        if (null != dockerMounts) {
            generator.setMounts(dockerMounts.stream()
                    .map(dockerMount -> new Mount()
                            .source(dockerMount.getSource())
                            .target(dockerMount.getTarget())
                            .type(dockerMount.getType().name().toLowerCase())
                            .readOnly(dockerMount.getReadOnly())
                            .bindOptions(convertBindOptions(dockerMount.getBindOptions())))
                    .collect(Collectors.toList()));
        }
        generator.setLabels(containerSpec.getLabels());

        try {
            ContainersApi api = new ContainersApi(new ApiClientExtension(URI.create("unix:///var/run/docker.sock")));
            ContainerCreateResponse containerCreateResponse = api.containerCreateLibpod(generator);

            return getContainerIfCreated(service.getContainerName());
        } catch (ApiException ex) {
            throw new RuntimeException("Failed to create Podman Container '" + containerName + "''.", ex);
        }
    }

    private BindOptions convertBindOptions(com.github.dockerjava.api.model.BindOptions dockerBindOptions) {
        try {
            if (null == dockerBindOptions) {
                return null;
            } else {
                BindPropagation propagation = dockerBindOptions.getPropagation();
                BindOptions podmanBindOptions = new BindOptions();
                if (null != propagation) {
                    podmanBindOptions.propagation(new ObjectMapper().writeValueAsString(propagation));
                }
                return podmanBindOptions;
            }
        } catch (JsonProcessingException ex) {
            throw new RuntimeException(ex);
        }
    }

}
