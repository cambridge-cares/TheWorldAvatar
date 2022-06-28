package com.cmclinnovations.stack.clients.docker;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.HttpURLConnection;
import java.net.MalformedURLException;
import java.net.ProtocolException;
import java.net.URI;
import java.net.URL;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Optional;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.apache.commons.compress.archivers.tar.TarArchiveEntry;
import org.apache.commons.compress.archivers.tar.TarArchiveInputStream;
import org.apache.commons.compress.archivers.tar.TarArchiveOutputStream;
import org.apache.commons.compress.compressors.gzip.GzipCompressorOutputStream;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.cmclinnovations.stack.clients.core.AbstractEndpointConfig;
import com.cmclinnovations.stack.clients.core.StackClient;
import com.github.dockerjava.api.command.CopyArchiveToContainerCmd;
import com.github.dockerjava.api.command.CreateConfigCmd;
import com.github.dockerjava.api.command.CreateSecretCmd;
import com.github.dockerjava.api.command.ExecCreateCmd;
import com.github.dockerjava.api.command.ExecStartCmd;
import com.github.dockerjava.api.command.InspectContainerCmd;
import com.github.dockerjava.api.command.InspectExecCmd;
import com.github.dockerjava.api.command.InspectExecResponse;
import com.github.dockerjava.api.command.ListConfigsCmd;
import com.github.dockerjava.api.command.ListContainersCmd;
import com.github.dockerjava.api.command.ListSecretsCmd;
import com.github.dockerjava.api.command.RemoveConfigCmd;
import com.github.dockerjava.api.command.RemoveSecretCmd;
import com.github.dockerjava.api.model.Config;
import com.github.dockerjava.api.model.Container;
import com.github.dockerjava.api.model.Secret;
import com.github.dockerjava.api.model.SecretSpec;
import com.github.dockerjava.core.DefaultDockerClientConfig;
import com.github.dockerjava.core.DefaultDockerClientConfig.Builder;
import com.github.dockerjava.core.DockerClientBuilder;
import com.github.dockerjava.core.DockerClientConfig;
import com.github.dockerjava.core.RemoteApiVersion;
import com.github.dockerjava.core.command.ExecStartResultCallback;
import com.github.dockerjava.httpclient5.ApacheDockerHttpClient;
import com.github.dockerjava.transport.DockerHttpClient;

public class DockerClient extends BaseClient {

    private static final String DOCKER_INTERNAL_HOST = "host.docker.internal";
    private static final String DEFAULT_DOCKER_PORT = "2375";

    protected static final Logger LOGGER = LoggerFactory.getLogger(DockerClient.class);

    private final com.github.dockerjava.api.DockerClient internalClient;

    private static Optional<Boolean> isInsideContainer = Optional.empty();

    private static final URI getDockerURI() {
        if (isInsideContainer.isEmpty()) {
            try {
                URL url = new URL("http://" + DOCKER_INTERNAL_HOST + ":" + DEFAULT_DOCKER_PORT + "/_ping");

                HttpURLConnection con = (HttpURLConnection) url.openConnection();
                con.setRequestMethod("GET");
                if (con.getResponseCode() == HttpURLConnection.HTTP_OK) {
                    isInsideContainer = Optional.of(true);
                } else {
                    isInsideContainer = Optional.of(false);
                }
            } catch (MalformedURLException | ProtocolException ex) {
                throw new RuntimeException("Something has gon very wrong.", ex);
            } catch (IOException ex) {
                // Failiure to connect means outside container
                isInsideContainer = Optional.of(false);
            }
        }
        return isInsideContainer.get() ? URI.create("tcp://" + DOCKER_INTERNAL_HOST + ":" + DEFAULT_DOCKER_PORT) : null;
    }

    public DockerClient() {
        this(getDockerURI());
    }

    public DockerClient(URI endpoint) {
        Builder dockerConfigBuilder = DefaultDockerClientConfig.createDefaultConfigBuilder();

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

        internalClient = DockerClientBuilder.getInstance(dockerConfig).withDockerHttpClient(httpClient).build();
    }

    public com.github.dockerjava.api.DockerClient getInternalClient() {
        return internalClient;
    }

    public String executeSimpleCommand(String containerId, String... cmd) {
        return createComplexCommand(containerId, cmd).exec();
    }

    public ComplexCommand createComplexCommand(String containerId, String... cmd) {
        return new ComplexCommand(containerId, cmd);
    }

    public final class ComplexCommand {

        private final ExecCreateCmd execCreateCmd;

        private String[] cmd;

        private boolean wait = true;

        private InputStream inputStream = null;
        private OutputStream outputStream = null;
        private OutputStream errorStream = null;

        private String hereDocument = null;

        private final Map<String, String> envVars = new HashMap<>();

        public ComplexCommand(String containerId, String... cmd) {
            execCreateCmd = internalClient.execCreateCmd(containerId);
            this.cmd = cmd;
        }

        public ComplexCommand withWait(boolean wait) {
            this.wait = wait;
            return this;
        }

        public ComplexCommand withInputStream(InputStream inputStream) {
            this.inputStream = inputStream;
            return this;
        }

        public ComplexCommand withOutputStream(OutputStream outputStream) {
            this.outputStream = outputStream;
            return this;
        }

        public ComplexCommand withErrorStream(OutputStream errorStream) {
            this.errorStream = errorStream;
            return this;
        }

        public ComplexCommand withHereDocument(String hereDocument) {
            this.hereDocument = hereDocument;
            return this;
        }

        public ComplexCommand withEnvVar(String key, String value) {
            envVars.put(key, value);
            return this;
        }

        public ComplexCommand withEnvVars(Map<String, String> envVars) {
            this.envVars.putAll(envVars);
            return this;
        }

        public String exec() {
            boolean attachStdin = null != inputStream;
            boolean attachStdout = null != outputStream;
            boolean attachStderr = null != errorStream;

            if (null != hereDocument) {
                if (attachStdin) {
                    throw new IllegalArgumentException("Can't specify both 'inputStream' and 'inputStream'.");
                }
                cmd = List.of("sh", "-c",
                        Arrays.stream(cmd).collect(Collectors.joining("' '", "'", "'"))
                                + "<< '\04\04\04'\n" + hereDocument + "\04\04\04")
                        .toArray(new String[] {});
            }

            String execId = execCreateCmd.withCmd(cmd)
                    .withEnv(envVars.entrySet().stream()
                            .map(entry -> entry.getKey() + '=' + entry.getValue())
                            .collect(Collectors.toList()))
                    .withAttachStdin(attachStdin)
                    .withAttachStdout(attachStdout)
                    .withAttachStderr(attachStderr)
                    .exec().getId();

            try (ExecStartCmd execStartCmd = internalClient.execStartCmd(execId)) {

                if (attachStdin) {
                    execStartCmd.withStdIn(inputStream);
                }

                // ExecStartResultCallback is marked deprecated but seems to do exactly what we
                // want and without knowing why it is deprecated any issues with it can't be
                // overcome anyway.
                try (ExecStartResultCallback result = execStartCmd
                        .exec(new ExecStartResultCallback(outputStream, errorStream))) {
                    if (wait) {
                        result.awaitCompletion(60, TimeUnit.SECONDS);
                    } else {
                        result.awaitStarted(10, TimeUnit.SECONDS);
                    }
                } catch (InterruptedException ex) {
                    Thread.currentThread().interrupt();
                    throw new RuntimeException("Docker exec command '" + Arrays.toString(cmd) + "' interupted", ex);
                } catch (IOException ex) {
                    throw new RuntimeException("Docker exec command '" + Arrays.toString(cmd) + "' failed", ex);
                }
            }
            return execId;
        }
    }

    public long getCommandErrorCode(String execId) {
        try (InspectExecCmd inspectExecCmd = internalClient.inspectExecCmd(execId)) {
            InspectExecResponse inspectExecResponce = inspectExecCmd.exec();
            Long exitCode = inspectExecResponce.getExitCodeLong();
            return (null != exitCode) ? exitCode : 1;
        }
    }

    public Optional<String> getEnvironmentVariable(String containerId, String key) {
        try (InspectContainerCmd inspectContainerCmd = internalClient.inspectContainerCmd(containerId)) {
            return Stream.of(inspectContainerCmd.exec().getConfig().getEnv()).map(entry -> entry.split("=", 2))
                    .filter(entry -> key.equals(entry[0]))
                    .map(entry -> entry[1])
                    .findFirst();
        }
    }

    public boolean fileExists(String containerId, String filePath) {
        return 0 == getCommandErrorCode(executeSimpleCommand(containerId, "test", "-f", filePath));
    }

    public void makeDir(String containerId, String directoryPath) {
        executeSimpleCommand(containerId, "mkdir", "-p", directoryPath);
    }

    public String makeTempDir(String containerId) {
        ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
        createComplexCommand(containerId, "mktemp", "-d").withOutputStream(outputStream).exec();
        return outputStream.toString().replaceAll("\\r?\\n?", "");
    }

    public void deleteFile(String containerId, String filePath) {
        executeSimpleCommand(containerId, "rm", filePath);
    }

    public boolean directoryExists(String containerId, String directoryPath) {
        return 0 == getCommandErrorCode(executeSimpleCommand(containerId, "test", "-d", directoryPath));
    }

    public void deleteDirectory(String containerId, String directoryPath) {
        executeSimpleCommand(containerId, "rm", "-r", directoryPath);
    }

    public void sendFiles(String containerId, Map<String, byte[]> files, String remoteDirPath) throws IOException {

        makeDir(containerId, remoteDirPath);

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
                CopyArchiveToContainerCmd copyArchiveToContainerCmd = internalClient
                        .copyArchiveToContainerCmd(containerId)) {
            copyArchiveToContainerCmd.withTarInputStream(is)
                    .withRemotePath(remoteDirPath).exec();

        }
    }

    public Map<String, byte[]> retrieveFiles(String containerId, String remoteDirPath) throws IOException {
        Map<String, byte[]> files = new HashMap<>();
        try (InputStream is = internalClient.copyArchiveFromContainerCmd(containerId, remoteDirPath).exec();
                TarArchiveInputStream tarArchiveInputStream = new TarArchiveInputStream(is)) {

            TarArchiveEntry tarArchiveEntry;
            while (null != (tarArchiveEntry = tarArchiveInputStream.getNextTarEntry())) {
                if (!tarArchiveEntry.isDirectory()) {
                    files.put(Path.of(remoteDirPath, tarArchiveEntry.getName()).toString(),
                            tarArchiveInputStream.readAllBytes());
                }
            }
        }
        return files;
    }

    public byte[] retrieveFile(String containerId, String remoteFilePath) throws IOException {
        try (InputStream is = internalClient.copyArchiveFromContainerCmd(containerId, remoteFilePath).exec();
                TarArchiveInputStream tarArchiveInputStream = new TarArchiveInputStream(is)) {

            TarArchiveEntry tarArchiveEntry;
            while (null != (tarArchiveEntry = tarArchiveInputStream.getNextTarEntry())) {
                if (!tarArchiveEntry.isDirectory()) {
                    return tarArchiveInputStream.readAllBytes();
                }
            }
        }
        return new byte[0];
    }

    public Optional<Container> getContainer(String containerName) {
        try (ListContainersCmd listContainersCmd = internalClient.listContainersCmd()) {
            // Setting "showAll" to "true" ensures non-running containers are also returned
            return listContainersCmd.withNameFilter(List.of(containerName))
                    .withShowAll(true).exec()
                    .stream().findAny();
        }
    }

    public boolean isContainerUp(String containerName) {
        try (ListContainersCmd listContainersCmd = internalClient.listContainersCmd()) {
            // Don't need to filter for "running" state as this is the default setting
            return !listContainersCmd.withNameFilter(List.of(containerName)).exec().isEmpty();
        }
    }

    public String getContainerId(String containerName) {
        return getContainer(containerName).map(Container::getId).orElseThrow();
    }

    private Map<String, List<String>> convertToConfigFilterMap(String configName, Map<String, String> labelMap) {
        Map<String, List<String>> result = labelMap.entrySet().stream().collect(Collectors.toMap(
                entry -> "label",
                entry -> List.of(entry.getKey() + "=" + entry.getValue())));
        if (null != configName) {
            result.put("name", List.of(configName));
        }
        return result;
    }

    public boolean configExists(String configName) {
        return getConfig(configName).isPresent();
    }

    public Optional<Config> getConfig(String configName) {
        try (ListConfigsCmd listConfigsCmd = internalClient.listConfigsCmd()) {
            return listConfigsCmd
                    .withFilters(convertToConfigFilterMap(StackClient.prependStackName(configName),
                            StackClient.getStackNameLabelMap()))
                    .exec().stream().findFirst();
        }
    }

    public Optional<Config> getConfig(List<Config> configs, String configName) {
        String fullConfigName = StackClient.prependStackName(configName);
        return configs.stream().filter(
                existingConfig -> existingConfig.getSpec().getName()
                        .equals(fullConfigName))
                .findFirst();

    }

    public List<Config> getConfigs() {
        try (ListConfigsCmd listConfigsCmd = internalClient.listConfigsCmd()) {
            return listConfigsCmd
                    .withFilters(convertToConfigFilterMap(null, StackClient.getStackNameLabelMap()))
                    .exec().stream().collect(Collectors.toList());
        }
    }

    public void addConfig(String configName, String data) {
        addConfig(configName, data.getBytes());
    }

    public void addConfig(String configName, byte[] data) {
        try (CreateConfigCmd createConfigCmd = internalClient.createConfigCmd()) {
            createConfigCmd
                    .withName(StackClient.prependStackName(configName))
                    .withData(data)
                    .withLabels(StackClient.getStackNameLabelMap())
                    .exec();
        }
    }

    public void removeConfig(Config config) {
        try (RemoveConfigCmd removeConfigCmd = getInternalClient()
                .removeConfigCmd(config.getId())) {
            removeConfigCmd.exec();
        } catch (Exception ex) {
            // Either the Config has been removed externally
            // or it is currently in use and can't be removed.
        }
    }

    @Override
    public <E extends AbstractEndpointConfig> void writeEndpointConfig(E endpointConfig) {
        writeEndpointConfig(endpointConfig, this);
    }

    @Override
    public <E extends AbstractEndpointConfig> E readEndpointConfig(String endpointName, Class<E> endpointConfigClass) {
        return readEndpointConfig(endpointName, endpointConfigClass, this);
    }

    public boolean secretExists(String secretName) {
        return getSecret(StackClient.prependStackName(secretName)).isPresent();
    }

    public Optional<Secret> getSecret(String secretName) {
        try (ListSecretsCmd listSecretsCmd = internalClient.listSecretsCmd()) {
            return listSecretsCmd
                    .withNameFilter(List.of(StackClient.prependStackName(secretName)))
                    .withLabelFilter(StackClient.getStackNameLabelMap())
                    .exec().stream().findFirst();
        }
    }

    public final Optional<Secret> getSecret(List<Secret> secrets, String secretName) {
        String fullSecretName = StackClient.prependStackName(secretName);
        return secrets.stream()
                .filter(secret -> secret.getSpec().getName().equals(fullSecretName))
                .findFirst();
    }

    public List<Secret> getSecrets() {
        try (ListSecretsCmd listSecretsCmd = internalClient.listSecretsCmd()) {
            return listSecretsCmd
                    .withLabelFilter(StackClient.getStackNameLabelMap())
                    .exec().stream().collect(Collectors.toList());
        }
    }

    public void addSecret(String secretName, String data) {
        SecretSpec secretSpec = new SecretSpec()
                .withName(StackClient.prependStackName(secretName))
                .withData(data)
                .withLabels(StackClient.getStackNameLabelMap());
        try (CreateSecretCmd createSecretCmd = getInternalClient()
                .createSecretCmd(secretSpec)) {
            createSecretCmd.exec();
        }
    }

    public void removeSecret(Secret secret) {
        try (RemoveSecretCmd removeSecretCmd = getInternalClient()
                .removeSecretCmd(secret.getId())) {
            removeSecretCmd.exec();
        } catch (Exception ex) {
            // Either the Secret has been removed externally
            // or it is currently in use and can't be removed.
        }
    }

}
