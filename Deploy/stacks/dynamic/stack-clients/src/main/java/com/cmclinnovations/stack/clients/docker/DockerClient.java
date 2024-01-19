package com.cmclinnovations.stack.clients.docker;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.URI;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Optional;
import java.util.concurrent.TimeUnit;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.apache.commons.compress.archivers.tar.TarArchiveEntry;
import org.apache.commons.compress.archivers.tar.TarArchiveInputStream;
import org.apache.commons.compress.archivers.tar.TarArchiveOutputStream;
import org.apache.commons.compress.compressors.gzip.GzipCompressorOutputStream;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.cmclinnovations.stack.clients.core.EndpointConfig;
import com.cmclinnovations.stack.clients.core.StackClient;
import com.cmclinnovations.stack.clients.utils.AbstractTempPath;
import com.cmclinnovations.stack.clients.utils.TempDir;
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

public class DockerClient extends BaseClient implements ContainerManager<com.github.dockerjava.api.DockerClient> {

    protected static final Logger LOGGER = LoggerFactory.getLogger(DockerClient.class);

    private final com.github.dockerjava.api.DockerClient internalClient;

    private static DockerClient instance = null;

    public static DockerClient getInstance() {
        if (null == instance) {
            if (StackClient.getContainerEngineName().equals("podman")) {
                instance = new PodmanClient();
            } else {
                instance = new DockerClient();
            }
        }
        return instance;
    }

    protected DockerClient() {
        this(null);
    }

    public DockerClient(URI endpoint) {
        Builder dockerConfigBuilder = DefaultDockerClientConfig.createDefaultConfigBuilder();

        if (null != endpoint) {
            dockerConfigBuilder.withDockerHost(endpoint.toString());
        }

        DockerClientConfig dockerConfig = dockerConfigBuilder
                .build();

        DockerHttpClient httpClient = new ApacheDockerHttpClient.Builder()
                .dockerHost(dockerConfig.getDockerHost())
                .sslConfig(dockerConfig.getSSLConfig())
                .build();

        internalClient = buildInternalClient(dockerConfig, httpClient);
    }

    @Override
    public com.github.dockerjava.api.DockerClient buildInternalClient(DockerClientConfig dockerConfig,
            DockerHttpClient httpClient) {
        return DockerClientBuilder.getInstance(dockerConfig).withDockerHttpClient(httpClient).build();
    }

    @Override
    public com.github.dockerjava.api.DockerClient getInternalClient() {
        return internalClient;
    }

    public String executeSimpleCommand(String containerId, String... cmd) {
        ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
        String execId = createComplexCommand(containerId, cmd)
                .withOutputStream(outputStream)
                .withErrorStream(outputStream)
                .exec();
        String output = outputStream.toString();
        return execId;
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

        private long initialisationTimeout = 10;
        private long evaluationTimeout = 60;

        private String user;

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

        public ComplexCommand withInitialisationTimeout(long timeout) {
            this.initialisationTimeout = timeout;
            return this;
        }

        public ComplexCommand withEvaluationTimeout(long timeout) {
            this.evaluationTimeout = timeout;
            return this;
        }

        public ComplexCommand withUser(String user) {
            this.user = user;
            return this;
        }

        public String exec() {
            boolean attachStdin = null != inputStream;
            boolean attachStdout = null != outputStream;
            boolean attachStderr = null != errorStream;

            if (null != hereDocument) {
                if (attachStdin) {
                    throw new IllegalArgumentException("Can't specify both 'inputStream' and 'hereDocument'.");
                }
                cmd = List.of("sh", "-c",
                        Arrays.stream(cmd).collect(Collectors.joining("' '", "'", "'"))
                                + "<< '\04\04\04'\n" + hereDocument + (hereDocument.endsWith("\n") ? "" : "\n")
                                + "\04\04\04")
                        .toArray(new String[] {});
            }

            String execId = execCreateCmd.withCmd(cmd)
                    .withEnv(envVars.entrySet().stream()
                            .map(entry -> entry.getKey() + '=' + entry.getValue())
                            .collect(Collectors.toList()))
                    .withAttachStdin(attachStdin)
                    .withAttachStdout(attachStdout)
                    .withAttachStderr(attachStderr)
                    .withUser(user)
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
                        if (!result.awaitCompletion(evaluationTimeout, TimeUnit.SECONDS)) {
                            LOGGER.warn("Docker exec command '{}' still running after the {} second execution timeout.",
                                    cmd, evaluationTimeout);
                        }
                    } else {
                        if (!result.awaitStarted(initialisationTimeout, TimeUnit.SECONDS)) {
                            LOGGER.warn(
                                    "Docker exec command '{}' still not started within the {} second initialisation timeout.",
                                    cmd, evaluationTimeout);
                        }
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
        Long exitCode = null;
        try (InspectExecCmd inspectExecCmd = internalClient.inspectExecCmd(execId)) {

            boolean isRunning = true;
            while (isRunning) {
                InspectExecResponse inspectExecResponce = inspectExecCmd.exec();
                isRunning = inspectExecResponce.isRunning();
                if (isRunning) {
                    Thread.sleep(500);
                } else {
                    exitCode = inspectExecResponce.getExitCodeLong();
                }
            }
        } catch (InterruptedException ex) {
            LOGGER.warn("Sleep method was interrupted whilst waiting for Docker inspect exec command.", ex);
            Thread.currentThread().interrupt();
        }
        if (null == exitCode) {
            throw new RuntimeException(
                    "Docker exec command returned 'null' exit code even after it had finshed running.");
        } else {
            return exitCode;
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

    public void makeDir(String containerId, String directoryPath, String user) {
        createComplexCommand(containerId, "mkdir", "-p", directoryPath).withUser(user).exec();
    }

    private final class RemoteTempDir extends AbstractTempPath implements TempDir {

        private final String containerId;

        public RemoteTempDir(String containerId, String path) {
            super(Path.of(path));
            this.containerId = containerId;
        }

        @Override
        public void close() throws RuntimeException {
            deleteDirectory(containerId, getPath().toString());
        }

        @Override
        public void copyFrom(Path sourcePath) {
            String targetDir = toString();
            if (Files.isDirectory(sourcePath)) {
                sendFolder(containerId, sourcePath.toString(), targetDir);
            } else if (Files.isRegularFile(sourcePath)) {
                sendFiles(containerId, sourcePath.getParent().toString(), List.of(sourcePath.getFileName().toString()),
                        targetDir);
            } else {
                throw new RuntimeException("Couldn't copy '" + sourcePath + "' into '" + targetDir
                        + "' as the source was neither a file nor a directory.");
            }
        }

        @Override
        public void copyTo(Path targetDir) {
            String sourcePath = toString();
            try {
                retrieveFiles(containerId, sourcePath).forEach((path, content) -> {
                    try {
                        Path localAbsPath = targetDir.resolve(Path.of(path));
                        Files.createDirectories(localAbsPath.getParent());
                        Files.write(localAbsPath, content);
                    } catch (IOException ex) {
                        throw new RuntimeException("Couldn't copy file '" + path + "'' from '" + sourcePath + "' into '"
                                + targetDir + "'.", ex);
                    }
                });
            } catch (IOException ex) {
                throw new RuntimeException("Couldn't copy '" + sourcePath + "' into '" + targetDir + "'.", ex);
            }
        }
    }

    public RemoteTempDir makeTempDir(String containerId) {
        ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
        createComplexCommand(containerId, "mktemp", "-d").withOutputStream(outputStream).exec();
        return new RemoteTempDir(containerId, outputStream.toString().replaceAll("\\r?\\n?", ""));
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

    private void sendFileEntries(String containerId, String remoteDirPath, Iterable<Entry<String, byte[]>> fileEntries)
            throws IOException {
        makeDir(containerId, remoteDirPath);

        byte[] byteArray;

        try (ByteArrayOutputStream bos = new ByteArrayOutputStream()) {
            try (GzipCompressorOutputStream gzo = new GzipCompressorOutputStream(bos);
                    TarArchiveOutputStream tar = new TarArchiveOutputStream(gzo)) {
                for (Entry<String, byte[]> fileEntry : fileEntries) {
                    addFileContentToTar(tar, fileEntry.getKey(), fileEntry.getValue());
                }
            }
            byteArray = bos.toByteArray();
        }

        sendTarFileContent(containerId, remoteDirPath, byteArray);
    }

    private void addFileContentToTar(TarArchiveOutputStream tar, String filePath, byte[] fileContent)
            throws IOException {
        // Tar files record paths as strings and should use the Linux path separator '/'
        filePath = filePath.replace('\\', '/');
        TarArchiveEntry entry = new TarArchiveEntry(filePath);
        entry.setSize(fileContent.length);
        entry.setMode(0755);
        // Set the files' user and group to the default ones in that container
        entry.setIds(1000, 1000);
        tar.putArchiveEntry(entry);
        tar.write(fileContent);
        tar.closeArchiveEntry();
    }

    private void sendTarFileContent(String containerId, String remoteDirPath, byte[] byteArray) throws IOException {
        try (InputStream is = new ByteArrayInputStream(byteArray);
                CopyArchiveToContainerCmd copyArchiveToContainerCmd = internalClient
                        .copyArchiveToContainerCmd(containerId)) {
            copyArchiveToContainerCmd.withTarInputStream(is)
                    .withRemotePath(remoteDirPath).exec();

        }
    }

    private final class FileIterater implements Iterable<Entry<String, byte[]>>, AutoCloseable {
        Path dirPath;
        private final Stream<Path> stream;

        public FileIterater(String baseDir) throws IOException {
            dirPath = Path.of(baseDir);
            stream = Files.walk(Path.of(baseDir));
        }

        public FileIterater(String baseDir, Collection<String> relativeFilePaths) {
            dirPath = Path.of(baseDir);
            stream = relativeFilePaths.stream().map(dirPath::resolve);
        }

        @Override
        public Iterator<Map.Entry<String, byte[]>> iterator() {

            return stream.filter(Files::isRegularFile)
                    .map(path -> {
                        try {
                            return Map.entry(dirPath.relativize(path).toString(), Files.readAllBytes(path));
                        } catch (IOException ex) {
                            throw new RuntimeException("Failed to read file '" + path + "'", ex);
                        }
                    }).iterator();
        }

        @Override
        public void close() throws Exception {
            stream.close();
        }
    }

    public void sendFilesContent(String containerId, Map<String, byte[]> files, String remoteDirPath) {
        try {
            sendFileEntries(containerId, remoteDirPath, files.entrySet());
        } catch (Exception ex) {
            throw new RuntimeException("Failed to send the content of the following files to '" + remoteDirPath + "':\n"
                    + files.keySet().stream().collect(Collectors.joining("'\n'", "'", "'")), ex);
        }
    }

    public void sendFiles(String containerId, String localDirPath, List<String> filePaths, String remoteDirPath) {
        try (FileIterater fileIterator = new FileIterater(localDirPath, filePaths)) {
            sendFileEntries(containerId, remoteDirPath, fileIterator);
        } catch (Exception ex) {
            throw new RuntimeException("Failed to send the following files to '" + remoteDirPath + "':\n"
                    + filePaths.stream().collect(Collectors.joining("'\n'", "'", "'")), ex);
        }
    }

    public void sendFolder(String containerId, String localDirPath, String remoteDirPath) {
        try (FileIterater fileIterator = new FileIterater(localDirPath)) {
            sendFileEntries(containerId, remoteDirPath, fileIterator);
        } catch (Exception ex) {
            throw new RuntimeException("Failed to send files from folder '" + localDirPath
                    + "' to '" + remoteDirPath + "'.", ex);
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

    public Optional<Container> getContainer(String containerName, boolean showAll) {
        try (ListContainersCmd listContainersCmd = internalClient.listContainersCmd()) {
            Pattern fullContainerNamePattern = Pattern
                    .compile("/?" + StackClient.getStackNameForRegex() + "-" + containerName + "(?:\\..*)?");
            List<Container> possibleContainers = listContainersCmd.withNameFilter(List.of(containerName))
                    .withLabelFilter(StackClient.getStackNameLabelMap())
                    .withShowAll(showAll).exec();
            return possibleContainers
                    .stream()
                    .filter(container -> Stream.of(container.getNames())
                            .anyMatch(name -> {
                                return fullContainerNamePattern.matcher(name).matches();
                            }))
                    .findAny();
        }
    }

    public Optional<Container> getContainer(String containerName) {
        // Setting "showAll" to "true" ensures non-running containers are also returned
        return getContainer(containerName, true);
    }

    public Optional<Container> getContainerFromID(String containerId) {
        try (ListContainersCmd listContainersCmd = internalClient.listContainersCmd()) {
            // Setting "showAll" to "true" ensures non-running containers are also returned
            return listContainersCmd.withIdFilter(List.of(containerId))
                    .withShowAll(true).exec()
                    .stream().findAny();
        }
    }

    public boolean isContainerUp(String containerName) {
        // Setting "showAll" to "false" ensures only running containers are returned
        return getContainer(containerName, false).isPresent();
    }

    public String getContainerId(String containerName) {
        return getContainer(containerName).map(Container::getId).orElseThrow();
    }

    private Map<String, List<String>> convertToConfigFilterMap(String configName, Map<String, String> labelMap) {
        Map<String, List<String>> result = new HashMap<>();
        result.put("label",
                labelMap.entrySet().stream()
                        .map(entry -> entry.getKey() + "=" + entry.getValue())
                        .collect(Collectors.toList()));
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
            String fullConfigName = StackClient.prependStackName(configName);
            return listConfigsCmd
                    .withFilters(convertToConfigFilterMap(fullConfigName,
                            StackClient.getStackNameLabelMap()))
                    .exec().stream()
                    .filter(config -> config.getSpec().getName().equals(fullConfigName))
                    .findFirst();
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
        try (RemoveConfigCmd removeConfigCmd = internalClient.removeConfigCmd(config.getId())) {
            removeConfigCmd.exec();
        } catch (Exception ex) {
            // Either the Config has been removed externally
            // or it is currently in use and can't be removed.
        }
    }

    @Override
    public <E extends EndpointConfig> void writeEndpointConfig(E endpointConfig) {
        writeEndpointConfig(endpointConfig, this);
    }

    @Override
    public <E extends EndpointConfig> E readEndpointConfig(String endpointName, Class<E> endpointConfigClass) {
        return readEndpointConfig(endpointName, endpointConfigClass, this);
    }

    protected Map<String, String> getSecretLabels() {
        return StackClient.getStackNameLabelMap();
    }

    public boolean secretExists(String secretName) {
        return getSecret(secretName).isPresent();
    }

    public Optional<Secret> getSecret(String secretName) {
        try (ListSecretsCmd listSecretsCmd = internalClient.listSecretsCmd()) {
            String fullSecretName = StackClient.prependStackName(secretName);
            return getSecret(listSecretsCmd.withNameFilter(List.of(fullSecretName)).exec(), secretName);
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
            Map<String, String> secretLabels = getSecretLabels();
            if (null != secretLabels) {
                listSecretsCmd.withLabelFilter(secretLabels);
            }
            return listSecretsCmd.exec().stream().collect(Collectors.toList());
        }
    }

    public void addSecret(String secretName, String data) {
        SecretSpec secretSpec = new SecretSpec()
                .withName(StackClient.prependStackName(secretName))
                .withData(data)
                .withLabels(getSecretLabels());
        try (CreateSecretCmd createSecretCmd = internalClient.createSecretCmd(secretSpec)) {
            createSecretCmd.exec();
        }
    }

    public void removeSecret(Secret secret) {
        try (RemoveSecretCmd removeSecretCmd = internalClient.removeSecretCmd(secret.getId())) {
            removeSecretCmd.exec();
        } catch (Exception ex) {
            // Either the Secret has been removed externally
            // or it is currently in use and can't be removed.
        }
    }

}
