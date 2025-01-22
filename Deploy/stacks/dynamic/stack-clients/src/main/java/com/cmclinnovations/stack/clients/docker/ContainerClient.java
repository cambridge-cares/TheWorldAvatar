package com.cmclinnovations.stack.clients.docker;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.NoSuchElementException;

import org.slf4j.Logger;

import com.cmclinnovations.stack.clients.docker.DockerClient.ComplexCommand;
import com.cmclinnovations.stack.clients.utils.LocalTempDir;
import com.cmclinnovations.stack.clients.utils.TempDir;
import com.github.dockerjava.api.model.Container;

public class ContainerClient extends BaseClient {

    protected final String executeSimpleCommand(String containerId, String... cmd) {
        return DockerClient.getInstance().executeSimpleCommand(containerId, cmd);
    }

    protected final ComplexCommand createComplexCommand(String containerId, String... cmd) {
        return DockerClient.getInstance().createComplexCommand(containerId, cmd);
    }

    protected final long getCommandErrorCode(String execId) {
        return DockerClient.getInstance().getCommandErrorCode(execId);
    }

    protected final Optional<String> getEnvironmentVariable(String containerId, String key) {
        return DockerClient.getInstance().getEnvironmentVariable(containerId, key);
    }

    protected final boolean fileExists(String containerId, String filePath) {
        return DockerClient.getInstance().fileExists(containerId, filePath);
    }

    protected final void makeDir(String containerId, String directoryPath) {
        DockerClient.getInstance().makeDir(containerId, directoryPath);
    }

    protected final TempDir makeLocalTempDir() {
        try {
            return new LocalTempDir();
        } catch (IOException ex) {
            throw new RuntimeException("Faile to create local temporary directory.", ex);
        }
    }

    protected final TempDir makeRemoteTempDir(String containerId) {
        return DockerClient.getInstance().makeTempDir(containerId);
    }

    protected final void deleteFile(String containerId, String filePath) {
        DockerClient.getInstance().deleteFile(containerId, filePath);
    }

    protected final boolean directoryExists(String containerId, String directoryPath) {
        return DockerClient.getInstance().directoryExists(containerId, directoryPath);
    }

    protected final void deleteDirectory(String containerId, String directoryPath) {
        DockerClient.getInstance().deleteDirectory(containerId, directoryPath);
    }

    protected final void sendFilesContent(String containerId, Map<String, byte[]> files, String remoteDirPath) {
        DockerClient.getInstance().sendFilesContent(containerId, files, remoteDirPath);
    }

    protected final void sendFiles(String containerId, String localDirPath, List<String> filePaths,
            String remoteDirPath) {
        DockerClient.getInstance().sendFiles(containerId, localDirPath, filePaths, remoteDirPath);
    }

    protected final void sendFolder(String containerId, String localDirPath, String remoteDirPath) {
        DockerClient.getInstance().sendFolder(containerId, localDirPath, remoteDirPath);
    }

    protected final Map<String, byte[]> retrieveFiles(String containerId, String remoteDirPath) throws IOException {
        return DockerClient.getInstance().retrieveFiles(containerId, remoteDirPath);
    }

    protected final byte[] retrieveFile(String containerId, String remoteFilePath) throws IOException {
        return DockerClient.getInstance().retrieveFile(containerId, remoteFilePath);
    }

    protected final Optional<Container> getContainer(String containerName) {
        return DockerClient.getInstance().getContainer(containerName);
    }

    protected final boolean isContainerUp(String containerName) {
        return DockerClient.getInstance().isContainerUp(containerName);
    }

    protected final String getContainerId(String containerName) throws NoSuchElementException {
        return DockerClient.getInstance().getContainerId(containerName);
    }

    protected final void handleErrors(ByteArrayOutputStream errorStream, String execId, Logger logger) {
        long commandErrorCode = getCommandErrorCode(execId);
        if (0 != commandErrorCode) {
            throw new RuntimeException("Docker exec command returned '" + commandErrorCode
                    + "' and wrote the following to stderr:\n" + errorStream.toString());
        } else {
            logger.warn("Docker exec command returned '0' but wrote the following to stderr:\n{}", errorStream);
        }
    }

}
