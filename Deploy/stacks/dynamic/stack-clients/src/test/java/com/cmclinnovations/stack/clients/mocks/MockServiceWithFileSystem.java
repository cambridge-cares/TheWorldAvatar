package com.cmclinnovations.stack.clients.mocks;

import java.io.IOException;
import java.time.Duration;
import java.util.HashMap;
import java.util.Map;
import java.util.stream.Stream;

import org.junit.jupiter.api.Assertions;
import org.testcontainers.containers.GenericContainer;
import org.testcontainers.containers.startupcheck.IsRunningStartupCheckStrategy;
import org.testcontainers.utility.DockerImageName;

import com.cmclinnovations.stack.clients.core.StackClient;
import com.cmclinnovations.stack.clients.docker.DockerClient;

public class MockServiceWithFileSystem extends GenericContainer<MockServiceWithFileSystem> {

    @SuppressWarnings({ "null" })
    public MockServiceWithFileSystem(String name) {
        super(DockerImageName.parse("ubuntu:22.04"));
        withCommand("tail -f /dev/null");
        withCreateContainerCmdModifier(cmd -> cmd.withName(StackClient.prependStackName(name, "-"))
                .withLabels(new HashMap<>(StackClient.getStackNameLabelMap())));
        withStartupCheckStrategy(new IsRunningStartupCheckStrategy().withTimeout(Duration.ofSeconds(10)));
    }

    public void assertFileContent(byte[] expectedFile, String remoteFilePath) {
        try {
            DockerClient dockerClient = DockerClient.getInstance();
            byte[] actualFile = dockerClient.retrieveFile(getContainerId(), remoteFilePath);

            Assertions.assertArrayEquals(expectedFile, actualFile);
        } catch (IOException e) {
            Assertions.fail(e);
        }
    }

    public void assertFileContent(String expectedFile, String remoteFilePath) {
        try {
            DockerClient dockerClient = DockerClient.getInstance();
            String actualFile = new String(dockerClient.retrieveFile(getContainerId(), remoteFilePath));

            Assertions.assertEquals(expectedFile, actualFile);
        } catch (IOException e) {
            Assertions.fail(e);
        }
    }

    public void assertDirContent(Map<String, String> expectedFiles, String remoteDirPath) {
        try {
            DockerClient dockerClient = DockerClient.getInstance();
            Object[] actualFiles = dockerClient
                    .retrieveFiles(getContainerId(), remoteDirPath).entrySet()
                    .stream().map(e -> e.getKey() + "=" + new String(e.getValue())).sorted().toArray();

            Object[] expectedFiles2 = expectedFiles.entrySet().stream()
                    .map(e -> remoteDirPath + e.getKey() + "=" + e.getValue()).sorted().toArray();

            Assertions.assertEquals(expectedFiles2.length, actualFiles.length);

            Assertions.assertAll(Stream.iterate(0, i -> ++i).limit(expectedFiles2.length)
                    .map(i -> () -> Assertions.assertEquals(expectedFiles2[i], actualFiles[i])));
        } catch (IOException e) {
            Assertions.fail(e);
        }
    }

}
