package com.cmclinnovations.stack.clients.mocks;

import java.io.IOException;
import java.time.Duration;
import java.util.HashMap;

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

    public void assertFileContent(String expectedFile, String remoteFilePath) {
        try {
            DockerClient dockerClient = DockerClient.getInstance();
            String actualFile = new String(dockerClient.retrieveFile(getContainerId(), remoteFilePath));

            Assertions.assertEquals(expectedFile, actualFile);
        } catch (IOException e) {
            Assertions.fail(e);
        }
    }

}
