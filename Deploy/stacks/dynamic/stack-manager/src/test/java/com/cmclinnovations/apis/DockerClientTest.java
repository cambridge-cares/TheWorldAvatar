package com.cmclinnovations.apis;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.Map;

import org.junit.AfterClass;
import org.junit.Assert;
import org.junit.BeforeClass;
import org.junit.Test;

import com.github.dockerjava.api.command.CreateContainerCmd;
import com.github.dockerjava.api.command.PullImageCmd;
import com.github.dockerjava.api.command.PullImageResultCallback;
import com.github.dockerjava.api.command.RemoveContainerCmd;
import com.github.dockerjava.api.command.StartContainerCmd;

public class DockerClientTest {

    private static final String TEST_MESSAGE = "testing123\n";

    private static DockerClient dockerAPI;
    private static String containerId;

    @BeforeClass
    public static void setup() {
        dockerAPI = new DockerClient();

        String image = "busybox:latest";

        com.github.dockerjava.api.DockerClient dockerClient = dockerAPI.getInternalClient();

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

        try (CreateContainerCmd createContainerCmd = dockerClient.createContainerCmd(image)) {
            containerId = createContainerCmd.withName("testContainer")
                    .withCmd("tail", "-f", "/dev/null")
                    .exec().getId();
            try (StartContainerCmd startContainerCmd = dockerClient.startContainerCmd(containerId)) {
                startContainerCmd.exec();
            }
        }
    }

    @AfterClass
    public static void tearDown() throws InterruptedException {
        if (null != dockerAPI && null != containerId) {
            try (RemoveContainerCmd removeContainerCmd = dockerAPI.getInternalClient()
                    .removeContainerCmd(containerId)) {
                removeContainerCmd.withForce(true)
                        .withRemoveVolumes(true)
                        .exec();
            }
        }
    }

    @Test
    public void dirTest() {
        String directoryPath = "/test/dir";

        Assert.assertFalse(dockerAPI.directoryExists(containerId, directoryPath));
        dockerAPI.makeDir(containerId, directoryPath);
        Assert.assertTrue(dockerAPI.directoryExists(containerId, directoryPath));
        dockerAPI.deleteDirectory(containerId, directoryPath);
        Assert.assertFalse(dockerAPI.directoryExists(containerId, directoryPath));
    }

    @Test
    public void fileTest() throws IOException {
        String directoryPath = "/test/";
        String filename = "file.txt";
        String filePath = directoryPath + filename;

        Assert.assertFalse(dockerAPI.fileExists(containerId, filePath));
        dockerAPI.sendFiles(containerId, Map.of(filename, TEST_MESSAGE.getBytes()), directoryPath);
        Assert.assertTrue(dockerAPI.fileExists(containerId, filePath));
        dockerAPI.deleteFile(containerId, filePath);
        Assert.assertFalse(dockerAPI.fileExists(containerId, filePath));
    }

    @Test
    public void testExecuteCommandSimpleTrue() {
        Assert.assertEquals(0, dockerAPI.getCommandErrorCode(dockerAPI.executeSimpleCommand(containerId, "true")));
    }

    @Test
    public void testExecuteCommandSimpleFalse() {
        Assert.assertEquals(1, dockerAPI.getCommandErrorCode(dockerAPI.executeSimpleCommand(containerId, "false")));
    }

    @Test
    public void testExecuteCommandHereDoc() {

        ByteArrayOutputStream outputStream = new ByteArrayOutputStream();

        dockerAPI.createComplexCommand(containerId, "cat", "-")
                .withOutputStream(outputStream)
                .withErrorStream(outputStream)
                .withHereDocument(TEST_MESSAGE)
                .exec();

        Assert.assertEquals(TEST_MESSAGE, outputStream.toString());
    }

    @Test
    public void testExecuteCommandIOFromString() {

        ByteArrayInputStream inputStream = new ByteArrayInputStream(TEST_MESSAGE.getBytes());

        ByteArrayOutputStream outputStream = new ByteArrayOutputStream();

        dockerAPI.createComplexCommand(containerId, "head", "-n", "1")
                .withInputStream(inputStream)
                .withOutputStream(outputStream)
                .withErrorStream(outputStream)
                .exec();

        Assert.assertEquals(TEST_MESSAGE, outputStream.toString());
    }

    @Test
    public void testExecuteCommandIOFromFile() {

        InputStream inputStream = DockerClientTest.class.getResourceAsStream("testFile.txt");

        OutputStream outputStream = new ByteArrayOutputStream();

        dockerAPI.createComplexCommand(containerId, "head", "-n", "3")
                .withInputStream(inputStream)
                .withOutputStream(outputStream)
                .withErrorStream(outputStream)
                .exec();

        String result = outputStream.toString();
        String expected = "1.abcd\r\n2.efdh\r\n3.abcd\r\n";
        Assert.assertEquals(expected, result);
    }

}
