package com.cmclinnovations.stack.clients.docker;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.util.List;
import java.util.Map;

import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.SystemUtils;
import org.junit.AfterClass;
import org.junit.Assert;
import org.junit.Assume;
import org.junit.BeforeClass;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;
import org.junit.rules.TestName;

import com.cmclinnovations.stack.clients.core.StackClient;
import com.github.dockerjava.api.command.CreateContainerCmd;
import com.github.dockerjava.api.command.PullImageCmd;
import com.github.dockerjava.api.command.PullImageResultCallback;
import com.github.dockerjava.api.command.RemoveContainerCmd;
import com.github.dockerjava.api.command.StartContainerCmd;

public class DockerClientTest {

    @Rule
    public TemporaryFolder tempFolder = new TemporaryFolder();

    @Rule
    public TestName testName = new TestName();

    private static final String TEST_MESSAGE = "testing123\n";

    private static DockerClient dockerAPI;
    private static String containerId;

    @BeforeClass
    public static void setup() {
        StackClient.setInStack(false);
        dockerAPI = DockerClient.getInstance();

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
        String remoteDir = "/" + testName.getMethodName() + "/dir";

        Assert.assertFalse(dockerAPI.directoryExists(containerId, remoteDir));
        dockerAPI.makeDir(containerId, remoteDir);
        Assert.assertTrue(dockerAPI.directoryExists(containerId, remoteDir));
        dockerAPI.deleteDirectory(containerId, remoteDir);
        Assert.assertFalse(dockerAPI.directoryExists(containerId, remoteDir));
    }

    @Test
    public void testSendFilesContent() {
        String remoteBaseDir = "/" + testName.getMethodName() + "/";
        String filename = "file.txt";
        String filePath = remoteBaseDir + filename;

        Assert.assertFalse(dockerAPI.fileExists(containerId, filePath));
        dockerAPI.sendFilesContent(containerId, Map.of(filename, TEST_MESSAGE.getBytes()), remoteBaseDir);
        Assert.assertTrue(dockerAPI.fileExists(containerId, filePath));
        dockerAPI.deleteFile(containerId, filePath);
        Assert.assertFalse(dockerAPI.fileExists(containerId, filePath));
    }

    @Test
    public void testSendFiles() throws IOException {
        String remoteBaseDir = "/" + testName.getMethodName() + "/";
        String dir1 = "testfolder/";
        String dir2 = "testfolder2/";

        File testDir2 = tempFolder.newFolder(dir1, dir2);
        File testFile2_1 = createFile(testDir2, "testFile2.1.txt", "Test text 2.1");
        createFile(testDir2, "testFile2.2.txt", "Test text 2.2");
        File testDir1 = testDir2.getParentFile();
        File testFile1 = createFile(testDir1, "testFile1.txt", "Test text 1");

        Assert.assertFalse(dockerAPI.directoryExists(containerId, remoteBaseDir + dir1 + dir2));

        Path rootPath = tempFolder.getRoot().toPath();
        dockerAPI.sendFiles(containerId,
                rootPath.toString(), List.of(
                        tempFolder.getRoot().toPath().relativize(testFile1.toPath()).toString(),
                        tempFolder.getRoot().toPath().relativize(testFile2_1.toPath()).toString()),
                remoteBaseDir);

        Assert.assertTrue(dockerAPI.fileExists(containerId, remoteBaseDir + dir1 + "testFile1.txt"));
        Assert.assertTrue(dockerAPI.fileExists(containerId, remoteBaseDir + dir1 + dir2 + "testFile2.1.txt"));
        Assert.assertFalse(dockerAPI.fileExists(containerId, remoteBaseDir + dir1 + dir2 + "testFile2.2.txt"));
        dockerAPI.deleteDirectory(containerId, remoteBaseDir);
        Assert.assertFalse(dockerAPI.directoryExists(containerId, remoteBaseDir));
    }

    @Test
    public void testSendFolder() throws IOException {
        String remoteBaseDir = "/" + testName.getMethodName() + "/";
        String dir1 = "testfolder/";
        String dir2 = "testfolder2/";

        File testDir2 = tempFolder.newFolder(dir1, dir2);
        createFile(testDir2, "testFile2.1.txt", "Test text 2.1");
        createFile(testDir2, "testFile2.2.txt", "Test text 2.2");
        File testDir1 = testDir2.getParentFile();
        createFile(testDir1, "testFile1.txt", "Test text 1");

        Assert.assertFalse(dockerAPI.directoryExists(containerId, remoteBaseDir + dir1 + dir2));
        dockerAPI.sendFolder(containerId, tempFolder.getRoot().toString(), remoteBaseDir);
        Assert.assertTrue(dockerAPI.fileExists(containerId, remoteBaseDir + dir1 + dir2 + "testFile2.1.txt"));
        Assert.assertTrue(dockerAPI.fileExists(containerId, remoteBaseDir + dir1 + dir2 + "testFile2.2.txt"));
        Assert.assertTrue(dockerAPI.fileExists(containerId, remoteBaseDir + dir1 + "testFile1.txt"));
        dockerAPI.deleteDirectory(containerId, remoteBaseDir);
        Assert.assertFalse(dockerAPI.directoryExists(containerId, remoteBaseDir));
    }

    private File createFile(File dir, String filename, String data) throws IOException {
        File file = new File(dir, filename);
        FileUtils.writeStringToFile(file, data, StandardCharsets.UTF_8);
        return file;
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
    public void testExecuteLongCommand() {
        Assert.assertEquals(0, dockerAPI
                .getCommandErrorCode(dockerAPI.createComplexCommand(containerId, "sh", "-c", "sleep 2 && true")
                        .withEvaluationTimeout(1).exec()));
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

        Assume.assumeFalse(
                "Passing a non-null value to ComplexCommand::withInputStream doesn't work when calling Docker via the Windows named pipe.",
                SystemUtils.IS_OS_WINDOWS);

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

        Assume.assumeFalse(
                "Passing a non-null value to ComplexCommand::withInputStream doesn't work when calling Docker via the Windows named pipe.",
                SystemUtils.IS_OS_WINDOWS);

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
