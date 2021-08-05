package uk.ac.cam.cares.jps.agent.aqmesh;


import org.junit.Assert;
import org.junit.ClassRule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;

import java.io.FileNotFoundException;
import java.io.FileWriter;
import java.io.IOException;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.net.URISyntaxException;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Objects;

public class AQMeshAPIConnectorTest {


    // Temporary folder to place a properties file (same file for all potential tests)
    @ClassRule
    public static TemporaryFolder folder = new TemporaryFolder();

    @Test
    public void AQMeshAPIConnectorConstructorTest() throws NoSuchFieldException, IllegalAccessException, URISyntaxException, IOException {
        // One connector constructed using the username and password directly
        AQMeshAPIConnector connector = new AQMeshAPIConnector("username", "password", "url");
        // One connector constructed using a properties file
        AQMeshAPIConnector connectorFile = new AQMeshAPIConnector(Paths.get(Objects.requireNonNull(getClass().getResource("/aqmesh.properties")).
                toURI()).toString());

        // Retrieve private fields for username and password and check that they were set correctly
        Field usernameField = AQMeshAPIConnector.class.getDeclaredField("username");
        usernameField.setAccessible(true);
        Assert.assertEquals("username", usernameField.get(connector));
        Assert.assertEquals("username", usernameField.get(connectorFile));

        Field passwordField = AQMeshAPIConnector.class.getDeclaredField("password");
        passwordField.setAccessible(true);
        Assert.assertEquals("password", passwordField.get(connector));
        Assert.assertEquals("password", passwordField.get(connectorFile));

        Field urlField = AQMeshAPIConnector.class.getDeclaredField("api_url");
        urlField.setAccessible(true);
        Assert.assertEquals("url", urlField.get(connector));
        Assert.assertEquals("url", urlField.get(connectorFile));
    }

    @Test
    public void loadAPIConfigsTest() throws NoSuchMethodException, IllegalAccessException, IOException, NoSuchFieldException {
        AQMeshAPIConnector connector = new AQMeshAPIConnector("username", "password", "url");
        // Filepath to not yet created file in temporary test folder
        String filepath = Paths.get(folder.getRoot().toString(), "aqmesh.properties").toString();
        // Error messages
        String fileNotFound = "No properties file found at specified filepath: " + filepath;
        String noUsername = "Properties file is missing \"aqmesh.username=<aqmesh_username>\"";
        String noPassword = "Properties file is missing \"aqmesh.password=<aqmesh_password>\"";
        String noURL = "Properties file is missing \"aqmesh.url=<aqmesh_url>\"";

        // Set private method to be accessible
        Method loadAPIConfig = AQMeshAPIConnector.class.getDeclaredMethod("loadAPIConfigs", String.class);
        loadAPIConfig.setAccessible(true);

        // Test for non-existing properties file
        try {
            loadAPIConfig.invoke(connector, filepath);
            Assert.fail();
        } catch (InvocationTargetException e) {
            Assert.assertEquals(FileNotFoundException.class, e.getCause().getClass());
            Assert.assertEquals(fileNotFound, e.getCause().getMessage());
        }

        // Test for missing username by creating a file only containing password
        writePropertyFile(filepath, Collections.singletonList("aqmesh.password=test_password"));
        // Try loading RDB configs
        try {
            loadAPIConfig.invoke(connector, filepath);
            Assert.fail();
        } catch (InvocationTargetException e) {
            Assert.assertEquals(IOException.class, e.getCause().getClass());
            Assert.assertEquals(noUsername, e.getCause().getMessage());
        }

        // Test for missing password by creating a file only containing user
        writePropertyFile(filepath, Collections.singletonList("aqmesh.username=test_user"));
        // Try loading RDB configs
        try {
            loadAPIConfig.invoke(connector, filepath);
            Assert.fail();
        } catch (InvocationTargetException e) {
            Assert.assertEquals(IOException.class, e.getCause().getClass());
            Assert.assertEquals(noPassword, e.getCause().getMessage());
        }

        // Test for missing URL by creating a file only containing user and password
        writePropertyFile(filepath, Arrays.asList("aqmesh.username=test_user", "aqmesh.password=test_password"));
        // Try loading RDB configs
        try {
            loadAPIConfig.invoke(connector, filepath);
            Assert.fail();
        } catch (InvocationTargetException e) {
            Assert.assertEquals(IOException.class, e.getCause().getClass());
            Assert.assertEquals(noURL, e.getCause().getMessage());
        }

        // Test for proper username and password
        writePropertyFile(filepath, Arrays.asList("aqmesh.username=test_user", "aqmesh.password=test_password", "aqmesh.url=test_url"));
        // Try loading RDB configs
        try {
            loadAPIConfig.invoke(connector, filepath);
        } catch (Exception e) {
            Assert.fail(e.getMessage());
        }

        // Retrieve private fields for username and password and check that they were set correctly
        Field usernameField = AQMeshAPIConnector.class.getDeclaredField("username");
        usernameField.setAccessible(true);
        Assert.assertEquals("test_user", usernameField.get(connector));

        Field passwordField = AQMeshAPIConnector.class.getDeclaredField("password");
        passwordField.setAccessible(true);
        Assert.assertEquals("test_password", passwordField.get(connector));

        Field urlField = AQMeshAPIConnector.class.getDeclaredField("api_url");
        urlField.setAccessible(true);
        Assert.assertEquals("test_url", urlField.get(connector));
    }

    private void writePropertyFile(String filepath, List<String> properties) throws IOException {
        // Overwrite potentially existing properties file
        FileWriter writer = new FileWriter(filepath, false);
        // Populate file
        for (String s : properties) {
            writer.write(s + "\n");
        }
        // Close the file and return the file
        writer.close();
    }

}
