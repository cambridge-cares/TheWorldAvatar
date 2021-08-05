package uk.ac.cam.cares.jps.agent.aqmesh;


import com.github.tomakehurst.wiremock.client.ResponseDefinitionBuilder;
import com.github.tomakehurst.wiremock.junit.WireMockRule;
import org.apache.http.client.HttpResponseException;
import org.json.JSONObject;
import org.junit.*;
import org.junit.rules.TemporaryFolder;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

import static com.github.tomakehurst.wiremock.client.WireMock.*;

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

    // Fields used for the mock API calls
    private static final int PORT = 8089;
    private static final String TEST_URL = "http://localhost:" + PORT + "/";
    // Mocking object to mock AQMesh API calls
    @Rule
    public WireMockRule accessTokenMock = new WireMockRule(PORT);

    private AQMeshAPIConnector testConnector;
    private String authenticatePath;

    @Before
    public void initializeTestConnector() throws NoSuchFieldException, IllegalAccessException {
        testConnector = new AQMeshAPIConnector("username", "password", TEST_URL);

        // Access static field to create the authentication path
        Field authenticateField = AQMeshAPIConnector.class.getDeclaredField("AUTHENTICATE_PATH");
        authenticateField.setAccessible(true);
        authenticatePath = "/" + authenticateField.get(testConnector).toString();
    }

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
            loadAPIConfig.invoke(testConnector, filepath);
            Assert.fail();
        } catch (InvocationTargetException e) {
            Assert.assertEquals(FileNotFoundException.class, e.getCause().getClass());
            Assert.assertEquals(fileNotFound, e.getCause().getMessage());
        }

        // Test for missing username by creating a file only containing password
        writePropertyFile(filepath, Collections.singletonList("aqmesh.password=test_password"));
        // Try loading RDB configs
        try {
            loadAPIConfig.invoke(testConnector, filepath);
            Assert.fail();
        } catch (InvocationTargetException e) {
            Assert.assertEquals(IOException.class, e.getCause().getClass());
            Assert.assertEquals(noUsername, e.getCause().getMessage());
        }

        // Test for missing password by creating a file only containing user
        writePropertyFile(filepath, Collections.singletonList("aqmesh.username=test_user"));
        // Try loading RDB configs
        try {
            loadAPIConfig.invoke(testConnector, filepath);
            Assert.fail();
        } catch (InvocationTargetException e) {
            Assert.assertEquals(IOException.class, e.getCause().getClass());
            Assert.assertEquals(noPassword, e.getCause().getMessage());
        }

        // Test for missing URL by creating a file only containing user and password
        writePropertyFile(filepath, Arrays.asList("aqmesh.username=test_user", "aqmesh.password=test_password"));
        // Try loading RDB configs
        try {
            loadAPIConfig.invoke(testConnector, filepath);
            Assert.fail();
        } catch (InvocationTargetException e) {
            Assert.assertEquals(IOException.class, e.getCause().getClass());
            Assert.assertEquals(noURL, e.getCause().getMessage());
        }

        // Test for proper username and password
        writePropertyFile(filepath, Arrays.asList("aqmesh.username=test_user", "aqmesh.password=test_password", "aqmesh.url=test_url"));
        // Try loading RDB configs
        try {
            loadAPIConfig.invoke(testConnector, filepath);
        } catch (Exception e) {
            Assert.fail(e.getMessage());
        }

        // Retrieve private fields for username and password and check that they were set correctly
        Field usernameField = AQMeshAPIConnector.class.getDeclaredField("username");
        usernameField.setAccessible(true);
        Assert.assertEquals("test_user", usernameField.get(testConnector));

        Field passwordField = AQMeshAPIConnector.class.getDeclaredField("password");
        passwordField.setAccessible(true);
        Assert.assertEquals("test_password", passwordField.get(testConnector));

        Field urlField = AQMeshAPIConnector.class.getDeclaredField("api_url");
        urlField.setAccessible(true);
        Assert.assertEquals("test_url", urlField.get(testConnector));
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

    @Test
    public void testConnect() {
        // Mock response body
        JSONObject responseBody = new JSONObject();
        responseBody.put("token", "test_token");

        // The API returns a proper token
        setTokenAPIMock(ok()
                .withHeader("Content-Type", "application/json; charset=utf-8")
                .withBody(responseBody.toString()));
        testConnector.connect();
        // Check whether token field was set correctly
        Assert.assertEquals(responseBody.get("token"), testConnector.getToken());

        // The API has an error //
        // Bad request
        setTokenAPIMock(badRequest());
        try {
            testConnector.connect();
            Assert.fail();
        }
        catch (JPSRuntimeException e) {
            Assert.assertEquals("Access token for AQMesh API could not be retrieved!", e.getMessage());
            // Check also the cause
            Assert.assertEquals(HttpResponseException.class, e.getCause().getClass());
            Assert.assertTrue(e.getCause().getMessage().contains("Invalid username or password."));
        }
        // Unauthorized
        setTokenAPIMock(unauthorized());
        try {
            testConnector.connect();
            Assert.fail();
        }
        catch (JPSRuntimeException e) {
            Assert.assertEquals("Access token for AQMesh API could not be retrieved!", e.getMessage());
            // Check also the cause
            Assert.assertEquals(HttpResponseException.class, e.getCause().getClass());
            Assert.assertTrue(e.getCause().getMessage().contains("Invalid username or password."));
        }
        // Server error
        setTokenAPIMock(serverError());
        try {
            testConnector.connect();
            Assert.fail();
        }
        catch (JPSRuntimeException e) {
            Assert.assertEquals("Access token for AQMesh API could not be retrieved!", e.getMessage());
            // Check also the cause
            Assert.assertEquals(HttpResponseException.class, e.getCause().getClass());
            Assert.assertTrue(e.getCause().getMessage().contains("Could not retrieve token."));
        }
    }

    private void setTokenAPIMock(ResponseDefinitionBuilder response) {
        // Expected request body
        JSONObject requestBody = new JSONObject();
        requestBody.put("username", "username");
        requestBody.put("password", "password");

        accessTokenMock.stubFor(post(urlPathEqualTo(authenticatePath))
                .withHeader("Content-Type", equalToIgnoreCase("application/json; charset=utf-8"))
                .withRequestBody(equalToJson(requestBody.toString(), true, false))
                .willReturn(response));
    }

}
