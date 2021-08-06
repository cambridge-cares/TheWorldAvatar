package uk.ac.cam.cares.jps.agent.aqmesh;

import com.github.tomakehurst.wiremock.client.ResponseDefinitionBuilder;
import com.github.tomakehurst.wiremock.junit.WireMockRule;
import org.apache.http.HttpHeaders;
import org.apache.http.client.HttpResponseException;
import org.json.JSONArray;
import org.json.JSONException;
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
    // Mocking objects to mock AQMesh API calls
    @Rule
    public WireMockRule aqMeshAPIMock = new WireMockRule(PORT);

    private AQMeshAPIConnector testConnector;
    private static final String TEST_TOKEN = "token";

    @Before
    public void initializeTestConnector() {
        testConnector = new AQMeshAPIConnector("username", "password", TEST_URL);
    }

    @After
    public void resetAPIMock() {
        aqMeshAPIMock.resetAll();
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
        responseBody.put(AQMeshAPIConnector.TOKEN_KEY, "test_token");

        // The API returns a proper token ...
        setTokenAPIMock(ok()
                .withHeader("Content-Type", "application/json; charset=utf-8")
                .withBody(responseBody.toString()));
        // ... but the ping does not work
        try {
            testConnector.connect();
            Assert.fail();
        }
        catch (JPSRuntimeException e) {
            Assert.assertEquals("Unable to connect to AQMesh API!", e.getMessage());
            // Check also the cause
            Assert.assertEquals(HttpResponseException.class, e.getCause().getClass());
            Assert.assertTrue(e.getCause().getMessage().contains("Unexpected status code."));
        }
        aqMeshAPIMock.resetAll();

        // The API returns a proper token ...
        setTokenAPIMock(ok()
                .withHeader("Content-Type", "application/json; charset=utf-8")
                .withBody(responseBody.toString()));
        // ... and the ping works properly
        aqMeshAPIMock.stubFor(get(urlPathEqualTo("/" + AQMeshAPIConnector.PING_PATH))
                .atPriority(2)
                .withHeader(HttpHeaders.AUTHORIZATION, equalToIgnoreCase("Bearer test_token"))
                .willReturn(ok().withBody("{ \"server_time\": '2021-08-05T04:24:01.75' }")));

        testConnector.connect();
        // Check whether token field was set correctly
        Assert.assertEquals(responseBody.get(AQMeshAPIConnector.TOKEN_KEY), testConnector.getToken());

        // The API has an error //
        // Bad request
        setTokenAPIMock(badRequest());
        try {
            testConnector.connect();
            Assert.fail();
        }
        catch (JPSRuntimeException e) {
            Assert.assertEquals("Unable to connect to AQMesh API!", e.getMessage());
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
            Assert.assertEquals("Unable to connect to AQMesh API!", e.getMessage());
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
            Assert.assertEquals("Unable to connect to AQMesh API!", e.getMessage());
            // Check also the cause
            Assert.assertEquals(HttpResponseException.class, e.getCause().getClass());
            Assert.assertTrue(e.getCause().getMessage().contains("Could not retrieve access token."));
        }
    }

    @Test
    public void testPing() throws IOException, NoSuchFieldException, IllegalAccessException {
        // Ping without set token
        try {
            testConnector.ping();
        }
        catch (Exception e) {
            Assert.assertEquals(JPSRuntimeException.class, e.getClass());
            Assert.assertEquals("Token is not set. Use the connect method first.", e.getMessage());
        }

        // Set a token to avoid needing to invoke connect
        Field tokenField = AQMeshAPIConnector.class.getDeclaredField("token");
        tokenField.setAccessible(true);
        tokenField.set(testConnector, TEST_TOKEN);

        // The server returns an error
        try {
            testConnector.ping();
        }
        catch (Exception e) {
            Assert.assertEquals(HttpResponseException.class, e.getClass());
            Assert.assertTrue(e.getMessage().contains("Unexpected status code."));
        }
        // The server returns a proper response
        String serverTime = "2021-08-05T04:24:01.75";
        aqMeshAPIMock.stubFor(get(urlPathEqualTo("/" + AQMeshAPIConnector.PING_PATH))
                .withHeader(HttpHeaders.AUTHORIZATION, equalTo("Bearer " + TEST_TOKEN))
                .willReturn(ok().withBody("{ \"server_time\": '" + serverTime + "' }")));
        Assert.assertEquals(serverTime, testConnector.ping());
    }

    @Test
    public void testSetLocation() throws NoSuchFieldException, IllegalAccessException, NoSuchMethodException {
        Assert.assertEquals("", testConnector.getLocation());
        // Set a token to avoid needing to invoke connect
        Field tokenField = AQMeshAPIConnector.class.getDeclaredField("token");
        tokenField.setAccessible(true);
        tokenField.set(testConnector, TEST_TOKEN);
        // Set the method to be invokable
        Method setLocation = AQMeshAPIConnector.class.getDeclaredMethod("setLocation");
        setLocation.setAccessible(true);

        // API returns an error (not status 200)
        aqMeshAPIMock.stubFor(get(urlPathEqualTo("/" + AQMeshAPIConnector.ASSETS_PATH))
                .withHeader(HttpHeaders.AUTHORIZATION, equalTo("Bearer " + TEST_TOKEN))
                .willReturn(badRequest()));
        try {
            setLocation.invoke(testConnector);
        }
        catch (InvocationTargetException e) {
            Assert.assertEquals(HttpResponseException.class, e.getCause().getClass());
            Assert.assertTrue(e.getCause().getMessage().contains("Could not retrieve location number."));
        }

        // API returns no assets
        aqMeshAPIMock.stubFor(get(urlPathEqualTo("/" + AQMeshAPIConnector.ASSETS_PATH))
                .withHeader(HttpHeaders.AUTHORIZATION, equalTo("Bearer " + TEST_TOKEN))
                .willReturn(ok().withBody(new JSONArray().toString())));
        try {
            setLocation.invoke(testConnector);
        }
        catch (InvocationTargetException e) {
            Assert.assertEquals(JSONException.class, e.getCause().getClass());
            Assert.assertTrue(e.getCause().getMessage().contains("No assets available in returned JSON Array."));
        }
        // API returns proper response
        int location = 12345;
        JSONArray responseBody = new JSONArray();
        JSONObject asset = new JSONObject();
        asset.put(AQMeshAPIConnector.LOCATION_KEY, location);
        responseBody.put(asset);
        aqMeshAPIMock.stubFor(get(urlPathEqualTo("/" + AQMeshAPIConnector.ASSETS_PATH))
                .withHeader(HttpHeaders.AUTHORIZATION, equalTo("Bearer " + TEST_TOKEN))
                .willReturn(ok().withBody(responseBody.toString())));
        try {
            setLocation.invoke(testConnector);
            Assert.assertEquals(Integer.toString(location), testConnector.getLocation());
        } catch (InvocationTargetException e) {
            Assert.fail(e.getMessage());
        }
    }

    private void setTokenAPIMock(ResponseDefinitionBuilder response) {
        // Expected request body
        JSONObject requestBody = new JSONObject();
        requestBody.put("username", "username");
        requestBody.put("password", "password");

        aqMeshAPIMock.stubFor(post(urlPathEqualTo("/" + AQMeshAPIConnector.AUTHENTICATE_PATH))
                .atPriority(1)
                .withHeader("Content-Type", equalToIgnoreCase("application/json; charset=utf-8"))
                .withRequestBody(equalToJson(requestBody.toString(), true, false))
                .willReturn(response));
    }

}
