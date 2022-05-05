package uk.ac.cam.cares.jps.agent.thingsboard;

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
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

public class ThingsBoardAPIConnectorTest {


    // Temporary folder to place a properties file
    @Rule
    public TemporaryFolder folder = new TemporaryFolder();

    // Fields used for the mock API calls
    private static final int PORT = 8089;
    private static final String PATH_URL = "http://localhost:" + PORT + "/";

    // Mocking objects to mock AQMesh API calls
    @Rule
    public WireMockRule thingsBoardAPIMock = new WireMockRule(PORT);

    private ThingsBoardAPIConnector testConnector;
    private static final String TEST_TOKEN = "token";
    
  
    @Before
    public void initializeTestConnector() {
        testConnector = new ThingsBoardAPIConnector("username", "password", PATH_URL, "device_id", "keys");
    
    }

    @After
    public void resetAPIMock() {
        thingsBoardAPIMock.resetAll();
    }
    

    @Test
    public void ThingsBoardAPIConnectorConstructorTest() throws NoSuchFieldException, IllegalAccessException, IOException {
        // One connector constructed using the username, password, auth_url, api_url and device token directly
        ThingsBoardAPIConnector connector = new ThingsBoardAPIConnector("username", "password", "path_url", "device_id", "keys");
        // One connector constructed using a properties file
        String propertiesFile = Paths.get(folder.getRoot().toString(), "api.properties").toString();
        writePropertyFile(propertiesFile, Arrays.asList("thingsboard.username=username", "thingsboard.password=password", "path.url=path_url", "device.id=device_id", "keys=keys"));
        ThingsBoardAPIConnector connectorFile = new ThingsBoardAPIConnector(propertiesFile);

        // Retrieve private fields for username and password and check that they were set correctly
        Field usernameField = ThingsBoardAPIConnector.class.getDeclaredField("username");
        usernameField.setAccessible(true);
        Assert.assertEquals("username", usernameField.get(connector));
        Assert.assertEquals("username", usernameField.get(connectorFile));
        
        Field passwordField = ThingsBoardAPIConnector.class.getDeclaredField("password");
        passwordField.setAccessible(true);
        Assert.assertEquals("password", passwordField.get(connector));
        Assert.assertEquals("password", passwordField.get(connectorFile));

        Field pathUrlField = ThingsBoardAPIConnector.class.getDeclaredField("path_url");
        pathUrlField.setAccessible(true);
        Assert.assertEquals("path_url", pathUrlField.get(connector));
        Assert.assertEquals("path_url", pathUrlField.get(connectorFile));
        
        Field deviceIdField = ThingsBoardAPIConnector.class.getDeclaredField("device_id");
        deviceIdField.setAccessible(true);
        Assert.assertEquals("device_id", deviceIdField.get(connector));
        Assert.assertEquals("device_id", deviceIdField.get(connectorFile));
        
        Field keysField = ThingsBoardAPIConnector.class.getDeclaredField("keys");
        keysField.setAccessible(true);
        Assert.assertEquals("keys", keysField.get(connector));
        Assert.assertEquals("keys", keysField.get(connectorFile));
        
    }

    @Test
    public void loadAPIConfigsTest() throws NoSuchMethodException, IllegalAccessException, IOException, NoSuchFieldException {
        // Filepath to not yet created file in temporary test folder
        String filepath = Paths.get(folder.getRoot().toString(), "thingsboard.properties").toString();
        // Error messages
        String fileNotFound = "No properties file found at specified filepath: " + filepath;
        String noUsername = "Properties file is missing \"thingsboard.username=<thingsboard_username>\"";
        String noPassword = "Properties file is missing \"thingsboard.password=<thingsboard_password>\"";
        String noPathURL = "Properties file is missing \"path.url=<path_url>\"";
        String noDeviceId = "Properties file is missing \"device.id=<device_id>\"";
        String noKeys = "Properties file is missing \"keys=<keys>\"";

        // Set private method to be accessible
        Method loadAPIConfig = ThingsBoardAPIConnector.class.getDeclaredMethod("loadAPIConfigs", String.class);
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
        writePropertyFile(filepath, Collections.singletonList("thingsboard.password=test_password"));
        // Try loading RDB configs
        try {
            loadAPIConfig.invoke(testConnector, filepath);
            Assert.fail();
        } catch (InvocationTargetException e) {
            Assert.assertEquals(IOException.class, e.getCause().getClass());
            Assert.assertEquals(noUsername, e.getCause().getMessage());
        }

        // Test for missing password by creating a file only containing user
        writePropertyFile(filepath, Collections.singletonList("thingsboard.username=test_user"));
        // Try loading RDB configs
        try {
            loadAPIConfig.invoke(testConnector, filepath);
            Assert.fail();
        } catch (InvocationTargetException e) {
            Assert.assertEquals(IOException.class, e.getCause().getClass());
            Assert.assertEquals(noPassword, e.getCause().getMessage());
        }

        // Test for missing URL by creating a file only containing user and password
        writePropertyFile(filepath, Arrays.asList("thingsboard.username=test_user", "thingsboard.password=test_password"));
        // Try loading RDB configs
        try {
            loadAPIConfig.invoke(testConnector, filepath);
            Assert.fail();
        } catch (InvocationTargetException e) {
            Assert.assertEquals(IOException.class, e.getCause().getClass());
            Assert.assertEquals(noPathURL, e.getCause().getMessage());
        }
        
        writePropertyFile(filepath, Arrays.asList("thingsboard.username=test_user", "thingsboard.password=test_password", "path.url=path_url"));
        // Try loading RDB configs
        try {
            loadAPIConfig.invoke(testConnector, filepath);
            Assert.fail();
        } catch (InvocationTargetException e) {
            Assert.assertEquals(IOException.class, e.getCause().getClass());
            Assert.assertEquals(noDeviceId, e.getCause().getMessage());
        }
        
        writePropertyFile(filepath, Arrays.asList("thingsboard.username=test_user", "thingsboard.password=test_password", "path.url=path_url", "device.id=device_id"));
        // Try loading RDB configs
        try {
            loadAPIConfig.invoke(testConnector, filepath);
            Assert.fail();
        } catch (InvocationTargetException e) {
            Assert.assertEquals(IOException.class, e.getCause().getClass());
            Assert.assertEquals(noKeys, e.getCause().getMessage());
        }

        // Test for proper username and password
        writePropertyFile(filepath, Arrays.asList("thingsboard.username=test_user", "thingsboard.password=test_password", "path.url=path_url", "device.id=device_id", "keys=keys01,keys02,keys03"));
        // Try loading RDB configs
        try {
            loadAPIConfig.invoke(testConnector, filepath);
        } catch (Exception e) {
            Assert.fail(e.getMessage());
        }

        // Retrieve private fields for username and password and check that they were set correctly
        Field usernameField = ThingsBoardAPIConnector.class.getDeclaredField("username");
        usernameField.setAccessible(true);
        Assert.assertEquals("test_user", usernameField.get(testConnector));

        Field passwordField = ThingsBoardAPIConnector.class.getDeclaredField("password");
        passwordField.setAccessible(true);
        Assert.assertEquals("test_password", passwordField.get(testConnector));

        Field pathUrlField = ThingsBoardAPIConnector.class.getDeclaredField("path_url");
        pathUrlField.setAccessible(true);
        Assert.assertEquals("path_url", pathUrlField.get(testConnector));
        
        Field deviceIdField = ThingsBoardAPIConnector.class.getDeclaredField("device_id");
        deviceIdField.setAccessible(true);
        // Correct value depends on what is set in the @Before initialization method
        Assert.assertEquals("device_id", deviceIdField.get(testConnector));
        
        Field keysField = ThingsBoardAPIConnector.class.getDeclaredField("keys");
        keysField.setAccessible(true);
        // Correct value depends on what is set in the @Before initialization method
        Assert.assertEquals("keys01,keys02,keys03", keysField.get(testConnector));

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
        responseBody.put(ThingsBoardAPIConnector.TOKEN_KEY, "test_token");

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
            Assert.assertEquals("Unable to connect to ThingsBoard API!", e.getMessage());
            // Check also the cause
            Assert.assertEquals(HttpResponseException.class, e.getCause().getClass());
            Assert.assertTrue(e.getCause().getMessage().contains("Unexpected status code."));
        }
        thingsBoardAPIMock.resetAll();

        // The API returns a proper token ...
        setTokenAPIMock(ok()
                .withHeader("Content-Type", "application/json; charset=utf-8")
                .withBody(responseBody.toString()));
        thingsBoardAPIMock.stubFor(get(urlPathEqualTo("/" + "api/plugins/telemetry/DEVICE/" + "device_id" + "/values/attributes" ))
                .atPriority(2)
                .withHeader("X-Authorization", equalToIgnoreCase("Bearer test_token"))
                .willReturn(ok().withBody("[{\"key\":'active',\"value\":true}]")));
        testConnector.connect();
        // Check whether token field was set correctly
        Assert.assertEquals(responseBody.get(ThingsBoardAPIConnector.TOKEN_KEY), testConnector.getToken());

        // The API has an error //
        // Bad request
        setTokenAPIMock(badRequest());
        try {
            testConnector.connect();
            Assert.fail();
        }
        catch (JPSRuntimeException e) {
            Assert.assertEquals("Unable to connect to ThingsBoard API!", e.getMessage());
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
            Assert.assertEquals("Unable to connect to ThingsBoard API!", e.getMessage());
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
            Assert.assertEquals("Unable to connect to ThingsBoard API!", e.getMessage());
            // Check also the cause
            Assert.assertEquals(HttpResponseException.class, e.getCause().getClass());
            Assert.assertTrue(e.getCause().getMessage().contains("Could not retrieve access token."));
        }
    }

    @Test
    public void testCheckStatus() throws IOException, NoSuchFieldException, IllegalAccessException {
        try {
            testConnector.checkStatus();
        }
        catch (Exception e) {
            Assert.assertEquals(JPSRuntimeException.class, e.getClass());
            Assert.assertEquals("Token is not set. Use the connect method first.", e.getMessage());
        }

        // Set a token to avoid needing to invoke connect
        Field tokenField = ThingsBoardAPIConnector.class.getDeclaredField("token");
        tokenField.setAccessible(true);
        tokenField.set(testConnector, TEST_TOKEN);
        
        //Set device token
        String device_id = "12345";
        Field deviceIdField = ThingsBoardAPIConnector.class.getDeclaredField("device_id");
        deviceIdField.setAccessible(true);
        deviceIdField.set(testConnector, device_id);

        // The server returns an error
        try {
            testConnector.checkStatus();
        }
        catch (Exception e) {
            Assert.assertEquals(HttpResponseException.class, e.getClass());
            Assert.assertTrue(e.getMessage().contains("Unexpected status code."));
        }
        // The server returns a proper response
        thingsBoardAPIMock.stubFor(get(urlEqualTo("/" + "api/plugins/telemetry/DEVICE/" + device_id + "/values/attributes"))
                .withHeader("X-Authorization", equalTo("Bearer " + TEST_TOKEN))
                .willReturn(ok().withBody("[{\"lastUpdateTs\":12345678,\"key\":'lastConnectTime',\"value\":48995324},{\"lastUpdateTs\":12345678,\"key\":'active',\"value\":true},{\"lastUpdateTs\":91011121314,\"key\":'lastActivityTime',\"value\":123456}]")));
        //
        		Assert.assertEquals("is server status active: true", testConnector.checkStatus());
       
    }

    @Test
    public void testGetAllReadingsMockFail() throws NoSuchFieldException, IllegalAccessException {
        // Set a token to avoid needing to invoke connect
        Field tokenField = ThingsBoardAPIConnector.class.getDeclaredField("token");
        tokenField.setAccessible(true);
        tokenField.set(testConnector, TEST_TOKEN);

        String device_id = "12345";
        Field deviceIdField = ThingsBoardAPIConnector.class.getDeclaredField("device_id");
        deviceIdField.setAccessible(true);
        deviceIdField.set(testConnector, device_id);  
        // API gives an error when retrieving the data
        try {
            testConnector.getAllReadings(); 
            Assert.fail();
        }
        catch (JPSRuntimeException e) {
            Assert.assertEquals("Fridge electrical readings, Temperature and Humidity readings could not be retrieved", e.getMessage());
        }
    }
    /*
        //TODO Difficult to mock System.currentTimeMillis() as the stubfor and assertion tests occurs with a slight time difference
        @Test
        public void testGetAllReadingsMock1stRequestSuccess() throws NoSuchFieldException, IllegalAccessException {
            // Set a token to avoid needing to invoke connect
            Field tokenField = ThingsBoardAPIConnector.class.getDeclaredField("token");
            tokenField.setAccessible(true);
            tokenField.set(testConnector, TEST_TOKEN);

            String device_id = "12345";
            Field deviceIdField = ThingsBoardAPIConnector.class.getDeclaredField("device_id");
            deviceIdField.setAccessible(true);
            deviceIdField.set(testConnector, device_id); 
            
            String keys = "keys01,keys02,keys03";
            Field keysField = ThingsBoardAPIConnector.class.getDeclaredField("keys");
            keysField.setAccessible(true);
            keysField.set(testConnector, keys);
            JSONObject readings = new JSONObject();
            JSONObject values_01 = new JSONObject();
            values_01.put("ts","7891011" );
            values_01.put("value", "0.123");
            JSONObject values_02 = new JSONObject();
            values_02.put("ts","123456" );
            values_02.put("value", "0.456");
            JSONArray values = new JSONArray();
            values.put(values_01);
            values.put(values_02);
            readings.put("Current", values);
            thingsBoardAPIMock.stubFor(get(urlEqualTo("/" + "api/plugins/telemetry/DEVICE/" + device_id + "/values/timeseries?keys=" + keys
        		+ "&startTs=1&endTs="+ System.currentTimeMillis() + "&limit=3600&agg=NONE"))
                .willReturn(ok().withBody(readings.toString())));
        Assert.assertEquals(readings.toString(), testConnector.getAllReadings().toString());
        
    } 
        */
    private void setTokenAPIMock(ResponseDefinitionBuilder response) {
        // Expected request body
        JSONObject requestBody = new JSONObject();
        requestBody.put("username", "username");
        requestBody.put("password", "password");

        thingsBoardAPIMock.stubFor(post(urlPathEqualTo("/" + "api/auth/login"))
                .atPriority(1)
                .withHeader("Content-Type", equalToIgnoreCase("application/json; charset=utf-8"))
                .withRequestBody(equalToJson(requestBody.toString(), true, false))
                .willReturn(response));
    }

}
