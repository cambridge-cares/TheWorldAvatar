package uk.ac.cam.cares.jps.agent.caresWeatherStation;

import com.github.tomakehurst.wiremock.junit.WireMockRule;
import org.json.JSONObject;
import org.junit.*;
import org.junit.rules.TemporaryFolder;


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

public class CARESWeatherStationAPIConnectorTest {
    // Temporary folder to place a properties file
    @Rule
    public TemporaryFolder folder = new TemporaryFolder();

    // Fields used for the mock API calls
    private static final int PORT = 8089;
    private static final String TEST_URL = "http://localhost:" + PORT + "/";
    // Mocking objects to mock weather station API calls
    @Rule
    public WireMockRule caresWeatherStationAPIMock = new WireMockRule(PORT);

    private CARESWeatherStationAPIConnector testConnector;

    @Before
    public void initializeTestConnector() {
        testConnector = new CARESWeatherStationAPIConnector("password", "id", TEST_URL);
    }

    @After
    public void resetAPIMock() {
        caresWeatherStationAPIMock.resetAll();
    }

    @Test
    public void CARESWeatherStationAPIConnectorConstructorTest() throws NoSuchFieldException, IllegalAccessException, IOException {
        // One connector constructed using the password, id and url directly
        CARESWeatherStationAPIConnector connector = new CARESWeatherStationAPIConnector("password", "id", "url");
        // One connector constructed using a properties file
        String propertiesFile = Paths.get(folder.getRoot().toString(), "api.properties").toString();
        writePropertyFile(propertiesFile, Arrays.asList("weather.api_key=password", "weather.stationId=id", "weather.api_url=url"));
        CARESWeatherStationAPIConnector connectorFile = new CARESWeatherStationAPIConnector(propertiesFile);

        // Retrieve private fields for api_key, stationId and api_url. Check that they were set correctly
        Field apiKeyField = CARESWeatherStationAPIConnector.class.getDeclaredField("api_key");
        apiKeyField.setAccessible(true);
        Assert.assertEquals("password", apiKeyField.get(connector));
        Assert.assertEquals("password", apiKeyField.get(connectorFile));

        Field stationIdField = CARESWeatherStationAPIConnector.class.getDeclaredField("stationId");
        stationIdField.setAccessible(true);
        Assert.assertEquals("id", stationIdField.get(connector));
        Assert.assertEquals("id", stationIdField.get(connectorFile));

        Field urlField = CARESWeatherStationAPIConnector.class.getDeclaredField("api_url");
        urlField.setAccessible(true);
        Assert.assertEquals("url", urlField.get(connector));
        Assert.assertEquals("url", urlField.get(connectorFile));
    }

    @Test
    public void loadAPIConfigsTest() throws NoSuchMethodException, IllegalAccessException, IOException, NoSuchFieldException {
        // Filepath to not yet created file in temporary test folder
        String filepath = Paths.get(folder.getRoot().toString(), "weather.properties").toString();
        // Error messages
        String fileNotFound = "There was no properties file found in the specified path: " + filepath;
        String noAPIKey = "The properties file is missing \"weather.api_key=<api_key>\"";
        String noStationId = "The properties file is missing \"weather.stationId=<stationId>\"";
        String noURL = "The properties file is missing \"weather.api_url=<api_url>\"";

        // Set private method to be accessible
        Method loadAPIConfig = CARESWeatherStationAPIConnector.class.getDeclaredMethod("loadAPIconfigs", String.class);
        loadAPIConfig.setAccessible(true);

        // Test for non-existing properties file
        try {
            loadAPIConfig.invoke(testConnector, filepath);
            Assert.fail();
        } catch (InvocationTargetException e) {
            Assert.assertEquals(FileNotFoundException.class, e.getCause().getClass());
            Assert.assertEquals(fileNotFound, e.getCause().getMessage());
        }

        // Test for missing api_key by creating a file only containing stationId
        writePropertyFile(filepath, Collections.singletonList("weather.stationId=id"));
        // Try loading RDB configs
        try {
            loadAPIConfig.invoke(testConnector, filepath);
            Assert.fail();
        } catch (InvocationTargetException e) {
            Assert.assertEquals(IOException.class, e.getCause().getClass());
            Assert.assertEquals(noAPIKey, e.getCause().getMessage());
        }

        // Test for missing stationId by creating a file only containing api_key
        writePropertyFile(filepath, Collections.singletonList("weather.api_key=password"));
        // Try loading RDB configs
        try {
            loadAPIConfig.invoke(testConnector, filepath);
            Assert.fail();
        } catch (InvocationTargetException e) {
            Assert.assertEquals(IOException.class, e.getCause().getClass());
            Assert.assertEquals(noStationId, e.getCause().getMessage());
        }

        // Test for missing URL by creating a file only containing api_key and stationId
        writePropertyFile(filepath, Arrays.asList("weather.api_key=password", "weather.stationId=id"));
        // Try loading RDB configs
        try {
            loadAPIConfig.invoke(testConnector, filepath);
            Assert.fail();
        } catch (InvocationTargetException e) {
            Assert.assertEquals(IOException.class, e.getCause().getClass());
            Assert.assertEquals(noURL, e.getCause().getMessage());
        }

        // Test for proper api_key, stationId and url
        writePropertyFile(filepath, Arrays.asList("weather.api_key=test_key", "weather.stationId=test_id", "weather.api_url=test_url"));
        // Try loading RDB configs
        try {
            loadAPIConfig.invoke(testConnector, filepath);
        } catch (Exception e) {
            Assert.fail(e.getMessage());
        }

        // Retrieve private fields for apiKey, stationId and api_url. Check that they were set correctly
        Field apiKeyField = CARESWeatherStationAPIConnector.class.getDeclaredField("api_key");
        apiKeyField.setAccessible(true);
        Assert.assertEquals("test_key", apiKeyField.get(testConnector));

        Field stationIdField = CARESWeatherStationAPIConnector.class.getDeclaredField("stationId");
        stationIdField.setAccessible(true);
        Assert.assertEquals("test_id", stationIdField.get(testConnector));

        Field urlField = CARESWeatherStationAPIConnector.class.getDeclaredField("api_url");
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
    public void testGetWeatherDataReadings() throws NoSuchFieldException, IllegalAccessException {

        // API returns a response
        JSONObject responseBody = new JSONObject();
        JSONObject asset = new JSONObject();
        double val=77.2;
        asset.put("testval", val);
        responseBody.put("testObject",asset);

        caresWeatherStationAPIMock.stubFor(get(urlEqualTo("/v2/pws/observations/all/1day?stationId=id"
                +"&format=json&units=s&numericPrecision=decimal&apiKey=password"))
                .willReturn(ok().withBody(responseBody.toString())));

        Assert.assertEquals(responseBody.toString(), testConnector.getWeatherReadings().toString());

    }

}