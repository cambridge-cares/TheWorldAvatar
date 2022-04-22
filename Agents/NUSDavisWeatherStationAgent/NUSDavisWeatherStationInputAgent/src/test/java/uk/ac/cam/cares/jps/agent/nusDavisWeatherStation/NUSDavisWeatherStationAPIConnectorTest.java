package uk.ac.cam.cares.jps.agent.nusDavisWeatherStation;

import com.github.tomakehurst.wiremock.junit.WireMockRule;
import org.json.JSONObject;
import org.junit.*;
import org.junit.rules.TemporaryFolder;


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

import static com.github.tomakehurst.wiremock.client.WireMock.*;

public class NUSDavisWeatherStationAPIConnectorTest {

    @Rule
    public TemporaryFolder folder = new TemporaryFolder();

    // Fields used for the mock API calls
    private static final int PORT = 8089;
    private static final String TEST_URL = "http://localhost:" + PORT + "/";
    // Mocking objects to mock weather station API calls
    @Rule
    public WireMockRule nusDavisWeatherStationAPIMock = new WireMockRule(PORT);

    private NUSDavisWeatherStationAPIConnector testConnector;

    @Before
    public void initializeTestConnector() {
        testConnector= new NUSDavisWeatherStationAPIConnector("key","secret",TEST_URL,123456);
    }

    @After
    public void resetAPIMock(){
        nusDavisWeatherStationAPIMock.resetAll();
    }

    @Test
    public void NUSDavisWeatherStationConstructorTest() throws NoSuchFieldException, IOException, IllegalAccessException {

        NUSDavisWeatherStationAPIConnector connector=new NUSDavisWeatherStationAPIConnector("key","secret","url",123456);
        String propertiesFile = Paths.get(folder.getRoot().toString(), "api.properties").toString();
        writePropertyFile(propertiesFile, Arrays.asList("weather.api_key=key", "weather.api_secret=secret","weather.api_url=url","weather.stationId=123456" ));
        NUSDavisWeatherStationAPIConnector connectorFile= new NUSDavisWeatherStationAPIConnector(propertiesFile);

        Field apiKeyField = NUSDavisWeatherStationAPIConnector.class.getDeclaredField("api_key");
        apiKeyField.setAccessible(true);
        Assert.assertEquals("key", apiKeyField.get(connector));
        Assert.assertEquals("key", apiKeyField.get(connectorFile));

        Field apiSecretField = NUSDavisWeatherStationAPIConnector.class.getDeclaredField("api_secret");
        apiSecretField.setAccessible(true);
        Assert.assertEquals("secret", apiSecretField.get(connector));
        Assert.assertEquals("secret", apiSecretField.get(connectorFile));

        Field apiUrlField = NUSDavisWeatherStationAPIConnector.class.getDeclaredField("api_url");
        apiUrlField.setAccessible(true);
        Assert.assertEquals("url", apiUrlField.get(connector));
        Assert.assertEquals("url", apiUrlField.get(connectorFile));

        Field stationIdField = NUSDavisWeatherStationAPIConnector.class.getDeclaredField("stationId");
        stationIdField.setAccessible(true);
        Assert.assertEquals(123456, stationIdField.get(connector));
        Assert.assertEquals(123456, stationIdField.get(connectorFile));

    }

    @Test
    public void loadAPIConfigsTest() throws IOException, NoSuchMethodException, NoSuchFieldException, IllegalAccessException {
        // Filepath to not yet created file in temporary test folder
        String filepath = Paths.get(folder.getRoot().toString(), "weather.properties").toString();
        // Error messages
        String fileNotFound = "There was no properties file found in the specified path: " + filepath;
        String noAPIKey = "The properties file is missing \"weather.api_key=<api_key>\"";
        String noAPISecret= "The properties file is missing \"weather.api_secret=<api_secret>\"";
        String noURL = "The properties file is missing \"weather.api_url=<api_url>\"";
        String noStationId = "The properties file is missing \"weather.stationId=<stationId>\"";

        // Set private method to be accessible
        Method loadAPIConfig = NUSDavisWeatherStationAPIConnector.class.getDeclaredMethod("loadAPIconfigs", String.class);
        loadAPIConfig.setAccessible(true);
        // Test for non-existing properties file
        try {
            loadAPIConfig.invoke(testConnector, filepath);
            Assert.fail();
        } catch (InvocationTargetException | IllegalAccessException e) {
            Assert.assertEquals(FileNotFoundException.class, e.getCause().getClass());
            Assert.assertEquals(fileNotFound, e.getCause().getMessage());
        }

        // Test for missing api_key by creating a file only containing stationId
        writePropertyFile(filepath, Collections.singletonList("weather.api_secret=secret"));
        // Try loading RDB configs
        try {
            loadAPIConfig.invoke(testConnector, filepath);
            Assert.fail();
        } catch (InvocationTargetException | IllegalAccessException e) {
            Assert.assertEquals(IOException.class, e.getCause().getClass());
            Assert.assertEquals(noAPIKey, e.getCause().getMessage());
        }

        // Test for missing api_secret by creating a file only containing api_key
        writePropertyFile(filepath, Collections.singletonList("weather.api_key=key"));
        // Try loading RDB configs
        try {
            loadAPIConfig.invoke(testConnector, filepath);
            Assert.fail();
        } catch (InvocationTargetException | IllegalAccessException e) {
            Assert.assertEquals(IOException.class, e.getCause().getClass());
            Assert.assertEquals(noAPISecret, e.getCause().getMessage());
        }

        // Test for missing URL by creating a file only containing api_key and api_secret
        writePropertyFile(filepath, Arrays.asList("weather.api_key=key", "weather.api_secret=secret"));
        // Try loading RDB configs
        try {
            loadAPIConfig.invoke(testConnector, filepath);
            Assert.fail();
        } catch (InvocationTargetException | IllegalAccessException e) {
            Assert.assertEquals(IOException.class, e.getCause().getClass());
            Assert.assertEquals(noURL, e.getCause().getMessage());
        }

        // Test for missing stationId by creating a file only containing api_key, api_secret and api_url
        writePropertyFile(filepath, Arrays.asList("weather.api_key=key", "weather.api_secret=secret", "weather.api_url=url"));
        // Try loading RDB configs
        try {
            loadAPIConfig.invoke(testConnector, filepath);
            Assert.fail();
        } catch (InvocationTargetException | IllegalAccessException e) {
            Assert.assertEquals(IOException.class, e.getCause().getClass());
            Assert.assertEquals(noStationId, e.getCause().getMessage());
        }

        // Test for proper api_key, api_secret, url and stationId
        writePropertyFile(filepath, Arrays.asList("weather.api_key=test_key", "weather.api_secret=test_secret", "weather.api_url=test_url","weather.stationId=123456"));
        // Try loading RDB configs
        try {
            loadAPIConfig.invoke(testConnector, filepath);
        } catch (Exception e) {
            Assert.fail(e.getMessage());
        }
        // Retrieve private fields for apiKey, stationId and api_url. Check that they were set correctly
        Field apiKeyField =NUSDavisWeatherStationAPIConnector.class.getDeclaredField("api_key");
        apiKeyField.setAccessible(true);
        Assert.assertEquals("test_key", apiKeyField.get(testConnector));

        Field apiSecretField =NUSDavisWeatherStationAPIConnector.class.getDeclaredField("api_secret");
        apiSecretField.setAccessible(true);
        Assert.assertEquals("test_secret", apiSecretField.get(testConnector));

        Field UrlField =NUSDavisWeatherStationAPIConnector.class.getDeclaredField("api_url");
        UrlField.setAccessible(true);
        Assert.assertEquals("test_url", UrlField.get(testConnector));

        Field stationIdField =NUSDavisWeatherStationAPIConnector.class.getDeclaredField("stationId");
        stationIdField.setAccessible(true);
        Assert.assertEquals(123456, stationIdField.get(testConnector));
    }
    @Test
    public void testGetWeatherDataReadings() throws NoSuchFieldException, IllegalAccessException {

        // API returns a response
        JSONObject responseBody = new JSONObject();
        JSONObject asset = new JSONObject();
        double val=77.2;
        asset.put("testval", val);
        responseBody.put("testObject",asset);

        NUSDavisWeatherStationAPIConnector testConnector1= new NUSDavisWeatherStationAPIConnector("987654321","ABC123",TEST_URL,123456,1558729481);


        Field api_sign=testConnector1.getClass().getDeclaredField("api_Signature");
        api_sign.setAccessible(true);
        //signature calculated using the online tool: freeformatter.com/hmac-generator
        String signature= "c1bd80b57a887aa35ed48f71dc66c64825328ad42a0344949cb9c510d318b36e";
        api_sign.set(testConnector1,signature);

        nusDavisWeatherStationAPIMock.stubFor(get(urlEqualTo("/v2/current/123456?api-key=987654321&t=1558729481&api-signature="+signature))
                .willReturn(ok().withBody(responseBody.toString())));
        String response=testConnector1.getWeatherReadings().toString();
        Assert.assertEquals(responseBody.toString(), testConnector1.getWeatherReadings().toString());
    }

    @Test
    public void testSetAPISignature() throws NoSuchMethodException, NoSuchFieldException, InvocationTargetException, IllegalAccessException {
        NUSDavisWeatherStationAPIConnector testConnector2= new NUSDavisWeatherStationAPIConnector("987654321","ABC123",TEST_URL,123456,1558729481);

        Field timestamp=testConnector2.getClass().getDeclaredField("current_timestamp");
        timestamp.setAccessible(true);
        Long ts= (Long)timestamp.get(testConnector2);
        Method setSign=testConnector2.getClass().getDeclaredMethod("setAPISignature",Long.class);
        setSign.setAccessible(true);
        setSign.invoke(testConnector2,ts);


        Field api_sign=testConnector2.getClass().getDeclaredField("api_Signature");
        api_sign.setAccessible(true);
        //expected signature calculated using the online tool: freeformatter.com/hmac-generator
        Assert.assertEquals("c1bd80b57a887aa35ed48f71dc66c64825328ad42a0344949cb9c510d318b36e",api_sign.get(testConnector2));
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
