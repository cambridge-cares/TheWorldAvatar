package uk.ac.cam.cares.jps.agent.thingspeak;

import com.github.tomakehurst.wiremock.client.ResponseDefinitionBuilder;
import com.github.tomakehurst.wiremock.junit.WireMockRule;
import org.apache.http.client.HttpResponseException;
import org.json.JSONArray;
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

public class ThingspeakAPIConnectorTest {


    // Temporary folder to place a properties file
    @Rule
    public TemporaryFolder folder = new TemporaryFolder();

    // Fields used for the mock API calls
    private static final int PORT = 8089;
    private static final String PATH_URL = "http://localhost:" + PORT + "/";

    // Mocking objects to mock API calls
    @Rule
    public WireMockRule thingspeakAPIMock = new WireMockRule(PORT);

    private ThingspeakAPIConnector testConnector;
    
  
    @Before
    public void initializeTestConnector() {
        testConnector = new ThingspeakAPIConnector("12345", "testapikey", "1", PATH_URL);
    
    }

    @After
    public void resetAPIMock() {
        thingspeakAPIMock.resetAll();
    }
    

    @Test
    public void ThingspeakAPIConnectorConstructorTest() throws NoSuchFieldException, IllegalAccessException, IOException {
        // One connector constructed using the parameters directly
        ThingspeakAPIConnector connector = new ThingspeakAPIConnector("12345", "testapikey", "1", PATH_URL);
        // One connector constructed using a properties file
        String propertiesFile = Paths.get(folder.getRoot().toString(), "api.properties").toString();
        writePropertyFile(propertiesFile, Arrays.asList("thingspeak.channelNumber=12345", "thingspeak.apiKey=testapikey", "thingspeak.results=1", "path.url=http://localhost:8089/"));
        ThingspeakAPIConnector connectorFile = new ThingspeakAPIConnector(propertiesFile);

        // Retrieve private fields and check that they were set correctly
        Field channelNumberField = ThingspeakAPIConnector.class.getDeclaredField("channelNumber");
        channelNumberField.setAccessible(true);
        Assert.assertEquals("12345", channelNumberField.get(connector));
        Assert.assertEquals("12345", channelNumberField.get(connectorFile));
        
        Field APIKeyField = ThingspeakAPIConnector.class.getDeclaredField("APIKey");
        APIKeyField.setAccessible(true);
        Assert.assertEquals("testapikey", APIKeyField.get(connector));
        Assert.assertEquals("testapikey", APIKeyField.get(connectorFile));

        Field resultsField = ThingspeakAPIConnector.class.getDeclaredField("results");
        resultsField.setAccessible(true);
        Assert.assertEquals("1", resultsField.get(connector));
        Assert.assertEquals("1", resultsField.get(connectorFile)); 
        
        Field pathUrlField = ThingspeakAPIConnector.class.getDeclaredField("pathUrl");
        pathUrlField.setAccessible(true);
        Assert.assertEquals(PATH_URL, pathUrlField.get(connector));
        Assert.assertEquals(PATH_URL, pathUrlField.get(connectorFile)); 
    }

    @Test
    public void loadAPIConfigsTest() throws NoSuchMethodException, IllegalAccessException, IOException, NoSuchFieldException {
        // Filepath to not yet created file in temporary test folder
        String filepath = Paths.get(folder.getRoot().toString(), "api.properties").toString();
        // Error messages
        String fileNotFound = "No properties file found at specified filepath: " + filepath;
        String noChannelNumber = "Properties file is missing \"thingspeak.channelNumber=<thingspeak_channelNumber>\"";
        String noResults = "Properties file is missing \"thingspeak.results=<results>\"";

        // Set private method to be accessible
        Method loadAPIConfig = ThingspeakAPIConnector.class.getDeclaredMethod("loadAPIConfigs", String.class);
        loadAPIConfig.setAccessible(true);

        // Test for non-existing properties file
        try {
            loadAPIConfig.invoke(testConnector, filepath);
            Assert.fail();
        } catch (InvocationTargetException e) {
            Assert.assertEquals(FileNotFoundException.class, e.getCause().getClass());
            Assert.assertEquals(fileNotFound, e.getCause().getMessage());
        }

        // Test for missing channel number
        writePropertyFile(filepath, Arrays.asList("thingspeak.apiKey=testapikey", "thingspeak.results=1"));
        // Try loading API config
        try {
            loadAPIConfig.invoke(testConnector, filepath);
            Assert.fail();
        } catch (InvocationTargetException e) {
            Assert.assertEquals(IOException.class, e.getCause().getClass());
            Assert.assertEquals(noChannelNumber, e.getCause().getMessage());
        }

        // Test for missing results
        writePropertyFile(filepath, Arrays.asList("thingspeak.channelNumber=12345", "thingspeak.apiKey=testapikey"));
        // Try loading RDB configs
        try {
            loadAPIConfig.invoke(testConnector, filepath);
            Assert.fail();
        } catch (InvocationTargetException e) {
            Assert.assertEquals(IOException.class, e.getCause().getClass());
            Assert.assertEquals(noResults, e.getCause().getMessage());
        }
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
    public void testGetDataReadings() throws NoSuchFieldException, IllegalAccessException, IOException {
    	
        String filepath = Paths.get(folder.getRoot().toString(), "api.properties").toString();
        writePropertyFile(filepath, Arrays.asList("thingspeak.channelNumber=12345", "thingspeak.apiKey=testapikey", "thingspeak.results=1", "path.url=http://localhost:8089/"));
        testConnector = new ThingspeakAPIConnector(filepath);
        // API returns a response
        JSONObject responseBody = new JSONObject();
        JSONObject jsonobject1 = new JSONObject();
        JSONObject jsonobject2 = new JSONObject();
        JSONArray jsonarray1 = new JSONArray();
        
        jsonobject1.put("created_at", "2022-11-09T03:05:18Z");
        jsonobject1.put("entry_id", 59267);
        jsonobject1.put("field1", "621");
        
        jsonarray1.put(jsonobject1);
        
        jsonobject2.put("id", 1876219);
        jsonobject2.put("name", "co2");
        jsonobject2.put("description", "co2 test sensor");
        jsonobject2.put("latitude", "0.0");
        jsonobject2.put("longitude","0.0");
        jsonobject2.put("field1", "ppm");
        jsonobject2.put("created_at", "2022-09-28T15:29:24Z");
        jsonobject2.put("updated_at", "2022-09-28T17:00:37Z");
        jsonobject2.put("last_entry_id", 59267);
        
        responseBody.put("channel", jsonobject2);
        responseBody.put("feeds", jsonarray1);
        

        thingspeakAPIMock.stubFor(get(urlEqualTo("/12345/feeds.json?api_key=testapikey&results=1"))
                .willReturn(ok().withBody(responseBody.toString())));

        Assert.assertEquals(responseBody.toString(), testConnector.getAllReadings().toString());
        
        String filepath2 = Paths.get(folder.getRoot().toString(), "api.properties").toString();
        writePropertyFile(filepath2, Arrays.asList("thingspeak.channelNumber=12345", "thingspeak.apiKey=None", "thingspeak.results=1", "path.url=http://localhost:8089/"));
        testConnector = new ThingspeakAPIConnector(filepath2);
        
        thingspeakAPIMock.stubFor(get(urlEqualTo("/12345/feeds.json?results=1"))
                .willReturn(ok().withBody(responseBody.toString())));

        Assert.assertEquals(responseBody.toString(), testConnector.getAllReadings().toString());

    }


}
