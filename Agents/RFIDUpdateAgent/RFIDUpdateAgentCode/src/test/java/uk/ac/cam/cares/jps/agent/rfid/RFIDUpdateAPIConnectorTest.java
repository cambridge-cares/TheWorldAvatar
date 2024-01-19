package uk.ac.cam.cares.jps.agent.rfid;

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

public class RFIDUpdateAPIConnectorTest {


    // Temporary folder to place a properties file
    @Rule
    public TemporaryFolder folder = new TemporaryFolder();

    // Fields used for the mock API calls
    private static final int PORT = 8089;
    private static final String PATH_URL = "http://localhost:" + PORT + "/";

    @Rule
    public WireMockRule RFIDAPIMock = new WireMockRule(PORT);

    private RFIDUpdateAPIConnector testConnector;
     
    @Before
    public void initializeTestConnector() {
        testConnector = new RFIDUpdateAPIConnector("3", "status", PATH_URL, "00000000000000A000009727");
    
    }

    @After
    public void resetAPIMock() {
        RFIDAPIMock.resetAll();
    }
    

    @Test
    public void RFIDUpdateAPIConnectorConstructorTest() throws NoSuchFieldException, IllegalAccessException, IOException {
        // One connector constructed using the username, password, auth_url, api_url and device token directly
        RFIDUpdateAPIConnector connector = new RFIDUpdateAPIConnector("3", "status", "path_url","00000000000000A000009727");
        // One connector constructed using a properties file
        String propertiesFile = Paths.get(folder.getRoot().toString(), "api.properties").toString();
        writePropertyFile(propertiesFile, Arrays.asList("limit=3", "variable=status", "path.url=path_url", "keys=00000000000000A000009727"));
        RFIDUpdateAPIConnector connectorFile = new RFIDUpdateAPIConnector(propertiesFile);

        // Retrieve private fields for username and password and check that they were set correctly
        Field limitField = RFIDUpdateAPIConnector.class.getDeclaredField("limit");
        limitField.setAccessible(true);
        Assert.assertEquals("3", limitField.get(connector));
        Assert.assertEquals("3", limitField.get(connectorFile));
        
        Field variableField = RFIDUpdateAPIConnector.class.getDeclaredField("variable");
        variableField.setAccessible(true);
        Assert.assertEquals("status", variableField.get(connector));
        Assert.assertEquals("status", variableField.get(connectorFile));

        Field pathUrlField = RFIDUpdateAPIConnector.class.getDeclaredField("path_url");
        pathUrlField.setAccessible(true);
        Assert.assertEquals("path_url", pathUrlField.get(connector));
        Assert.assertEquals("path_url", pathUrlField.get(connectorFile));
        
        Field keysField = RFIDUpdateAPIConnector.class.getDeclaredField("keys");
        keysField.setAccessible(true);
        Assert.assertEquals("00000000000000A000009727", keysField.get(connector));
        Assert.assertEquals("00000000000000A000009727", keysField.get(connectorFile));
        
    }

    @Test
    public void loadAPIConfigsTest() throws NoSuchMethodException, IllegalAccessException, IOException, NoSuchFieldException {
        // Filepath to not yet created file in temporary test folder
        String filepath = Paths.get(folder.getRoot().toString(), "rfid.properties").toString();
        // Error messages
        String fileNotFound = "No properties file found at specified filepath: " + filepath;
        String noLimit = "Properties file is missing \"limit=<limit>\"";
        String noVariable = "Properties file is missing \"variable=<variable>\"";
        String noPathURL = "Properties file is missing \"path.url=<path_url>\"";
        String noKeys = "Properties file is missing \"keys=<keys>\"";

        // Set private method to be accessible
        Method loadAPIConfig = RFIDUpdateAPIConnector.class.getDeclaredMethod("loadAPIConfigs", String.class);
        loadAPIConfig.setAccessible(true);

        // Test for non-existing properties file
        try {
            loadAPIConfig.invoke(testConnector, filepath);
            Assert.fail();
        } catch (InvocationTargetException e) {
            Assert.assertEquals(FileNotFoundException.class, e.getCause().getClass());
            Assert.assertEquals(fileNotFound, e.getCause().getMessage());
        }

        // Test for missing limit by creating a file only containing variable
        writePropertyFile(filepath, Collections.singletonList("variable=status"));
        // Try loading RDB configs
        try {
            loadAPIConfig.invoke(testConnector, filepath);
            Assert.fail();
        } catch (InvocationTargetException e) {
            Assert.assertEquals(IOException.class, e.getCause().getClass());
            Assert.assertEquals(noLimit, e.getCause().getMessage());
        }

        // Test for missing variable by creating a file only containing limit
        writePropertyFile(filepath, Collections.singletonList("limit=3"));
        // Try loading RDB configs
        try {
            loadAPIConfig.invoke(testConnector, filepath);
            Assert.fail();
        } catch (InvocationTargetException e) {
            Assert.assertEquals(IOException.class, e.getCause().getClass());
            Assert.assertEquals(noVariable, e.getCause().getMessage());
        }

        // Test for missing URL by creating a file only containing limit and variable
        writePropertyFile(filepath, Arrays.asList("limit=3", "variable=status"));
        // Try loading RDB configs
        try {
            loadAPIConfig.invoke(testConnector, filepath);
            Assert.fail();
        } catch (InvocationTargetException e) {
            Assert.assertEquals(IOException.class, e.getCause().getClass());
            Assert.assertEquals(noPathURL, e.getCause().getMessage());
        }
        // Test for missing keys by creating a file only containing limit, variable and path.url
        writePropertyFile(filepath, Arrays.asList("limit=3", "variable=status", "path.url=path_url"));
        // Try loading RDB configs
        try {
            loadAPIConfig.invoke(testConnector, filepath);
            Assert.fail();
        } catch (InvocationTargetException e) {
            Assert.assertEquals(IOException.class, e.getCause().getClass());
            Assert.assertEquals(noKeys, e.getCause().getMessage());
        }

        // Retrieve private fields for username and password and check that they were set correctly
        Field limitField = RFIDUpdateAPIConnector.class.getDeclaredField("limit");
        limitField.setAccessible(true);
        Assert.assertEquals("3", limitField.get(testConnector));

        Field variableField = RFIDUpdateAPIConnector.class.getDeclaredField("variable");
        variableField.setAccessible(true);
        Assert.assertEquals("status", variableField.get(testConnector));

        Field pathUrlField = RFIDUpdateAPIConnector.class.getDeclaredField("path_url");
        pathUrlField.setAccessible(true);
        Assert.assertEquals("path_url", pathUrlField.get(testConnector));
        
        Field keysField = RFIDUpdateAPIConnector.class.getDeclaredField("keys");
        keysField.setAccessible(true);
        // Correct value depends on what is set in the @Before initialization method
        Assert.assertEquals("00000000000000A000009727", keysField.get(testConnector));

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

    
    //TODO Difficult to mock System.currentTimeMillis() as the stubfor and assertion tests occurs with a slight time difference
    @Test
    public void testGetAllReadingsMockRequestSuccess() throws NoSuchFieldException, IllegalAccessException {
        // Set a token to avoid needing to invoke connect
        Field limitField = RFIDUpdateAPIConnector.class.getDeclaredField("limit");
        limitField.setAccessible(true);
        limitField.set(testConnector, "3" );

        String variable = "status";
        Field variableField = RFIDUpdateAPIConnector.class.getDeclaredField("variable");
        variableField.setAccessible(true);
        variableField.set(testConnector, variable); 
            
        String keys = "00000000000000A000009727";
        Field keysField = RFIDUpdateAPIConnector.class.getDeclaredField("keys");
        keysField.setAccessible(true);
        keysField.set(testConnector, keys);
        JSONObject readings = new JSONObject();
        JSONObject values_01 = new JSONObject();
        values_01.put("ts","123461" );
        values_01.put("value", "Out");
        JSONObject values_02 = new JSONObject();
        values_02.put("ts","123456" );
        values_02.put("value", "In");
        JSONArray values = new JSONArray();
        values.put(values_01);
        values.put(values_02);
        readings.put("tag_00000000000000A000009727_status", values);
        RFIDAPIMock.stubFor(get(urlEqualTo("/" + "values=status/limit=3/keys=00000000000000A000009727"))
                .willReturn(ok().withBody(readings.toString())));
        Assert.assertEquals(readings.toString(), testConnector.getAllReadings().toString());
        
    } 
        
}
