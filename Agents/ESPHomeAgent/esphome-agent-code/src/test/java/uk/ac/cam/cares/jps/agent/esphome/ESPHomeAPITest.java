package uk.ac.cam.cares.jps.agent.esphome;

import com.github.tomakehurst.wiremock.junit.WireMockRule;
import com.jcraft.jsch.Logger;

import org.apache.http.client.HttpResponseException;
import org.json.JSONException;
import org.json.JSONObject;
import org.junit.*;
import org.junit.rules.TemporaryFolder;
import org.mockito.Mockito;

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

public class ESPHomeAPITest {


    // Temporary folder to place a properties file
    @Rule
    public TemporaryFolder folder = new TemporaryFolder();

    // Fields used for the mock API calls
    private static final int PORT = 8089;
    private static final String TEST_URL = "http://localhost:" + PORT + "/";

    // Mocking objects to mock ESPHome API calls
    @Rule
    public WireMockRule esphomeAPIMock = new WireMockRule(PORT);

    private ESPHomeAPI testAPI;
    
    String filepath;
  
    @Before
    public void initializeTestAPI() throws IOException {
    	
    	// One connector constructed using a properties file
    	String propertiesFile = Paths.get(folder.getRoot().toString(), "api.properties").toString();
        writePropertyFile(propertiesFile, Arrays.asList("path.url=" + TEST_URL, "esphome.domain=test_domain", "domain.ID=test_ID", "esphome.threshold=25"));
        testAPI = new ESPHomeAPI(propertiesFile);
    
    }

    @After
    public void resetAPIMock() {
    	esphomeAPIMock.resetAll();
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
    public void ESPHomeUpdateAPIConnectorConstructorTest() throws NoSuchFieldException, IllegalAccessException, IOException {
        // One connector constructed using path URL, domain, domain ID and threshold value directly
        ESPHomeAPI connector = new ESPHomeAPI("path_url", "domain", "ID");
        // One connector constructed using a properties file
        String propertiesFile = Paths.get(folder.getRoot().toString(), "api.properties").toString();
        writePropertyFile(propertiesFile, Arrays.asList("path.url=path_url", "esphome.domain=domain", "domain.ID=ID"));
        ESPHomeAPI connectorFile = new ESPHomeAPI(propertiesFile);

        Field pathUrlField = ESPHomeAPI.class.getDeclaredField("path_url");
        pathUrlField.setAccessible(true);
        Assert.assertEquals("path_url", pathUrlField.get(connector));
        Assert.assertEquals("path_url", pathUrlField.get(connectorFile));
        
        Field deviceIdField = ESPHomeAPI.class.getDeclaredField("domain");
        deviceIdField.setAccessible(true);
        Assert.assertEquals("domain", deviceIdField.get(connector));
        Assert.assertEquals("domain", deviceIdField.get(connectorFile));
        
        Field IDField = ESPHomeAPI.class.getDeclaredField("ID");
        IDField.setAccessible(true);
        Assert.assertEquals("ID", IDField.get(connector));
        Assert.assertEquals("ID", IDField.get(connectorFile));
    }

    @Test
    public void loadAPIConfigsTest() throws NoSuchMethodException, IllegalAccessException, IOException, NoSuchFieldException {
        // Filepath to not yet created file in temporary test folder
        String filepath = Paths.get(folder.getRoot().toString(), "thingsboard.properties").toString();
        // Error messages
        String fileNotFound = "No properties file found at specified filepath: " + filepath;
        String noPathURL = "Properties file is missing \"path.url=<path_url>\"";
        String noDomain = "Properties file is missing \"esphome.domain=<domain>\"";
        String noID = "Properties file is missing \"domain.ID=<ID>\"";

        // Set private method to be accessible
        Method loadESPHomeConfigs = ESPHomeAPI.class.getDeclaredMethod("loadESPHomeConfigs", String.class);
        loadESPHomeConfigs.setAccessible(true);

        // Test for non-existing properties file
        try {
        	loadESPHomeConfigs.invoke(testAPI, filepath);
            Assert.fail();
        } catch (InvocationTargetException e) {
            Assert.assertEquals(FileNotFoundException.class, e.getCause().getClass());
            Assert.assertEquals(fileNotFound, e.getCause().getMessage());
        }

        // Test for missing URL 
        writePropertyFile(filepath, Arrays.asList("esphome.domain=domain", "domain.ID=ID"));
        // Try loading RDB configs
        try {
        	loadESPHomeConfigs.invoke(testAPI, filepath);
            Assert.fail();
        } catch (InvocationTargetException e) {
            Assert.assertEquals(IOException.class, e.getCause().getClass());
            Assert.assertEquals(noPathURL, e.getCause().getMessage());
        }
        //Test for missing domain
        writePropertyFile(filepath, Arrays.asList("path.url=path_url", "domain.ID=ID"));
        
        try {
        	loadESPHomeConfigs.invoke(testAPI, filepath);
            Assert.fail();
        } catch (InvocationTargetException e) {
            Assert.assertEquals(IOException.class, e.getCause().getClass());
            Assert.assertEquals(noDomain, e.getCause().getMessage());
        }
        //Test for missing ID
        writePropertyFile(filepath, Arrays.asList("path.url=path_url", "esphome.domain=domain"));
      
        try {
        	loadESPHomeConfigs.invoke(testAPI, filepath);
            Assert.fail();
        } catch (InvocationTargetException e) {
            Assert.assertEquals(IOException.class, e.getCause().getClass());
            Assert.assertEquals(noID, e.getCause().getMessage());
        }

        // Test for proper path URL, domain, domain ID
        writePropertyFile(filepath, Arrays.asList("path.url=path_url", "esphome.domain=domain", "domain.ID=ID"));
       
        try {
        	loadESPHomeConfigs.invoke(testAPI, filepath);
        } catch (Exception e) {
            Assert.fail(e.getMessage());
        }

        Field pathUrlField = ESPHomeAPI.class.getDeclaredField("path_url");
        pathUrlField.setAccessible(true);
        Assert.assertEquals("path_url", pathUrlField.get(testAPI));
        
        Field domainField = ESPHomeAPI.class.getDeclaredField("domain");
        domainField.setAccessible(true);
        // Correct value depends on what is set in the @Before initialization method
        Assert.assertEquals("domain", domainField.get(testAPI));
        
        Field IDField = ESPHomeAPI.class.getDeclaredField("ID");
        IDField.setAccessible(true);
        // Correct value depends on what is set in the @Before initialization method
        Assert.assertEquals("ID", IDField.get(testAPI));
    }
    
    @Test
    public void testTurnOnComponent() throws NoSuchFieldException, IllegalAccessException, NoSuchMethodException {
    	Method turnOnComponent = ESPHomeAPI.class.getDeclaredMethod("turnOnComponent");
    	turnOnComponent.setAccessible(true);
    
    	// API returns an error (not status 200)
    	esphomeAPIMock.stubFor(post(urlPathEqualTo("/" + "test_domain/test_ID/turn_on"))
                .willReturn(badRequest()));
        try {
        	
        	turnOnComponent.invoke(testAPI);
        }
        catch (InvocationTargetException e) {
            Assert.assertTrue(e.getCause().getMessage().contains("Could not establish connection with ESPHome web server."));
        }
        
        //Status 200
        esphomeAPIMock.stubFor(post(urlPathEqualTo("/" + "test_domain/test_ID/turn_on"))
        .willReturn(ok()));
        try {
            JSONObject message = new JSONObject(turnOnComponent.invoke(testAPI).toString());
            Assert.assertEquals(message.get("message"), "A POST request has been sent to turn on the device or component.");
        }catch (InvocationTargetException e) {
        }
        
    }
    
    @Test
    public void testTurnOffComponent() throws NoSuchFieldException, IllegalAccessException, NoSuchMethodException, JSONException, IllegalArgumentException, InvocationTargetException {
    	Method turnOffComponent = ESPHomeAPI.class.getDeclaredMethod("turnOffComponent");
    	turnOffComponent.setAccessible(true);
    
    	// API returns an error (not status 200)
    	esphomeAPIMock.stubFor(post(urlPathEqualTo("/" + "test_domain/test_ID/turn_off"))
                .willReturn(badRequest()));
        try {
        	
        	turnOffComponent.invoke(testAPI);
        }
        catch (InvocationTargetException e) {
            Assert.assertTrue(e.getCause().getMessage().contains("Could not establish connection with ESPHome web server."));
        }
        
        //Status 200
        esphomeAPIMock.stubFor(post(urlPathEqualTo("/" + "test_domain/test_ID/turn_off"))
        .willReturn(ok()));
        try {
            JSONObject message = new JSONObject(turnOffComponent.invoke(testAPI).toString());
            Assert.assertEquals(message.get("message"), "A POST request has been sent to turn off the device or component.");
        }catch (InvocationTargetException e) {
        }
        	
    }
    
    
    @Test
    public void testEsphomeSwitchControl() throws NoSuchFieldException, IllegalAccessException, NoSuchMethodException, InvocationTargetException {
    	double exceedThreshold = 26;
    	double belowThreshold = 24.4;
        
        //Status == ON && exceedThreshold > threshold 
        JSONObject message = testAPI.esphomeSwitchControl(exceedThreshold, "ON", 25);
        	Assert.assertEquals(message.get("message"),"The component is already in the ON state.");	
 
       //Status == OFF && exceedThreshold > threshold
        esphomeAPIMock.stubFor(post(urlPathEqualTo("/" + "test_domain/test_ID/turn_on"))
                .willReturn(ok()));
        JSONObject message1 = testAPI.esphomeSwitchControl(exceedThreshold, "OFF", 25);
        Assert.assertEquals(message1.get("message"),"A POST request has been sent to turn on the device or component.");
       
       //Status == OFF && belowThreshold < threshold
        JSONObject message2 = testAPI.esphomeSwitchControl(belowThreshold, "OFF", 25);
        Assert.assertEquals(message2.get("message"),"The component is already in the OFF state.");
      
      //Status == ON && belowThreshold < threshold
        esphomeAPIMock.stubFor(post(urlPathEqualTo("/" + "test_domain/test_ID/turn_off"))
                .willReturn(ok()));
        JSONObject message3 = testAPI.esphomeSwitchControl(belowThreshold, "ON", 25);
        Assert.assertEquals(message3.get("message"),"A POST request has been sent to turn off the device or component.");    

    }
}
