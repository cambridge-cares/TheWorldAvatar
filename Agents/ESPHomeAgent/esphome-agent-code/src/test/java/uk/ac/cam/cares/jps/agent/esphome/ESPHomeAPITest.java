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

    // Mocking objects to mock AQMesh API calls
    @Rule
    public WireMockRule esphomeAPIMock = new WireMockRule(PORT);

    private ESPHomeAPI testAPI;
    
    String filepath;
  
    @Before
    public void initializeTestAPI() throws IOException {
    	
    	// One connector constructed using a properties file
    	String propertiesFile = Paths.get(folder.getRoot().toString(), "api.properties").toString();
        writePropertyFile(propertiesFile, Arrays.asList("esphome.url=" + TEST_URL, "esphome.threshold=25"));
        testAPI = new ESPHomeAPI(propertiesFile);
    
    }

    @After
    public void resetAPIMock() {
    	esphomeAPIMock.resetAll();
    }
    

    @Test
    public void ESPHomeAPIConstructorTest() throws NoSuchFieldException, IllegalAccessException, IOException {
    	 // Filepath to not yet created file in temporary test folder
        String filePath = Paths.get(folder.getRoot().toString(), "apitest.properties").toString();
        //test constructor with non existent file
        try {
            new ESPHomeAPI(filePath);
            Assert.fail();
        }
        catch (FileNotFoundException e) {
            Assert.assertEquals("No properties file found at specified filepath: " + filePath, e.getMessage());
        }   
        // Test for missing esphome.url
        writePropertyFile(filePath, Collections.singletonList("esphome.threshold=25"));
        try {
            new ESPHomeAPI(filePath);
            Assert.fail();
        }
        catch (IOException e) {
            Assert.assertEquals("Properties file is missing \"esphome.url=<esphome_url>\" ", e.getMessage());
        }
        
        // Test for missing esphome.threshold
        writePropertyFile(filePath, Collections.singletonList("esphome.url=test_url"));
        try {
            new ESPHomeAPI(filePath);
            Assert.fail();
        }
        catch (IOException e) {
            Assert.assertEquals("Properties file is missing \"esphome.threshold=<esphome_threshold>\" ", e.getMessage());
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
    public void testCheckStatus() throws NoSuchFieldException, IllegalAccessException, NoSuchMethodException, JSONException, IOException {
    	Method checkStatus = ESPHomeAPI.class.getDeclaredMethod("checkStatus");
        checkStatus.setAccessible(true);
    	// API returns an error (not status 200)
    	esphomeAPIMock.stubFor(get(urlPathEqualTo("/" + "switch/generic_output"))
                .willReturn(badRequest()));
          try {
			testAPI.checkStatus();
		} catch (HttpResponseException e) {
            Assert.assertTrue(e.getMessage().contains("Could not establish connection with ESPHome web server."));
        }
        
        JSONObject responseBody = new JSONObject();
        
        esphomeAPIMock.stubFor(get(urlPathEqualTo("/" + "switch/generic_output"))
                .willReturn(ok().withBody(responseBody.toString())));
                	boolean state;
        			try {
        				state = testAPI.checkStatus();
        			} catch (JSONException e) {
        				Assert.assertTrue(e.getMessage().contains("No assets available in returned JSON Object."));
        			}
        			
        //JSONObject for off state
        JSONObject responseBodyOffState = new JSONObject();
        responseBodyOffState.put("id", "switch-generic_output");
        responseBodyOffState.put("state", "OFF");
        responseBodyOffState.put("value", false);
        
        
        esphomeAPIMock.stubFor(get(urlPathEqualTo("/" + "switch/generic_output"))
        .willReturn(ok().withBody(responseBodyOffState.toString())));
			
				state = testAPI.checkStatus();
				Assert.assertEquals(state,false);
        
        
      //JSONObject for on state
        JSONObject responseBodyOnState = new JSONObject();
        responseBodyOnState.put("id", "switch-generic_output");
        responseBodyOnState.put("state", "ON");
        responseBodyOnState.put("value", true);
        
        
        esphomeAPIMock.stubFor(get(urlPathEqualTo("/" + "switch/generic_output"))
        .willReturn(ok().withBody(responseBodyOnState.toString())));
        
        	state = testAPI.checkStatus();
            Assert.assertEquals(state,true);
    }
    
    @Test
    public void testTurnOnSwitch() throws NoSuchFieldException, IllegalAccessException, NoSuchMethodException {
    	Method turnOnSwitch = ESPHomeAPI.class.getDeclaredMethod("turnOnSwitch");
    	turnOnSwitch.setAccessible(true);
    
    	// API returns an error (not status 200)
    	esphomeAPIMock.stubFor(post(urlPathEqualTo("/" + "switch/generic_output/turn_on"))
                .willReturn(badRequest()));
        try {
        	
        	turnOnSwitch.invoke(testAPI);
        }
        catch (InvocationTargetException e) {
            Assert.assertTrue(e.getCause().getMessage().contains("Could not establish connection with ESPHome web server."));
        }
        
        //Status 200
        esphomeAPIMock.stubFor(post(urlPathEqualTo("/" + "switch/generic_output/turn_on"))
        .willReturn(ok()));
        try {
            JSONObject message = new JSONObject(turnOnSwitch.invoke(testAPI).toString());
            Assert.assertEquals(message.get("message"), "A POST request has been sent to turn on the device or component.");
        }catch (InvocationTargetException e) {
        }
        
    }
    
    @Test
    public void testTurnOffSwitch() throws NoSuchFieldException, IllegalAccessException, NoSuchMethodException, JSONException, IllegalArgumentException, InvocationTargetException {
    	Method turnOffSwitch = ESPHomeAPI.class.getDeclaredMethod("turnOffSwitch");
    	turnOffSwitch.setAccessible(true);
    
    	// API returns an error (not status 200)
    	esphomeAPIMock.stubFor(post(urlPathEqualTo("/" + "switch/generic_output/turn_off"))
                .willReturn(badRequest()));
        try {
        	
        	turnOffSwitch.invoke(testAPI);
        }
        catch (InvocationTargetException e) {
            Assert.assertTrue(e.getCause().getMessage().contains("Could not establish connection with ESPHome web server."));
        }
        
        //Status 200
        esphomeAPIMock.stubFor(post(urlPathEqualTo("/" + "switch/generic_output/turn_off"))
        .willReturn(ok()));
        try {
            JSONObject message = new JSONObject(turnOffSwitch.invoke(testAPI).toString());
            Assert.assertEquals(message.get("message"), "A POST request has been sent to turn off the device or component.");
        }catch (InvocationTargetException e) {
        }
        	
    }
    
    
    @Test
    public void testEsphomeSwitchControl() throws NoSuchFieldException, IllegalAccessException, NoSuchMethodException, InvocationTargetException {
    	double exceedThreshold = 26;
    	double belowThreshold = 24.4;
    	
    	//JSONObject for on state
        JSONObject responseBodyOnState = new JSONObject();
        responseBodyOnState.put("id", "switch-generic_output");
        responseBodyOnState.put("state", "ON");
        responseBodyOnState.put("value", true);
        
      //JSONObject for off state
        JSONObject responseBodyOffState = new JSONObject();
        responseBodyOffState.put("id", "switch-generic_output");
        responseBodyOffState.put("state", "OFF");
        responseBodyOffState.put("value", false);
    	
    	Method checkStatus = ESPHomeAPI.class.getDeclaredMethod("checkStatus");
        checkStatus.setAccessible(true);
        
        //checkStatus == true && exceedThreshold > threshold 
        JSONObject message = testAPI.esphomeSwitchControl(exceedThreshold, true);
        	Assert.assertEquals(message.get("message"),"The switch is already in the ON state.");	
 
       //checkStatus == false && exceedThreshold > threshold
        esphomeAPIMock.stubFor(post(urlPathEqualTo("/" + "switch/generic_output/turn_on"))
                .willReturn(ok()));
        JSONObject message1 = testAPI.esphomeSwitchControl(exceedThreshold, false);
        Assert.assertEquals(message1.get("message"),"A POST request has been sent to turn on the device or component.");
       
       //checkStatus == false && belowThreshold < threshold
        JSONObject message2 = testAPI.esphomeSwitchControl(belowThreshold, false);
        Assert.assertEquals(message2.get("message"),"The switch is already in the OFF state.");
      
     
      //checkStatus == true && belowThreshold < threshold
        esphomeAPIMock.stubFor(post(urlPathEqualTo("/" + "switch/generic_output/turn_off"))
                .willReturn(ok()));
        JSONObject message3 = testAPI.esphomeSwitchControl(belowThreshold, true);
        Assert.assertEquals(message3.get("message"),"A POST request has been sent to turn off the device or component.");    

    }
}
