package uk.ac.cam.cares.jps.agent.esphomeUpdate;

import com.github.tomakehurst.wiremock.client.ResponseDefinitionBuilder;
import com.github.tomakehurst.wiremock.junit.WireMockRule;
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
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

public class ESPHomeUpdateAPIConnectorTest {


    // Temporary folder to place a properties file
    @Rule
    public TemporaryFolder folder = new TemporaryFolder();

    // Fields used for the mock API calls
    private static final int PORT = 8089;
    private static final String PATH_URL = "http://localhost:" + PORT + "/";

    // Mocking objects to mock ESPHome API calls
    @Rule
    public WireMockRule esphomeAPIMock = new WireMockRule(PORT);

    private ESPHomeUpdateAPIConnector testConnector;
    
  
    @Before
    public void initializeTestConnector() {
        testConnector = new ESPHomeUpdateAPIConnector(PATH_URL, "domain", "ID");
    
    }

    @After
    public void resetAPIMock() {
        esphomeAPIMock.resetAll();
    }
    

    @Test
    public void ESPHomeUpdateAPIConnectorConstructorTest() throws NoSuchFieldException, IllegalAccessException, IOException {
        // One connector constructed using the path url, domain and domain ID directly
        ESPHomeUpdateAPIConnector connector = new ESPHomeUpdateAPIConnector("path_url", "domain", "ID");
        // One connector constructed using a properties file
        String propertiesFile = Paths.get(folder.getRoot().toString(), "api.properties").toString();
        writePropertyFile(propertiesFile, Arrays.asList("path.url=path_url", "esphome.domain=domain", "domain.ID=ID"));
        ESPHomeUpdateAPIConnector connectorFile = new ESPHomeUpdateAPIConnector(propertiesFile);

        Field pathUrlField = ESPHomeUpdateAPIConnector.class.getDeclaredField("path_url");
        pathUrlField.setAccessible(true);
        Assert.assertEquals("path_url", pathUrlField.get(connector));
        Assert.assertEquals("path_url", pathUrlField.get(connectorFile));
        
        Field deviceIdField = ESPHomeUpdateAPIConnector.class.getDeclaredField("domain");
        deviceIdField.setAccessible(true);
        Assert.assertEquals("domain", deviceIdField.get(connector));
        Assert.assertEquals("domain", deviceIdField.get(connectorFile));
        
        Field keysField = ESPHomeUpdateAPIConnector.class.getDeclaredField("ID");
        keysField.setAccessible(true);
        Assert.assertEquals("ID", keysField.get(connector));
        Assert.assertEquals("ID", keysField.get(connectorFile));
        
    }

    @Test
    public void loadAPIConfigsTest() throws NoSuchMethodException, IllegalAccessException, IOException, NoSuchFieldException {
        // Filepath to not yet created file in temporary test folder
        String filepath = Paths.get(folder.getRoot().toString(), "esphome.properties").toString();
        // Error messages
        String fileNotFound = "No properties file found at specified filepath: " + filepath;
        String noPathURL = "Properties file is missing \"path.url=<path_url>\"";
        String noDomain = "Properties file is missing \"esphome.domain=<domain>\"";
        String noID = "Properties file is missing \"domain.ID=<ID>\"";

        // Set private method to be accessible
        Method loadAPIConfig = ESPHomeUpdateAPIConnector.class.getDeclaredMethod("loadAPIConfigs", String.class);
        loadAPIConfig.setAccessible(true);

        // Test for non-existing properties file
        try {
            loadAPIConfig.invoke(testConnector, filepath);
            Assert.fail();
        } catch (InvocationTargetException e) {
            Assert.assertEquals(FileNotFoundException.class, e.getCause().getClass());
            Assert.assertEquals(fileNotFound, e.getCause().getMessage());
        }

        // Test for missing URL by creating a file only containing domain and domain ID
        writePropertyFile(filepath, Arrays.asList("esphome.domain=domain", "domain.ID=ID"));
        // Try loading RDB configs
        try {
            loadAPIConfig.invoke(testConnector, filepath);
            Assert.fail();
        } catch (InvocationTargetException e) {
            Assert.assertEquals(IOException.class, e.getCause().getClass());
            Assert.assertEquals(noPathURL, e.getCause().getMessage());
        }
     // Test for missing domain by creating a file only containing path URL and domain ID
        writePropertyFile(filepath, Arrays.asList("path.url=path_url", "domain.ID=ID"));
        
        try {
            loadAPIConfig.invoke(testConnector, filepath);
            Assert.fail();
        } catch (InvocationTargetException e) {
            Assert.assertEquals(IOException.class, e.getCause().getClass());
            Assert.assertEquals(noDomain, e.getCause().getMessage());
        }
        
     // Test for missing domain ID by creating a file only containing path URL and domain
        writePropertyFile(filepath, Arrays.asList("path.url=path_url", "esphome.domain=domain"));
      
        try {
            loadAPIConfig.invoke(testConnector, filepath);
            Assert.fail();
        } catch (InvocationTargetException e) {
            Assert.assertEquals(IOException.class, e.getCause().getClass());
            Assert.assertEquals(noID, e.getCause().getMessage());
        }

        // Test for proper path URL, domain and domain ID
        writePropertyFile(filepath, Arrays.asList("path.url=path_url", "esphome.domain=domain", "domain.ID=ID"));
       
        try {
            loadAPIConfig.invoke(testConnector, filepath);
        } catch (Exception e) {
            Assert.fail(e.getMessage());
        }

        Field pathUrlField = ESPHomeUpdateAPIConnector.class.getDeclaredField("path_url");
        pathUrlField.setAccessible(true);
        Assert.assertEquals("path_url", pathUrlField.get(testConnector));
        
        Field domainField = ESPHomeUpdateAPIConnector.class.getDeclaredField("domain");
        domainField.setAccessible(true);
        // Correct value depends on what is set in the @Before initialization method
        Assert.assertEquals("domain", domainField.get(testConnector));
        
        Field IDField = ESPHomeUpdateAPIConnector.class.getDeclaredField("ID");
        IDField.setAccessible(true);
        // Correct value depends on what is set in the @Before initialization method
        Assert.assertEquals("ID", IDField.get(testConnector));

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
    	Method checkStatus = ESPHomeUpdateAPIConnector.class.getDeclaredMethod("checkStatus");
        checkStatus.setAccessible(true);
        
      //Set domain
        String domain = "domain";
        Field domainField = ESPHomeUpdateAPIConnector.class.getDeclaredField("domain");
        domainField.setAccessible(true);
        domainField.set(testConnector, domain);
        
      //Set ID
        String ID = "switch_01";
        Field IDField = ESPHomeUpdateAPIConnector.class.getDeclaredField("ID");
        IDField.setAccessible(true);
        IDField.set(testConnector, ID);

        
    	// API returns an error (not status 200)
    	esphomeAPIMock.stubFor(get(urlPathEqualTo("/" + domain + "/" + ID))
                .willReturn(badRequest()));
          try {
			testConnector.checkStatus();
		} catch (HttpResponseException e) {
            Assert.assertTrue(e.getMessage().contains("Could not establish connection with ESPHome web server."));
        }
        
        JSONObject responseBody = new JSONObject();
        
        esphomeAPIMock.stubFor(get(urlPathEqualTo("/" + domain +"/" + ID))
                .willReturn(ok().withBody(responseBody.toString())));
                	JSONObject reading;
        			try {
        				reading = testConnector.checkStatus();
        			} catch (JSONException e) {
        				Assert.assertTrue(e.getMessage().contains("No assets available in returned JSON Object."));
        			}
        
    }

    /*
        //TODO Difficult to mock System.currentTimeMillis() as the stubfor and assertion tests occurs with a slight time difference
        
        */

}
