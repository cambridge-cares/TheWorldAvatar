package uk.ac.cam.cares.jps.agent.historicalaqmesh;

import org.json.JSONArray;
import org.json.JSONObject;
import org.junit.*;
import org.junit.rules.TemporaryFolder;
import org.mockito.ArgumentCaptor;
import org.mockito.Mockito;

import com.github.stefanbirkner.systemlambda.SystemLambda;

import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;

import java.io.*;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.time.OffsetDateTime;
import java.time.ZoneOffset;
import java.util.*;

public class HistoricalAQMeshAgentXLSXConnectorTest {

	@Rule
    public TemporaryFolder folder = new TemporaryFolder();

    private final String connectorPropertiesFilename = "xlsxconnector.properties";
    String connectorPropertiesFilePath;
    
    @Before
    public void initializePropertyFile() throws IOException {
        File apiPropertyFile= folder.newFile(connectorPropertiesFilename);
        // Paths to the three different property files
        connectorPropertiesFilePath = apiPropertyFile.getCanonicalPath();
    }
    
    @Test
    public void testConstructorFailed() throws IOException {
    	HistoricalAQMeshAgentXLSXConnector connector;
    	//test random filepaths that leads to nowhere
    	try {
    		connector = new HistoricalAQMeshAgentXLSXConnector("test_01", "test_02", "test_03");
    		Assert.fail();
    	} catch (FileNotFoundException e) {
    		Assert.assertTrue(e.getMessage().contains("No properties file found at specified filepath: "));
    	}
    	
    	//test with empty properties file
    	try {
    		connector = new HistoricalAQMeshAgentXLSXConnector("test_01", "test_02", connectorPropertiesFilePath);
    		Assert.fail();
    	} catch (IOException e) {
    		Assert.assertTrue(e.getMessage().contains("Properties file is missing \"numOfGasKeys=<numOfGasKeys>\""));
    	}
    	
    	String propertiesFile = Paths.get(folder.getRoot().toString(), connectorPropertiesFilename).toString();
        try (FileWriter writer = new FileWriter(propertiesFile, false)) {
            writer.write("numOfGasKeys=65\n");     
        }
        // test with properties file containing only numOfGasKeys
        try {
    		connector = new HistoricalAQMeshAgentXLSXConnector("test_01", "test_02", connectorPropertiesFilePath);
    		Assert.fail();
    	} catch (IOException e) {
    		Assert.assertTrue(e.getMessage().contains("Properties file is missing \"numOfParticleAndGeneralKeys=<numOfParticleAndGeneralKeys>\""));
    	}
    }
    
    @Test
    public void testGetClassFromJSONKey() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException, IOException {
    	createProperConnectorPropertiesFile();
    	HistoricalAQMeshAgentXLSXConnector connector;
    	connector = new HistoricalAQMeshAgentXLSXConnector("test_01", "test_02", connectorPropertiesFilePath);
        // Make private method accessible
        Method getClassFromJSONKey = HistoricalAQMeshAgentXLSXConnector.class.getDeclaredMethod("getClassFromJSONKey", String.class);
        getClassFromJSONKey.setAccessible(true);
        // No specific key should return the string class
        Assert.assertEquals("String", getClassFromJSONKey.invoke(connector, "ts"));
        Assert.assertEquals("Integer", getClassFromJSONKey.invoke(connector, "gas_p1"));
        Assert.assertEquals("Integer", getClassFromJSONKey.invoke(connector, "gas_reading_number"));
        Assert.assertEquals("Double", getClassFromJSONKey.invoke(connector, "battery_voltage"));
       
    }
    
    private void createProperConnectorPropertiesFile() throws IOException {
        // Filepath for the properties file
        String propertiesFile = Paths.get(folder.getRoot().toString(), connectorPropertiesFilename).toString();
        try (FileWriter writer = new FileWriter(propertiesFile, false)) {
            writer.write("numOfGasKeys=65\n");
            writer.write("numOfParticleAndGeneralKeys=35\n");
           
        }
    }
}
