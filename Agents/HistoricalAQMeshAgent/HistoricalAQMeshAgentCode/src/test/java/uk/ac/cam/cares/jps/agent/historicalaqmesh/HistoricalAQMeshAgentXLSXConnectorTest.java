package uk.ac.cam.cares.jps.agent.historicalaqmesh;

import org.json.JSONArray;

import org.junit.*;
import org.junit.rules.TemporaryFolder;
import java.io.*;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.nio.file.Path;
import java.nio.file.Paths;


@Ignore("Requires sample data excel files in xlsx format for both gas and particle readings to be in the data folder, check data.txt for more information")

public class HistoricalAQMeshAgentXLSXConnectorTest {

	@Rule
    public TemporaryFolder folder = new TemporaryFolder();

    private final String connectorPropertiesFilename = "xlsxconnector.properties";
    String connectorPropertiesFilePath;
    HistoricalAQMeshAgentXLSXConnector connector;
    
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
    	
    	connector = new HistoricalAQMeshAgentXLSXConnector("test_01", "test_02", connectorPropertiesFilePath);
        // Make private method accessible
        Method getClassFromJSONKey = HistoricalAQMeshAgentXLSXConnector.class.getDeclaredMethod("getClassFromJSONKey", String.class);
        getClassFromJSONKey.setAccessible(true);
        // No specific key should return the string class
        Assert.assertEquals("ts", getClassFromJSONKey.invoke(connector, "reading_datestamp"));
        Assert.assertEquals("Integer", getClassFromJSONKey.invoke(connector, "gas_p1"));
        Assert.assertEquals("Integer", getClassFromJSONKey.invoke(connector, "gas_reading_number"));
        Assert.assertEquals("Double", getClassFromJSONKey.invoke(connector, "battery_voltage"));
        Assert.assertEquals("Double", getClassFromJSONKey.invoke(connector, "temperature"));
        Assert.assertEquals("Double", getClassFromJSONKey.invoke(connector, "pressure"));
        Assert.assertEquals("Double", getClassFromJSONKey.invoke(connector, "humidity"));
        Assert.assertEquals("Double", getClassFromJSONKey.invoke(connector, "noise"));
        Assert.assertEquals("Double", getClassFromJSONKey.invoke(connector, "prescale"));
        Assert.assertEquals("Double", getClassFromJSONKey.invoke(connector, "slope"));
        Assert.assertEquals("Double", getClassFromJSONKey.invoke(connector, "offset"));
        Assert.assertEquals("Integer", getClassFromJSONKey.invoke(connector, "aux2_sensor_serial_number"));
        Assert.assertEquals("Integer", getClassFromJSONKey.invoke(connector, "uart_sensor_serial_number"));
        Assert.assertEquals("Integer", getClassFromJSONKey.invoke(connector, "aux1_sensor_serial_number"));
        Assert.assertEquals("Integer", getClassFromJSONKey.invoke(connector, "h2s_sensor_serial_number"));
        Assert.assertEquals("Integer", getClassFromJSONKey.invoke(connector, "pod_serial_number"));
        Assert.assertEquals("Integer", getClassFromJSONKey.invoke(connector, "owner_number"));
        Assert.assertEquals("Integer", getClassFromJSONKey.invoke(connector, "location_number"));
        Assert.assertEquals("Integer", getClassFromJSONKey.invoke(connector, "_serial_number"));
        Assert.assertEquals("Boolean", getClassFromJSONKey.invoke(connector, "battery_low"));
        Assert.assertEquals("Boolean", getClassFromJSONKey.invoke(connector, "particle_modem_overlap"));
        Assert.assertEquals("String", getClassFromJSONKey.invoke(connector, "aux2_state"));
        Assert.assertEquals("String", getClassFromJSONKey.invoke(connector, "aux1_type"));
    }
    
    @Test
    public void testRetrieveReadings() throws IOException, IllegalAccessException, IllegalArgumentException, InvocationTargetException, NoSuchMethodException, SecurityException {
    	createProperConnectorPropertiesFile();
    	Path gasDataFilePath = Paths.get("./data/aqmeshGasData.xlsx");
    	Path particleDataFilePath = Paths.get("./data/aqmeshParticleAndGeneralData.xlsx");
    	connector = new HistoricalAQMeshAgentXLSXConnector(gasDataFilePath.toString(), particleDataFilePath.toString(), connectorPropertiesFilePath);
    	JSONArray values = connector.retrieveReadings(gasDataFilePath.toString(), 65);
    	Assert.assertEquals(values.length(), 3);
    	Assert.assertEquals(values.getJSONObject(0).length(), 65);
    	
    	values = connector.retrieveReadings(particleDataFilePath.toString(), 35);
    	Assert.assertEquals(values.length(), 3);
    	Assert.assertEquals(values.getJSONObject(0).length(), 35);
 
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
