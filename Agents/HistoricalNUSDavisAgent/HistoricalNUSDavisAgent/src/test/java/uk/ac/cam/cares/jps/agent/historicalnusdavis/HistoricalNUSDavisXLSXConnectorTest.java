package uk.ac.cam.cares.jps.agent.historicalnusdavis;

import org.json.JSONArray;
import org.json.JSONObject;
import org.junit.*;
import org.junit.rules.TemporaryFolder;
import java.io.*;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.nio.file.Path;
import java.nio.file.Paths;

@Ignore("Requires a sample data excel file to be in the data folder, check data.txt for more information")

public class HistoricalNUSDavisXLSXConnectorTest {

	@Rule
    public TemporaryFolder folder = new TemporaryFolder();

    private final String connectorPropertiesFilename = "xlsxconnector.properties";
    String connectorPropertiesFilePath;
    HistoricalNUSDavisXLSXConnector connector;
    
    @Before
    public void initializePropertyFile() throws IOException {
        File apiPropertyFile= folder.newFile(connectorPropertiesFilename);
        // Paths to the three different property files
        connectorPropertiesFilePath = apiPropertyFile.getCanonicalPath();
    }
    
    @Test
    public void testConstructorFailed() throws IOException {
    	HistoricalNUSDavisXLSXConnector connector;
    	//test random filepaths that leads to nowhere
    	try {
    		connector = new HistoricalNUSDavisXLSXConnector("test_01", "test_03");
    		Assert.fail();
    	} catch (FileNotFoundException e) {
    		Assert.assertTrue(e.getMessage().contains("No properties file found at specified filepath: "));
    	}
    	
    	//test with empty properties file
    	try {
    		connector = new HistoricalNUSDavisXLSXConnector("test_01", connectorPropertiesFilePath);
    		Assert.fail();
    	} catch (IOException e) {
    		Assert.assertTrue(e.getMessage().contains("Properties file is missing \"numOfKeys=<numOfKeys>\""));
    	}
    	
    	
    }
    
    @Test
    public void testGetClassFromJSONKey() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException, IOException {
    	createProperConnectorPropertiesFile();
    	
    	connector = new HistoricalNUSDavisXLSXConnector("test_01", connectorPropertiesFilePath);
        // Make private method accessible
        Method getClassFromJSONKey = HistoricalNUSDavisXLSXConnector.class.getDeclaredMethod("getClassFromJSONKey", String.class);
        getClassFromJSONKey.setAccessible(true);
        // No specific key should return the string class
        //Assert.assertEquals("ts", getClassFromJSONKey.invoke(connector, "reading_datestamp"));
      
    }
    
    @Test
    public void testRetrieveReadings() throws IOException, IllegalAccessException, IllegalArgumentException, InvocationTargetException, NoSuchMethodException, SecurityException {
    	createProperConnectorPropertiesFile();
    	Path dataFilePath = Paths.get("./data/data.xlsx");
    	connector = new HistoricalNUSDavisXLSXConnector(dataFilePath.toString(), connectorPropertiesFilePath);
    	JSONObject values = connector.retrieveReadings(dataFilePath.toString(), 18);
    	Assert.assertEquals(values.getJSONArray("sensors").getJSONObject(0).getJSONArray("data").length(), 3);
    }
    
    private void createProperConnectorPropertiesFile() throws IOException {
        // Filepath for the properties file
        String propertiesFile = Paths.get(folder.getRoot().toString(), connectorPropertiesFilename).toString();
        try (FileWriter writer = new FileWriter(propertiesFile, false)) {
            writer.write("numOfKeys=18\n");
           
        }
    }
}
