package uk.ac.cam.cares.jps.agent.historicalpirmasens;

import org.json.JSONArray;
import org.json.JSONObject;
import org.junit.*;
import org.junit.rules.TemporaryFolder;
import java.io.*;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.nio.file.Path;
import java.nio.file.Paths;
/*
@Ignore("Requires a sample data CSV file to be in the data folder")
*/
public class HistoricalPirmasensStationCSVConnectorTest {

	@Rule
    public TemporaryFolder folder = new TemporaryFolder();

    private final String connectorPropertiesFilename = "csvconnector.properties";
    String connectorPropertiesFilePath;
    HistoricalPirmasensStationCSVConnector connector;
    
    @Before
    public void initializePropertyFile() throws IOException {
        File apiPropertyFile= folder.newFile(connectorPropertiesFilename);
        // Paths to the three different property files
        connectorPropertiesFilePath = apiPropertyFile.getCanonicalPath();
    }
    
    @Test
    public void testConstructorFailed() throws IOException {
    	HistoricalPirmasensStationCSVConnector connector;
    	//test random filepaths that leads to nowhere
    	try {
    		connector = new HistoricalPirmasensStationCSVConnector("test_01", "test_03");
    		Assert.fail();
    	} catch (FileNotFoundException e) {
    		Assert.assertTrue(e.getMessage().contains("No properties file found at specified filepath: "));
    	}
    	
    	//test with empty properties file
    	try {
    		connector = new HistoricalPirmasensStationCSVConnector("test_01", connectorPropertiesFilePath);
    		Assert.fail();
    	} catch (IOException e) {
    		Assert.assertTrue(e.getMessage().contains("Properties file is missing \"numOfKeys=<numOfKeys>\""));
    	}
    	
    	
    }
    
    @Test
    public void testRetrieveReadingsSuccess() throws IOException, IllegalAccessException, IllegalArgumentException, InvocationTargetException, NoSuchMethodException, SecurityException {
    	createProperConnectorPropertiesFile();
    	Path dataFilePath = Paths.get("./testData/testData.csv");
    	connector = new HistoricalPirmasensStationCSVConnector(dataFilePath.toString(), connectorPropertiesFilePath);
    	JSONObject values = connector.retrieveReadings(dataFilePath.toString(), 12);
    	//two days of data = 48 readings for each hour
    	Assert.assertEquals(values.getJSONArray("sensors").getJSONObject(0).getJSONArray("data").length(), 48);
    }
    
    @Test
    public void testRetrieveReadingsFailed() throws IOException, IllegalAccessException, IllegalArgumentException, InvocationTargetException, NoSuchMethodException, SecurityException {
    	createProperConnectorPropertiesFile();
    	Path dataFilePath = Paths.get("./testData/wrongTestData.csv");
    	connector = new HistoricalPirmasensStationCSVConnector(dataFilePath.toString(), connectorPropertiesFilePath);
    	try {
    	JSONObject values = connector.retrieveReadings(dataFilePath.toString(), 12);
    	Assert.fail();
    	} catch (Exception e) {
    	//two days of data = 48 readings for each hour
    	Assert.assertEquals("There was an error while retrieving the data from the CSV file! The current row retrieved from the CSV file is PS-Innenstadt';'Gesamt-UV" , e.getMessage());
    }
    }
     
    @Test
    public void testCountNumberOfRowsSuccess() throws IOException {
    	createProperConnectorPropertiesFile();
    	Path dataFilePath = Paths.get("./testData/testData.csv");
    	connector = new HistoricalPirmasensStationCSVConnector(dataFilePath.toString(), connectorPropertiesFilePath);
    	Integer numberOfRows = connector.countNumberOfRows(dataFilePath.toString());
    	Assert.assertEquals(numberOfRows, Integer.valueOf("25"));
    }
    
    @Test
    public void testCountNumberOfRowsFailed() throws IOException {
    	Integer numberOfRows = 0;
    	//test random filepaths that leads to nowhere
    	try {
    		Path dataFilePath = Paths.get("./test_01");
    		createProperConnectorPropertiesFile();
    		connector = new HistoricalPirmasensStationCSVConnector(dataFilePath.toString(), connectorPropertiesFilePath);
    		numberOfRows = connector.countNumberOfRows(dataFilePath.toString());
    		Assert.fail();
    	} catch (Exception e) {
    		Assert.assertTrue(e.getMessage().contains("No properties file found at specified filepath: "));
    	}
    }
    
    
    private void createProperConnectorPropertiesFile() throws IOException {
        // Filepath for the properties file
        String propertiesFile = Paths.get(folder.getRoot().toString(), connectorPropertiesFilename).toString();
        try (FileWriter writer = new FileWriter(propertiesFile, false)) {
            writer.write("numOfKeys=12\n");
           
        }
    }
}
