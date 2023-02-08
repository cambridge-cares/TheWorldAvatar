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
@Ignore("Requires a sample data CSV file to be in the testdata folder")
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
    	
    	//Modified raw data where keys such as Gesamt-UV Strahlung, Relative Feuchte, eBC_PM2_5,
    	//Windgeschwindigkeit-rohwert and Windrichtung-rohwert were changed to:
    	//Gesamt_UV_Strahlung, Relative_Feuchte, eBC_PM2-5, Windgeschwindigkeit_rohwert, Windrichtung_rohwert
    	
    	Path dataFilePath = Paths.get("./testData/testData.csv");
    	connector = new HistoricalPirmasensStationCSVConnector(dataFilePath.toString(), connectorPropertiesFilePath);
    	JSONObject values = connector.retrieveReadings(dataFilePath.toString(), 12);
    	//two days of data = 48 readings for each hour
    	Assert.assertEquals(values.getJSONArray("sensors").getJSONObject(0).getJSONArray("data").length(), 48);
    }
    
    @Test
    public void testRetrieveReadingsFailed() throws IOException, IllegalAccessException, IllegalArgumentException, InvocationTargetException, NoSuchMethodException, SecurityException {
    	createProperConnectorPropertiesFile();
    	// original raw data without any modifications
    	Path dataFilePath = Paths.get("./testData/wrongTestData.csv");
    	connector = new HistoricalPirmasensStationCSVConnector(dataFilePath.toString(), connectorPropertiesFilePath);
    	try {
    	JSONObject values = connector.retrieveReadings(dataFilePath.toString(), 12);
    	Assert.fail();
    	} catch (Exception e) {
    	Assert.assertEquals("There was an error while retrieving the data from the CSV file! The current row retrieved from the CSV file is PS-Innenstadt';'Gesamt-UV" , e.getMessage());
    }
    	//date is in yyyy-mm-dd format instead of yyyymmdd
    	dataFilePath = Paths.get("./testData/wrongDateFormatTestData.csv");
    	connector = new HistoricalPirmasensStationCSVConnector(dataFilePath.toString(), connectorPropertiesFilePath);
    	try {
        	JSONObject values = connector.retrieveReadings(dataFilePath.toString(), 1);
        	Assert.fail();
        	} catch (Exception e) {
        	Assert.assertEquals("Ecountered an error while parsing the date 2022-07-07" , e.getMessage());
        }
    	// There are only 2 readings provided for the date 20220707
    	dataFilePath = Paths.get("./testData/wrongNumOfReadingsTestData.csv");
    	connector = new HistoricalPirmasensStationCSVConnector(dataFilePath.toString(), connectorPropertiesFilePath);
    	try {
        	JSONObject values = connector.retrieveReadings(dataFilePath.toString(), 1);
        	Assert.fail();
        	} catch (Exception e) {
        	Assert.assertEquals("There was an error while retrieving the data from the CSV file! The current row retrieved from the CSV file is PS_Innenstadt';'Gesamt_UV_Strahlung';'20220707';0.21651599;0.09434250" , e.getMessage());
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
