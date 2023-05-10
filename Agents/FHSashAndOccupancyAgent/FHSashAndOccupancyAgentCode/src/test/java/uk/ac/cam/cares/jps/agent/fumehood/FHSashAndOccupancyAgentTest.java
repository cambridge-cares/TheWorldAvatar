package uk.ac.cam.cares.jps.agent.fumehood;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;
import java.io.FileNotFoundException;
import java.io.FileWriter;
import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class FHSashAndOccupancyAgentTest {
	
    // Temporary folder to place a properties file
    @Rule
    public TemporaryFolder folder = new TemporaryFolder();
    Map<String, List<String>> map1;

    @Before
    public void createMockMaps() throws IOException {
        map1 = new HashMap<>();
        map1.put("FHandWFH", new ArrayList<>());
        map1.put("SashOpeningTsData", new ArrayList<>());
        map1.put("OccupiedStateTsData", new ArrayList<>());
        map1.get("FHandWFH").add("test IRI 1");
        map1.get("FHandWFH").add("test IRI 2");
        map1.get("FHandWFH").add("test IRI 3");
        map1.get("FHandWFH").add("test IRI 4");
        map1.get("FHandWFH").add("test IRI 5");
        map1.get("SashOpeningTsData").add("99.03234");
        map1.get("OccupiedStateTsData").add("This device does not have an occupied state.");
        map1.get("SashOpeningTsData").add("This device does not have a sash opening.");
        map1.get("OccupiedStateTsData").add("0.0");
        map1.get("SashOpeningTsData").add("0.0");
        map1.get("OccupiedStateTsData").add("0.0");
        map1.get("SashOpeningTsData").add("This device does not have a sash opening.");
        map1.get("OccupiedStateTsData").add("This device does not have an occupied state.");
        map1.get("SashOpeningTsData").add("79.03234");
        map1.get("OccupiedStateTsData").add("0.0");
    }
    
    @Test
    public void loadTSClientConfigsTest() throws NoSuchMethodException, IllegalAccessException, IOException, NoSuchFieldException {
    	FHSashAndOccupancyAgent testAgent = new FHSashAndOccupancyAgent();
        // Filepath to not yet created file in temporary test folder
    	String filepath = "";
        // Error messages
        String fileNotFound = "No properties file found at specified filepath: " + filepath;
        String noURL = "Properties file is missing \"db.url=<db_url>\"";
        String noUser = "Properties file is missing \"db.user=<db_user>\"";
        String noPassword = "Properties file is missing \"db.password=<db_password>\"";
        String noSparqlQueryEndpoint = "Properties file is missing \"sparql.query.endpoint=<sparql_query_endpoint>\"";
        String noSparqlUpdateEndpoint = "Properties file is missing \"sparql.update.endpoint=<sparql_update_endpoint>\"";
        // Set private method to be accessible
        Method loadTSClientConfig = FHSashAndOccupancyAgent.class.getDeclaredMethod("loadTSClientConfigs", String.class, String.class);
        loadTSClientConfig.setAccessible(true);

        // Test for non-existing properties file
        try {
            loadTSClientConfig.invoke(testAgent, filepath, filepath);
            Assert.fail();
        } catch (InvocationTargetException e) {
            Assert.assertEquals(FileNotFoundException.class, e.getCause().getClass());
            Assert.assertEquals(fileNotFound, e.getCause().getMessage());
        }

        filepath = Paths.get(folder.getRoot().toString(), "client.properties").toString();

        // Test for missing db url
        writePropertyFile(filepath, Arrays.asList("db.user=postgres", "db.password=postgres", "sparql.query.endpoint=test", "sparql.update.endpoint=test", "bg.username=bg_user", "bg.password=bg_password"));
        // Try loading config
        try {
            loadTSClientConfig.invoke(testAgent, filepath, filepath);
            Assert.fail();
        } catch (InvocationTargetException e) {
            Assert.assertEquals(IOException.class, e.getCause().getClass());
            Assert.assertEquals(noURL, e.getCause().getMessage());
        }

        // Test for missing db user
        writePropertyFile(filepath, Arrays.asList("db.url=test", "db.password=postgres", "sparql.query.endpoint=test", "sparql.update.endpoint=test", "bg.username=bg_user", "bg.password=bg_password"));
        // Try loading configs
        try {
            loadTSClientConfig.invoke(testAgent, filepath, filepath);
            Assert.fail();
        } catch (InvocationTargetException e) {
            Assert.assertEquals(IOException.class, e.getCause().getClass());
            Assert.assertEquals(noUser, e.getCause().getMessage());
        }
        
        // Test for missing db password
        writePropertyFile(filepath, Arrays.asList("db.url=test", "db.user=postgres", "sparql.query.endpoint=test", "sparql.update.endpoint=test", "bg.username=bg_user", "bg.password=bg_password"));
        // Try loading configs
        try {
            loadTSClientConfig.invoke(testAgent, filepath, filepath);
            Assert.fail();
        } catch (InvocationTargetException e) {
            Assert.assertEquals(IOException.class, e.getCause().getClass());
            Assert.assertEquals(noPassword, e.getCause().getMessage());
        }
        
        // Test for missing sparql query endpoint
        writePropertyFile(filepath, Arrays.asList("db.url=test", "db.user=postgres", "db.password=postgres", "sparql.update.endpoint=test", "bg.username=bg_user", "bg.password=bg_password"));
        // Try loading configs
        try {
            loadTSClientConfig.invoke(testAgent, filepath, filepath);
            Assert.fail();
        } catch (InvocationTargetException e) {
            Assert.assertEquals(IOException.class, e.getCause().getClass());
            Assert.assertEquals(noSparqlQueryEndpoint, e.getCause().getMessage());
        }
        
        // Test for missing sparql update endpoint
        writePropertyFile(filepath, Arrays.asList("db.url=test", "db.user=postgres", "db.password=postgres", "sparql.query.endpoint=test", "bg.username=bg_user", "bg.password=bg_password"));
        // Try loading configs
        try {
            loadTSClientConfig.invoke(testAgent, filepath, filepath);
            Assert.fail();
        } catch (InvocationTargetException e) {
            Assert.assertEquals(IOException.class, e.getCause().getClass());
            Assert.assertEquals(noSparqlUpdateEndpoint, e.getCause().getMessage());
        }
    }

    //test all possible combinations of SashOpeningTsData and OccupiedStateTsData values
    @Test
    public void testCheckSashAndOccupancy() throws NoSuchMethodException, SecurityException, IllegalAccessException, IllegalArgumentException, InvocationTargetException {
        Double threshold = 50.0;
        FHSashAndOccupancyAgent testAgent = new FHSashAndOccupancyAgent();
        // Set private method to be accessible
        Method checkSashAndOccupancy = FHSashAndOccupancyAgent.class.getDeclaredMethod("checkSashAndOccupancy", Map.class, Double.class);
        checkSashAndOccupancy.setAccessible(true);
        Boolean check = (Boolean) checkSashAndOccupancy.invoke(testAgent, map1, threshold);
        Assert.assertTrue(check);

        threshold = 90.0;
        check = (Boolean) checkSashAndOccupancy.invoke(testAgent, map1, threshold);
        Assert.assertFalse(check);

        //after removing map1.get("SashOpeningTsData").add("79.03234"); and map1.get("OccupiedStateTsData").add("0.0");
        //all subsequent combinations should return a false

        //map1.get("SashOpeningTsData").add("This device does not have a sash opening."); and map1.get("OccupiedStateTsData").add("This device does not have an occupied state.");
        //should return a false
        map1.get("FHandWFH").remove(map1.get("FHandWFH").size()-1);
        map1.get("SashOpeningTsData").remove(map1.get("SashOpeningTsData").size()-1);
        map1.get("OccupiedStateTsData").remove(map1.get("OccupiedStateTsData").size()-1);
        check = (Boolean) checkSashAndOccupancy.invoke(testAgent, map1, threshold);
        Assert.assertFalse(check);

        //map1.get("SashOpeningTsData").add("0.0"); and map1.get("OccupiedStateTsData").add("0.0"); should return a false
        map1.get("FHandWFH").remove(map1.get("FHandWFH").size()-1);
        map1.get("SashOpeningTsData").remove(map1.get("SashOpeningTsData").size()-1);
        map1.get("OccupiedStateTsData").remove(map1.get("OccupiedStateTsData").size()-1);
        check = (Boolean) checkSashAndOccupancy.invoke(testAgent, map1, threshold);
        Assert.assertFalse(check);

        //map1.get("SashOpeningTsData").add("This device does not have a sash opening."); and map1.get("OccupiedStateTsData").add("0.0"); should return a false
        map1.get("FHandWFH").remove(map1.get("FHandWFH").size()-1);
        map1.get("SashOpeningTsData").remove(map1.get("SashOpeningTsData").size()-1);
        map1.get("OccupiedStateTsData").remove(map1.get("OccupiedStateTsData").size()-1);
        check = (Boolean) checkSashAndOccupancy.invoke(testAgent, map1, threshold);
        Assert.assertFalse(check);

        //map1.get("SashOpeningTsData").add("99.03234"); and map1.get("OccupiedStateTsData").add("This device does not have an occupied state."); should return a false
        map1.get("FHandWFH").remove(map1.get("FHandWFH").size()-1);
        map1.get("SashOpeningTsData").remove(map1.get("SashOpeningTsData").size()-1);
        map1.get("OccupiedStateTsData").remove(map1.get("OccupiedStateTsData").size()-1);
        check = (Boolean) checkSashAndOccupancy.invoke(testAgent, map1, threshold);
        Assert.assertFalse(check);
        
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
}
