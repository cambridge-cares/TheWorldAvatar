package uk.ac.cam.cares.jps.base.timeseries.test;


import org.jooq.SQLDialect;
import org.junit.AfterClass;
import org.junit.Assert;
import org.junit.ClassRule;
import org.junit.Rule;
import org.junit.Test;
import java.io.*;

import org.junit.rules.TemporaryFolder;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesRDBClient;

import java.lang.reflect.Field;
import java.nio.file.Paths;
import java.time.Instant;
import java.util.Arrays;
import java.util.List;

/**
 * This class provides unit tests for the TimeSeriesRDBClient class
 */

public class TimeSeriesRDBClientTest {

    @ClassRule
    // Temporary folder to place a properties file (same file for all potential tests)
    public static TemporaryFolder folder = new TemporaryFolder();
    
    @AfterClass
    public static void deleteTemporaryFolder() throws IOException {
    	// Normally junit temporary folder would take care of this; however no exception is thrown if
    	// deletion fails (as AfterClass method); therefore, it is safer to delete an verify
    	
		// delete property file to clear folder
		File file = new File(Paths.get(folder.getRoot().toString(), "timeseries.properties").toString());
		System.out.println(file.exists());
		if (file.exists()) {
			file.delete();
		}
		
		// delete empty folder
		File dir = new File(folder.getRoot().toString());
		System.out.println(dir.exists());
		if (dir.exists()) {
			dir.delete();
		}		
		
		if (folder.getRoot().exists()) {
			System.out.println("Temporary folder deletion failed: " + folder.getRoot().toString());
		} else {
			System.out.println("Temporary folder successfully deleted");
		}

    }

    @Test
    public void testConstructor() throws NoSuchFieldException, SecurityException, IllegalArgumentException, IllegalAccessException {
        TimeSeriesRDBClient<Instant> client = new TimeSeriesRDBClient<>(Instant.class);
        // Retrieve the value of the private field 'timeColumn' of the client to check its value
        Field timeColumnField = client.getClass().getDeclaredField("timeColumn");
        timeColumnField.setAccessible(true);
        org.jooq.Field<String> timeColumn = (org.jooq.Field<String>) timeColumnField.get(client);
        // Test for correct field name and class
        Assert.assertEquals("time", timeColumn.getName());
        Assert.assertEquals(Instant.class, timeColumn.getType());
    }

    @Test
    public void testPrivateDatabaseRelatedFields() throws NoSuchFieldException, SecurityException, IllegalArgumentException, IllegalAccessException {
        TimeSeriesRDBClient<Instant> client = new TimeSeriesRDBClient<>(Instant.class);
        // Retrieve the value of the private field 'dialect' of the client to check its value
        Field dialectField = client.getClass().getDeclaredField("dialect");
        dialectField.setAccessible(true);
        SQLDialect dialect = (SQLDialect) dialectField.get(client);
        Assert.assertEquals(SQLDialect.POSTGRES, dialect);
        // Retrieve the value of the private field 'dbTableName' of the client to check its value
        Field tableNameField = client.getClass().getDeclaredField("dbTableName");
        tableNameField.setAccessible(true);
        String tableName = (String) tableNameField.get(client);
        Assert.assertEquals("dbTable", tableName);
        // Retrieve the value of the private field 'dataIRIcolumn' of the client to check its value
        Field dataIRIcolumnField = client.getClass().getDeclaredField("dataIRIcolumn");
        dataIRIcolumnField.setAccessible(true);
        org.jooq.Field<String> dataIRIcolumn = (org.jooq.Field<String>) dataIRIcolumnField.get(client);
        Assert.assertEquals("dataIRI", dataIRIcolumn.getName());
        Assert.assertEquals(String.class, dataIRIcolumn.getType());
        // Retrieve the value of the private field 'tsIRIcolumn' of the client to check its value
        Field tsIRIcolumnField = client.getClass().getDeclaredField("tsIRIcolumn");
        tsIRIcolumnField.setAccessible(true);
        org.jooq.Field<String> tsIRIcolumn = (org.jooq.Field<String>) tsIRIcolumnField.get(client);
        Assert.assertEquals("timeseriesIRI", tsIRIcolumn.getName());
        Assert.assertEquals(String.class, tsIRIcolumn.getType());
        // Retrieve the value of the private field 'tsTableNameColumn' of the client to check its value
        Field tsTableNameColumnField = client.getClass().getDeclaredField("tsTableNameColumn");
        tsTableNameColumnField.setAccessible(true);
        org.jooq.Field<String> tsTableNameColumn = (org.jooq.Field<String>) tsTableNameColumnField.get(client);
        Assert.assertEquals("tableName", tsTableNameColumn.getName());
        Assert.assertEquals(String.class, tsTableNameColumn.getType());
        // Retrieve the value of the private field 'columnNameColumn' of the client to check its value
        Field columnNameColumnField = client.getClass().getDeclaredField("columnNameColumn");
        columnNameColumnField.setAccessible(true);
        org.jooq.Field<String> columnNameColumn = (org.jooq.Field<String>) columnNameColumnField.get(client);
        Assert.assertEquals("columnName", columnNameColumn.getName());
        Assert.assertEquals(String.class, columnNameColumn.getType());
    }

    @Test
    public void testSetAndGetRdbURL() {
        TimeSeriesRDBClient<Instant> client = new TimeSeriesRDBClient<>(Instant.class);
        Assert.assertNull(client.getRdbURL());
        client.setRdbURL("http://localhost:5342");
        Assert.assertEquals("http://localhost:5342", client.getRdbURL());
    }

    @Test
    public void testSetAndGetRdbUser() {
        TimeSeriesRDBClient<Instant> client = new TimeSeriesRDBClient<>(Instant.class);
        Assert.assertNull(client.getRdbUser());
        client.setRdbUser("postgres");
        Assert.assertEquals("postgres", client.getRdbUser());
    }

    @Test
    public void testSetRdbPassword() throws NoSuchFieldException, SecurityException, IllegalArgumentException, IllegalAccessException {
        TimeSeriesRDBClient<Instant> client = new TimeSeriesRDBClient<>(Instant.class);
        // Retrieve the value of the private field 'rdbPassword' of the client to check its value
        Field rdbPasswordField = client.getClass().getDeclaredField("rdbPassword");
        rdbPasswordField.setAccessible(true);

        Assert.assertNull(rdbPasswordField.get(client));
        client.setRdbPassword("password");
        Assert.assertNotNull(rdbPasswordField.get(client));
        Assert.assertEquals("password", rdbPasswordField.get(client));
    }

    @Test
    public void testLoadRdbConfig () throws IOException {
    	TimeSeriesRDBClient<Instant> client = new TimeSeriesRDBClient<>(Instant.class);
    	// Filepath to not yet created file in temporary test folder
    	String filepath = Paths.get(folder.getRoot().toString(), "timeseries.properties").toString();
    	// JPSRuntime error messages
    	String m1 = "TimeSeriesRDBClient: No properties file found at specified filepath: " + filepath;
    	String m2 = "TimeSeriesRDBClient: Properties file is missing \"db.url=<rdb_url>\" ";
    	String m3 = "TimeSeriesRDBClient: Properties file is missing \"db.user=<rdb_username>\" ";
    	String m4 = "TimeSeriesRDBClient: Properties file is missing \"db.password=<rdb_password>\" ";
    	
    	// Test for non-existing properties file
    	try {
    		client.loadRdbConfigs(filepath);
    		Assert.fail();
    	} catch (JPSRuntimeException e) {
    		Assert.assertEquals(m1, e.getMessage());
    	}
    	
    	// Test for missing Rdb URL by creating a file only containing user and password
    	writePropertyFile(filepath, Arrays.asList("db.user=test_user", "db.password=test_password"));
	    // Try loading RDB configs
    	try {
    		client.loadRdbConfigs(filepath);
            Assert.fail();
    	} catch (JPSRuntimeException e) {
    		Assert.assertEquals(m2, e.getMessage());
    	}

    	// Test for missing user name by creating a file only containing url and password
    	writePropertyFile(filepath, Arrays.asList("db.url=test_url", "db.password=test_password"));
	    // Try loading RDB configs
    	try {
    		client.loadRdbConfigs(filepath);
            Assert.fail();
    	} catch (JPSRuntimeException e) {
    		Assert.assertEquals(m3, e.getMessage());
    	}

    	// Test for missing password  by creating a file only containing url and user
    	writePropertyFile(filepath, Arrays.asList("db.url=test_url", "db.user=test_user"));
	    // Try loading RDB configs
    	try {
    		client.loadRdbConfigs(filepath);
            Assert.fail();
    	} catch (JPSRuntimeException e) {
    		Assert.assertEquals(m4, e.getMessage());
    	}

    	// Test for proper URL, username and password
    	writePropertyFile(filepath, Arrays.asList("db.url=test_url", "db.user=test_user", "db.password=test_password"));
	    // Try loading RDB configs
    	try {
    		client.loadRdbConfigs(filepath);
    	} catch (Exception e) {
    	    Assert.fail(e.getMessage());
    	}
    	Assert.assertEquals("test_url", client.getRdbURL());
    	Assert.assertEquals("test_user", client.getRdbUser());
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
