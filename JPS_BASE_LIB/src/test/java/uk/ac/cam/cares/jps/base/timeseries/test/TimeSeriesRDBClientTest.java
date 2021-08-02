package uk.ac.cam.cares.jps.base.timeseries.test;

import org.jooq.SQLDialect;
import org.junit.Assert;
import org.junit.Test;
import org.junit.Ignore;
import java.io.*;

import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesRDBClient;

import java.lang.reflect.Field;
import java.time.Instant;
import java.util.Arrays;
import java.util.List;

/**
 * This class provides unit tests for the TimeSeriesRDBClient class
 * <p>Selected functionality is mocked using Mockito
 */

public class TimeSeriesRDBClientTest {

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
    public void testSetAndGetTimeUnit() {
        TimeSeriesRDBClient<Instant> client = new TimeSeriesRDBClient<>(Instant.class);
        Assert.assertNull(client.getTimeUnit());
        client.setTimeUnit("s");
        Assert.assertEquals("s", client.getTimeUnit());
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
    @Ignore("Overrides potentially existing timeseries.properties file in \"src/main/resources/\"")
    public void testLoadRdbConfig () {
    	TimeSeriesRDBClient<Instant> client = new TimeSeriesRDBClient<>(Instant.class);
    	String filepath = "src/test/resources/timeseries.properties";
    	// Delete potentially existing properties file
    	new File(filepath).delete();
    	// JPSRuntime error messages
    	String m1 = "TimeSeriesRDBClient: No properties file found at specified filepath: " + filepath;
    	String m2 = "TimeSeriesRDBClient: Properties file is missing \"db.url=<rdb_url>\" ";
    	String m3 = "TimeSeriesRDBClient: Properties file is missing \"db.user=<rdb_username>\" ";
    	String m4 = "TimeSeriesRDBClient: Properties file is missing \"db.password=<rdb_password>\" ";
    	// Test for non-existing properties file
    	boolean thrown = false;
    	try {
    		client.loadRdbConfigs(filepath);
    	} catch (Exception e) {
    		Assert.assertEquals(m1, e.getMessage());
    		thrown = true;
    	}
    	Assert.assertTrue(thrown);
    	// Test for missing Rdb URL
    	writePropertyFile(filepath, Arrays.asList("db.user=test_user", "db.password=test_password"));
	    // Try loading RDB configs
    	thrown = false;
    	try {
    		client.loadRdbConfigs(filepath);
    	} catch (Exception e) {
    		Assert.assertEquals(m2, e.getMessage());
    		thrown = true;
    	}
    	Assert.assertTrue(thrown);
    	// Test for missing user name
    	writePropertyFile(filepath, Arrays.asList("db.url=test_url", "db.password=test_password"));
	    // Try loading RDB configs
    	thrown = false;
    	try {
    		client.loadRdbConfigs(filepath);
    	} catch (Exception e) {
    		Assert.assertEquals(m3, e.getMessage());
    		thrown = true;
    	}
    	Assert.assertTrue(thrown);
    	// Test for missing password
    	writePropertyFile(filepath, Arrays.asList("db.url=test_url", "db.user=test_user"));
	    // Try loading RDB configs
    	thrown = false;
    	try {
    		client.loadRdbConfigs(filepath);
    	} catch (Exception e) {
    		Assert.assertEquals(m4, e.getMessage());
    		thrown = true;
    	}
    	Assert.assertTrue(thrown);
    	// Test for proper URL, username and password
    	writePropertyFile(filepath, Arrays.asList("db.url=test_url", "db.user=test_user", "db.password=test_password"));
	    // Try loading RDB configs
    	thrown = false;
    	try {
    		client.loadRdbConfigs(filepath);
    	} catch (Exception e) {
    		thrown = true;
    	}
    	Assert.assertFalse(thrown);
    	Assert.assertEquals("test_url", client.getRdbURL());
    	Assert.assertEquals("test_user", client.getRdbUser());
    }
    
    private void writePropertyFile(String filepath, List<String> properties) {
    	try {
    		// Create empty properties file
    		File file = new File(filepath);
    		FileWriter writer = new FileWriter(file);
    		// Populate file
    		for (String s : properties) {
    			writer.write(s + "\n");
    		}
    	    writer.close();
    	} catch (Exception e) {
    		e.printStackTrace();
    	} 
    }
}
