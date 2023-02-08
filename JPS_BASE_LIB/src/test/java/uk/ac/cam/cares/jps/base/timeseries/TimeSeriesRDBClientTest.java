package uk.ac.cam.cares.jps.base.timeseries;

import org.junit.Assert;
import org.junit.ClassRule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

import java.io.FileWriter;
import java.io.IOException;
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
    public static TemporaryFolder folder = new TemporaryFolder();
    
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
