package uk.ac.cam.cares.jps.base.timeseries.test;

import org.jooq.CreateTableColumnStep;
import org.jooq.DSLContext;
import org.jooq.SQLDialect;
import org.jooq.impl.DSL;
import org.junit.Assert;
import org.junit.Test;
import org.junit.jupiter.api.Disabled;
import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.postgresql.util.PSQLException;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.interfaces.KnowledgeBaseClientInterface;
import uk.ac.cam.cares.jps.base.query.RemoteKnowledgeBaseClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesRDBClient;

import java.lang.reflect.Field;
import java.sql.Connection;
import java.sql.DriverManager;
import java.time.Instant;
import java.util.ArrayList;

public class TimeSeriesRDBClientTest {

    private DSLContext context = Mockito.mock(DSLContext.class, Mockito.RETURNS_DEEP_STUBS);
    private CreateTableColumnStep create = Mockito.mock(CreateTableColumnStep.class);
    private Connection connection = Mockito.mock(Connection.class);

    @Test
    public void testConstructor() throws NoSuchFieldException, IllegalAccessException {
        TimeSeriesRDBClient<Instant> client = new TimeSeriesRDBClient<>(Instant.class);
        // Retrieve the value of the private field 'timeColumn' of the client to check its value
        Field timeColumnField = client.getClass().getDeclaredField("timeColumn");
        timeColumnField.setAccessible(true);
        org.jooq.Field<String> timeColumn = (org.jooq.Field<String>) timeColumnField.get(client);
        Assert.assertEquals("time", timeColumn.getName());
        Assert.assertEquals(Instant.class, timeColumn.getType());
    }

    @Test
    public void testPrivateDatabaseRelatedFields() throws NoSuchFieldException, IllegalAccessException {
        TimeSeriesRDBClient<Instant> client = new TimeSeriesRDBClient<>(Instant.class);
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
    public void testSetKBClient() throws NoSuchFieldException, IllegalAccessException {
        TimeSeriesRDBClient<Instant> client = new TimeSeriesRDBClient<>(Instant.class);
        // Retrieve the value of the private field 'kbClient' of the client to check its value
        Field kbClientField = client.getClass().getDeclaredField("kbClient");
        kbClientField.setAccessible(true);

        Assert.assertNull(kbClientField.get(client));
        KnowledgeBaseClientInterface kbClient = new RemoteKnowledgeBaseClient();
        client.setKBClient(kbClient);
        Assert.assertSame(kbClient, kbClientField.get(client));
    }

    @Test
    public void testSetTimeUnit() throws NoSuchFieldException, IllegalAccessException {
        TimeSeriesRDBClient<Instant> client = new TimeSeriesRDBClient<>(Instant.class);
        // Retrieve the value of the private field 'timeUnit' of the client to check its value
        Field timeUnitField = client.getClass().getDeclaredField("timeUnit");
        timeUnitField.setAccessible(true);

        Assert.assertNull(timeUnitField.get(client));
        client.setTimeUnit("s");
        Assert.assertEquals("s", timeUnitField.get(client));
    }

    @Test
    public void testSetRdbURL() throws NoSuchFieldException, IllegalAccessException {
        TimeSeriesRDBClient<Instant> client = new TimeSeriesRDBClient<>(Instant.class);
        // Retrieve the value of the private field 'rdbURL' of the client to check its value
        Field rdbURLField = client.getClass().getDeclaredField("rdbURL");
        rdbURLField.setAccessible(true);

        Assert.assertNull(rdbURLField.get(client));
        client.setRdbURL("http://localhost:5342");
        Assert.assertEquals("http://localhost:5342", rdbURLField.get(client));
    }

    @Test
    public void testSetRdbUser() throws NoSuchFieldException, IllegalAccessException {
        TimeSeriesRDBClient<Instant> client = new TimeSeriesRDBClient<>(Instant.class);
        // Retrieve the value of the private field 'rdbUser' of the client to check its value
        Field rdbUserField = client.getClass().getDeclaredField("rdbUser");
        rdbUserField.setAccessible(true);

        Assert.assertNull(rdbUserField.get(client));
        client.setRdbUser("postgres");
        Assert.assertEquals("postgres", rdbUserField.get(client));
    }

    @Test
    public void testSetRdbPassword() throws NoSuchFieldException, IllegalAccessException {
        TimeSeriesRDBClient<Instant> client = new TimeSeriesRDBClient<>(Instant.class);
        // Retrieve the value of the private field 'rdbPassword' of the client to check its value
        Field rdbPasswordField = client.getClass().getDeclaredField("rdbPassword");
        rdbPasswordField.setAccessible(true);

        Assert.assertNull(rdbPasswordField.get(client));
        client.setRdbPassword("password");
        Assert.assertEquals("password", rdbPasswordField.get(client));
    }

    @Test
    public void testInitConnectionException() {
        TimeSeriesRDBClient<Instant> client = new TimeSeriesRDBClient<>(Instant.class);
        // To be able to mock the connection to the database we use Mockito
        // (whenever DriverManager is used in the try block we can mock the behaviour)
        try (MockedStatic<DriverManager> mockDriver = Mockito.mockStatic(DriverManager.class)) {
            mockDriver.when(() -> DriverManager.getConnection(null, null, null))
                    .thenThrow(PSQLException.class);
            client.init(new ArrayList<>(), new ArrayList<>());
            // Exception is not thrown
            Assert.fail();
        }
        catch (JPSRuntimeException e) {
            Assert.assertEquals(PSQLException.class, e.getCause().getClass());
        }
    }

    @Disabled("Works until the knowledge graph is queried in the init.")
    @Test
    public void testInitConnection() {
        TimeSeriesRDBClient<Instant> client = new TimeSeriesRDBClient<>(Instant.class);
        // To be able to mock the connection to the database we use Mockito
        // (whenever DriverManager or DSL is used in the try block we can mock the behaviour)
        try (MockedStatic<DriverManager> mockDriver = Mockito.mockStatic(DriverManager.class); MockedStatic<DSL> mockDSL = Mockito.mockStatic(DSL.class)) {
            mockDriver.when(() -> DriverManager.getConnection(null, null, null))
                    .thenReturn(connection);
            mockDSL.when(() -> DSL.using(connection, SQLDialect.POSTGRES))
                    .thenReturn(context);
            client.init(new ArrayList<>(), new ArrayList<>());
            // Mocks the behaviour of the the context when used to create a table
            Mockito.when(context.createTableIfNotExists("dbTable").column(Mockito.any())
                    .column(Mockito.any()).column(Mockito.any()).column(Mockito.any()).execute())
                    .thenReturn(1);
            Mockito.verify(context, Mockito.times(1)).createDatabaseIfNotExists("dbTable");
        }
    }

}
