package uk.ac.cam.cares.jps.base.timeseries.test;

import org.jooq.CreateTableColumnStep;
import org.jooq.DSLContext;
import org.jooq.SQLDialect;
import org.jooq.impl.DSL;
import org.junit.Assert;
import org.junit.Test;
import org.junit.Ignore;
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

public class TimeSeriesSparqlTest {
	
	// Create mocks
    private DSLContext context = Mockito.mock(DSLContext.class, Mockito.RETURNS_DEEP_STUBS);
    private CreateTableColumnStep create = Mockito.mock(CreateTableColumnStep.class);
    private Connection connection = Mockito.mock(Connection.class);

    @Test
    @Ignore("To be moved to KB client class")
    public void testSetKBClient() throws NoSuchFieldException, SecurityException, IllegalArgumentException, IllegalAccessException {
        TimeSeriesRDBClient<Instant> client = new TimeSeriesRDBClient<>(Instant.class);
        // Retrieve the value of the private field 'kbClient' of the client to check its value
        Field kbClientField = client.getClass().getDeclaredField("kbClient");
        kbClientField.setAccessible(true);

        Assert.assertNull(kbClientField.get(client));
        KnowledgeBaseClientInterface kbClient = new RemoteKnowledgeBaseClient();
        //client.setKBClient(kbClient);
        Assert.assertSame(kbClient, kbClientField.get(client));
    }
}
