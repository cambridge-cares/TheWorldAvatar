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
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesSparql;

import java.lang.reflect.Field;
import java.sql.Connection;
import java.sql.DriverManager;
import java.time.Instant;
import java.util.ArrayList;

public class TimeSeriesSparqlTest {
	
//	// Create mocks
//    private DSLContext context = Mockito.mock(DSLContext.class, Mockito.RETURNS_DEEP_STUBS);
//
//    @Test
//    public void testConstructor() throws IllegalArgumentException, IllegalAccessException {
//    	TimeSeriesSparql client = new TimeSeriesSparql();
//
//        // Test for correct field name and class
//        Assert.assertEquals("time", timeColumn.getName());
//        Assert.assertEquals(Instant.class, timeColumn.getType());
//    }
//    
//    @Test
//    public void testSetKBClient() throws NoSuchFieldException, SecurityException, IllegalArgumentException, IllegalAccessException {
//        
//    	RemoteKnowledgeBaseClient kbClient = new RemoteKnowledgeBaseClient();
//    	TimeSeriesRDBClient<Instant> client = new TimeSeriesRDBClient<>(Instant.class);
//        // Retrieve the value of the private field 'kbClient' of the client to check its value
//        Field kbClientField = client.getClass().getDeclaredField("kbClient");
//        kbClientField.setAccessible(true);
//
//        Assert.assertNull(kbClientField.get(client));
//        KnowledgeBaseClientInterface kbClient = new RemoteKnowledgeBaseClient();
//        //client.setKBClient(kbClient);
//        Assert.assertSame(kbClient, kbClientField.get(client));
//    }
}
