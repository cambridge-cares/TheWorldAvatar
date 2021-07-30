package uk.ac.cam.cares.jps.base.timeseries.test;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import uk.ac.cam.cares.jps.base.interfaces.StoreClientInterface;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesRDBClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesSparql;

import java.io.IOException;
import java.lang.reflect.Field;
import java.net.URISyntaxException;
import java.nio.file.Paths;
import java.time.Instant;
import java.util.Objects;

public class TimeSeriesClientTest {

    @Test
    public void testConstructorWithKBClient() throws IOException, NoSuchFieldException, IllegalAccessException, URISyntaxException {
        RemoteStoreClient kbClient = new RemoteStoreClient();
        kbClient.setQueryEndpoint("sparql_query");
        kbClient.setUpdateEndpoint("sparql_update");
        // nk591: Can this be done easier?
        TimeSeriesClient<Instant> client = new TimeSeriesClient<>(kbClient, Instant.class,
                Paths.get(Objects.requireNonNull(getClass().getResource("/timeseries.properties")).toURI()).toString());

        // Make private field accessible and retrieve the rdf client to test whether it is set correctly
        Field rdfClientField = TimeSeriesClient.class.getDeclaredField("rdfClient");
        rdfClientField.setAccessible(true);
        TimeSeriesSparql rdfClient = (TimeSeriesSparql) rdfClientField.get(client);
        Field kbClientField = TimeSeriesSparql.class.getDeclaredField("kbClient");
        kbClientField.setAccessible(true);
        StoreClientInterface setKBClient = (StoreClientInterface) kbClientField.get(rdfClient);
        Assertions.assertEquals(kbClient.getQueryEndpoint(), setKBClient.getQueryEndpoint());
        Assertions.assertEquals(kbClient.getUpdateEndpoint(), setKBClient.getUpdateEndpoint());
        // Make private field accessible and retrieve the rdb client to test whether it is set correctly
        Field rdbClientField = TimeSeriesClient.class.getDeclaredField("rdbClient");
        rdbClientField.setAccessible(true);
        TimeSeriesRDBClient<Instant> rdbClient = (TimeSeriesRDBClient<Instant>) rdbClientField.get(client);
        Assertions.assertEquals("jdbc:postgresql:timeseries", rdbClient.getRdbURL());
        Assertions.assertEquals("postgres", rdbClient.getRdbUser());
    }


    @Test
    public void testConstructorWithOnlyPropertiesFile() throws IOException, NoSuchFieldException, IllegalAccessException, URISyntaxException {
        // nk591: Can this be done easier?
        TimeSeriesClient<Instant> client = new TimeSeriesClient<>(Instant.class,
                Paths.get(Objects.requireNonNull(getClass().getResource("/timeseries.properties")).toURI()).toString());

        // Make private field accessible and retrieve the rdf client to test whether it is set correctly
        Field rdfClientField = TimeSeriesClient.class.getDeclaredField("rdfClient");
        rdfClientField.setAccessible(true);
        TimeSeriesSparql rdfClient = (TimeSeriesSparql) rdfClientField.get(client);
        Field kbClientField = TimeSeriesSparql.class.getDeclaredField("kbClient");
        kbClientField.setAccessible(true);
        StoreClientInterface setKBClient = (StoreClientInterface) kbClientField.get(rdfClient);
        Assertions.assertEquals("http://localhost:9999/blazegraph/namespace/timeseries/sparql", setKBClient.getQueryEndpoint());
        Assertions.assertEquals("http://localhost:9999/blazegraph/namespace/timeseries/sparql", setKBClient.getUpdateEndpoint());
        // Make private field accessible and retrieve the rdb client to test whether it is set correctly
        Field rdbClientField = TimeSeriesClient.class.getDeclaredField("rdbClient");
        rdbClientField.setAccessible(true);
        TimeSeriesRDBClient<Instant> rdbClient = (TimeSeriesRDBClient<Instant>) rdbClientField.get(client);
        Assertions.assertEquals("jdbc:postgresql:timeseries", rdbClient.getRdbURL());
        Assertions.assertEquals("postgres", rdbClient.getRdbUser());
    }

    @Test
    public void testSetKBClient() throws IOException, NoSuchFieldException, IllegalAccessException, URISyntaxException {
        RemoteStoreClient kbClient = new RemoteStoreClient();
        kbClient.setQueryEndpoint("sparql_query");
        kbClient.setUpdateEndpoint("sparql_update");
        // nk591: Can this be done easier?
        TimeSeriesClient<Instant> client = new TimeSeriesClient<>(Instant.class,
                Paths.get(Objects.requireNonNull(getClass().getResource("/timeseries.properties")).toURI()).toString());
        client.setKBClient(kbClient);
        // Make private field accessible and retrieve the rdf client to test whether it is set correctly
        Field rdfClientField = TimeSeriesClient.class.getDeclaredField("rdfClient");
        rdfClientField.setAccessible(true);
        TimeSeriesSparql rdfClient = (TimeSeriesSparql) rdfClientField.get(client);
        Field kbClientField = TimeSeriesSparql.class.getDeclaredField("kbClient");
        kbClientField.setAccessible(true);
        StoreClientInterface setKBClient = (StoreClientInterface) kbClientField.get(rdfClient);
        Assertions.assertEquals(kbClient.getQueryEndpoint(), setKBClient.getQueryEndpoint());
        Assertions.assertEquals(kbClient.getUpdateEndpoint(), setKBClient.getUpdateEndpoint());
    }

    @Test
    public void testSetRDBClient() throws IOException, NoSuchFieldException, IllegalAccessException, URISyntaxException {
        // nk591: Can this be done easier?
        TimeSeriesClient<Instant> client = new TimeSeriesClient<>(Instant.class,
                Paths.get(Objects.requireNonNull(getClass().getResource("/timeseries.properties")).toURI()).toString());
        client.setRDBClient("testURL", "user", "password");
        // Make private field accessible and retrieve the rdb client to test whether it is set correctly
        Field rdbClientField = TimeSeriesClient.class.getDeclaredField("rdbClient");
        rdbClientField.setAccessible(true);
        TimeSeriesRDBClient<Instant> rdbClient = (TimeSeriesRDBClient<Instant>) rdbClientField.get(client);
        Assertions.assertEquals("testURL", rdbClient.getRdbURL());
        Assertions.assertEquals("user", rdbClient.getRdbUser());
        Field passwordField = TimeSeriesRDBClient.class.getDeclaredField("rdbPassword");
        passwordField.setAccessible(true);
        Assertions.assertEquals("password", passwordField.get(rdbClient));
    }

}
