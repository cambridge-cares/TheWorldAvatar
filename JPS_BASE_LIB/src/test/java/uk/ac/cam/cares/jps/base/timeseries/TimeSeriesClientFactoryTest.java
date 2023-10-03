package uk.ac.cam.cares.jps.base.timeseries;

import java.lang.reflect.InvocationTargetException;
import java.sql.Connection;
import java.sql.SQLException;
import java.time.Instant;
import java.util.Arrays;
import java.util.List;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.testcontainers.containers.GenericContainer;
import org.testcontainers.containers.PostgreSQLContainer;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.utility.DockerImageName;

import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

public class TimeSeriesClientFactoryTest {
    @Container
    private static final GenericContainer<?> blazegraph = new GenericContainer<>(
            DockerImageName.parse("ghcr.io/cambridge-cares/blazegraph_for_tests:1.0.0"))
            .withExposedPorts(9999);
    @Container
    private PostgreSQLContainer<?> postgres = new PostgreSQLContainer<>("postgres:16.0");
    RemoteStoreClient remoteStoreClient;
    RemoteRDBStoreClient remoteRDBStoreClient;

    @Before
    public void initialiseSparqlClient() {
        // Start the container manually
        blazegraph.start();
        postgres.start();

        // Set up a kb client that points to the location of the triple store
        // This can be a RemoteStoreClient or the FileBasedStoreClient
        remoteStoreClient = new RemoteStoreClient();
        // Set endpoint to the triple store. The host and port are read from the
        // container
        String endpoint = "http://" + blazegraph.getHost() + ":" + blazegraph.getFirstMappedPort();
        // Default namespace in blazegraph is "kb", but in production a specific one
        // should be created
        endpoint = endpoint + "/blazegraph/namespace/kb/sparql";
        remoteStoreClient.setUpdateEndpoint(endpoint);
        remoteStoreClient.setQueryEndpoint(endpoint);

        remoteRDBStoreClient = new RemoteRDBStoreClient(postgres.getJdbcUrl(), postgres.getUsername(),
                postgres.getPassword());
    }

    @Test
    public void test() throws ClassNotFoundException, NoSuchMethodException, SecurityException, InstantiationException,
            IllegalAccessException, IllegalArgumentException, InvocationTargetException, SQLException {
        TimeSeriesRDBClient<Instant> timeSeriesRDBClient = new TimeSeriesRDBClient<>(Instant.class);
        TimeSeriesClient<Instant> tsClient = new TimeSeriesClient<>(remoteStoreClient, timeSeriesRDBClient);

        List<String> dataIriList = Arrays.asList("http://data1");
        try (Connection conn = remoteRDBStoreClient.getConnection()) {
            tsClient.initTimeSeries(dataIriList, Arrays.asList(Double.class), "timeUnit", conn);

            TimeSeriesClient<?> newTimeSeriesClient = TimeSeriesClientFactory.getInstance(remoteStoreClient,
                    dataIriList);

            Assert.assertEquals(postgres.getJdbcUrl(), newTimeSeriesClient.getRdbUrl());
        }

    }
}
