package uk.ac.cam.cares.jps.base.timeseries;

import java.lang.reflect.InvocationTargetException;
import java.sql.Connection;
import java.sql.SQLException;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.testcontainers.containers.PostgreSQLContainer;
import org.testcontainers.junit.jupiter.Container;

import uk.ac.cam.cares.jps.base.BlazegraphContainer;
import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

public class TimeSeriesClientFactoryTest {
    @Container
    private static final BlazegraphContainer blazegraph = new BlazegraphContainer();
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
        remoteStoreClient = blazegraph.getRemoteStoreClient();

        remoteRDBStoreClient = new RemoteRDBStoreClient(postgres.getJdbcUrl(), postgres.getUsername(),
                postgres.getPassword());
    }

    @Test
    public void testTimeSeriesClientFactory()
            throws ClassNotFoundException, NoSuchMethodException, SecurityException, InstantiationException,
            IllegalAccessException, IllegalArgumentException, InvocationTargetException, SQLException {
        TimeSeriesRDBClient<Instant> timeSeriesRDBClient = new TimeSeriesRDBClient<>(Instant.class);
        TimeSeriesClient<Instant> tsClient = new TimeSeriesClient<>(remoteStoreClient, timeSeriesRDBClient);

        String dataIri = "http://data1";
        List<String> dataIriList = Arrays.asList(dataIri);

        try (Connection conn = remoteRDBStoreClient.getConnection()) {
            tsClient.initTimeSeries(dataIriList, Arrays.asList(Double.class), "timeUnit", conn);

            Instant timestamp = Instant.now();
            double insertedValue = 10.0;

            List<List<?>> values = new ArrayList<>();
            values.add(Arrays.asList(insertedValue));
            TimeSeries<Instant> timeSeries = new TimeSeries<>(Arrays.asList(timestamp), dataIriList,
                    values);

            tsClient.addTimeSeriesData(timeSeries, conn);

            TimeSeriesClient<?> newTimeSeriesClient = TimeSeriesClientFactory.getInstance(remoteStoreClient,
                    dataIriList);

            TimeSeries<?> queriedTimeSeries = newTimeSeriesClient.getTimeSeries(dataIriList, conn);

            // queried Instant value is in different precision
            Assert.assertEquals(timestamp.getEpochSecond(),
                    ((Instant) queriedTimeSeries.getTimes().get(0)).getEpochSecond());

            Assert.assertEquals((Double) insertedValue,
                    (Double) queriedTimeSeries.getValuesAsDouble(dataIri).get(0));

            Assert.assertEquals(postgres.getJdbcUrl(), newTimeSeriesClient.getRdbUrl());
        }
    }
}
