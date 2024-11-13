package uk.ac.cam.cares.jps.base.timeseries.reducedtables;

import java.time.Instant;

import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesRDBClientWithReducedTables;

public class TimeSeriesClientIntegrationTest
        extends uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClientIntegrationTest {
    @Override
    protected void setTsClient(RemoteStoreClient remoteStoreClient) {
        TimeSeriesRDBClientWithReducedTables<Instant> rdbClient = new TimeSeriesRDBClientWithReducedTables<>(
                Instant.class);
        tsClient = new TimeSeriesClient<>(remoteStoreClient, rdbClient);
    }
}
