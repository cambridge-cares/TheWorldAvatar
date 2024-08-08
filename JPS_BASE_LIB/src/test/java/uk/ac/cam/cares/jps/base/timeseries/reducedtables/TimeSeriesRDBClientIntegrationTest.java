package uk.ac.cam.cares.jps.base.timeseries.reducedtables;

import java.time.Instant;

import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesRDBClientWithReducedTables;

public class TimeSeriesRDBClientIntegrationTest
        extends uk.ac.cam.cares.jps.base.timeseries.TimeSeriesRDBClientIntegrationTest {

    @Override
    protected void setRdbClient() {
        client = new TimeSeriesRDBClientWithReducedTables<>(Instant.class);
    }
}
