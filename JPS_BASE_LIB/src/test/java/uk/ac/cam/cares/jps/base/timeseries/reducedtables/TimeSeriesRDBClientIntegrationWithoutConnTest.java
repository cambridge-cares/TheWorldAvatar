package uk.ac.cam.cares.jps.base.timeseries.reducedtables;

import java.time.Instant;

import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesRDBClientWithReducedTables;

public class TimeSeriesRDBClientIntegrationWithoutConnTest
        extends uk.ac.cam.cares.jps.base.timeseries.TimeSeriesRDBClientIntegrationWithoutConnTest {

    @Override
    protected void setRdbClient() {
        client = new TimeSeriesRDBClientWithReducedTables<>(Instant.class);
    }
}
