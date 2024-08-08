package uk.ac.cam.cares.jps.base.timeseries.reducedtables;

import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesRDBClientWithReducedTables;

public class TimeSeriesPostGISIntegrationTest
        extends uk.ac.cam.cares.jps.base.timeseries.TimeSeriesPostGISIntegrationTest {

    @Override
    protected void setRdbClient() {
        tsClient = new TimeSeriesRDBClientWithReducedTables<>(Integer.class);
    }
}
