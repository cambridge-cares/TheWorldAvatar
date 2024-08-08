package uk.ac.cam.cares.jps.base.timeseries.reducedtables;

import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesRDBClientWithReducedTables;

public class TimeSeriesPostGISIntegrationWithoutConnTest
        extends uk.ac.cam.cares.jps.base.timeseries.TimeSeriesPostGISIntegrationWithoutConnTest {

    @Override
    protected void setRdbClient() {
        tsClient = new TimeSeriesRDBClientWithReducedTables<>(Integer.class);
    }
}
