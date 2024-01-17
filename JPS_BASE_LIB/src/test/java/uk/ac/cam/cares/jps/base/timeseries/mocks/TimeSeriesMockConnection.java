package uk.ac.cam.cares.jps.base.timeseries.mocks;

import org.jooq.tools.jdbc.MockConnection;
import org.jooq.tools.jdbc.MockDataProvider;

public class TimeSeriesMockConnection extends MockConnection {
    public TimeSeriesMockConnection(MockDataProvider mockDataProvider) {
        super(mockDataProvider);
    }

    public MockDatabaseMetaData getMetaData() {
        return new MockDatabaseMetaData();
    }
}
