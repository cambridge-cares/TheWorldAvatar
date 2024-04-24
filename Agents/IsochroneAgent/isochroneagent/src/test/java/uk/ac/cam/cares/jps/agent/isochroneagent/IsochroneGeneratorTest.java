package uk.ac.cam.cares.jps.agent.isochroneagent;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.fail;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.Statement;
import java.util.List;
import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;
import org.junit.jupiter.api.Test;
import java.util.Arrays;

public class IsochroneGeneratorTest {
    @Test
    public void getPoiTypesTest() {
        // Test the public method getPoiTypes
            try {
                // Mock the behavior of the Connection, Statement, and ResultSet
                Connection connectionMock = mock(Connection.class);
                Statement statementMock = mock(Statement.class);
                ResultSet resultSetMock = mock(ResultSet.class);
                RemoteRDBStoreClient remoteRDBStoreClientMock = mock (RemoteRDBStoreClient.class);

                // Mock the ResultSet to return some sample data
                doReturn(connectionMock).when(remoteRDBStoreClientMock).getConnection();
                doReturn(statementMock).when(connectionMock).createStatement();
                doReturn(resultSetMock).when(statementMock).executeQuery(anyString());
                doReturn(true).doReturn(true).doReturn(false).when(resultSetMock).next();
                doReturn("POI1", "POI2").when(resultSetMock).getString("poi_type");

                IsochroneGenerator isochroneGenerator = new IsochroneGenerator();

                List<String> poiTypes = isochroneGenerator.getPoiTypes(remoteRDBStoreClientMock);
                
                // Verify that the getPoiTypes method returned the expected list of poiTypes
                assertEquals(Arrays.asList("POI1", "POI2"), poiTypes);
            
            } catch (Exception e) {
                fail("Exception occurred: " + e.getMessage());
            }
        
    }

}

