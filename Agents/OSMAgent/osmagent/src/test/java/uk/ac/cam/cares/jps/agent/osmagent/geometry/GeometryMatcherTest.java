package uk.ac.cam.cares.jps.agent.osmagent.geometry;

import static org.mockito.Mockito.*;

import org.junit.jupiter.api.Test;
import org.mockito.MockedConstruction;

import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;

import org.json.JSONArray;
import org.json.JSONObject;

import java.sql.Connection;
import java.sql.SQLException;
import java.sql.Statement;

public class GeometryMatcherTest {
    @Test
    public void testMatchGeometry() throws SQLException {
        Connection connMock = mock(Connection.class);

        Statement statementMock = mock(Statement.class);

        JSONArray sridJSON = new JSONArray().put(new JSONObject().put("srid", 4326));
        JSONArray idJSON = new JSONArray().put(new JSONObject().put("min", 0).put("max", 100));

        doReturn(statementMock).when(connMock).createStatement();
        try (MockedConstruction<RemoteRDBStoreClient> rdbStoreClientMock = mockConstruction(RemoteRDBStoreClient.class,
                (mock, context) -> {
                        doReturn(connMock).when(mock).getConnection();
                        when(mock.executeQuery(anyString())).thenReturn(sridJSON)
                            .thenReturn(sridJSON).thenReturn(idJSON).thenReturn(idJSON);
                })) {
           new GeometryMatcher("","","").matchGeometry("", "", null, null);

           verify(rdbStoreClientMock.constructed().get(0), times(4)).executeQuery(anyString());
           verify(rdbStoreClientMock.constructed().get(0), times(4)).getConnection();

           verify(connMock, times(4)).createStatement();
           verify(statementMock, times(205)).execute(anyString());
        }
    }
}
