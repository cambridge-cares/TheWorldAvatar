package uk.ac.cam.cares.jps.agent.cea.utils.input;

import static org.mockito.Mockito.*;
import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.Test;
import org.mockito.MockedConstruction;

import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;
import uk.ac.cam.cares.jps.agent.cea.utils.uri.OntologyURIHelper;

import org.json.JSONArray;
import org.json.JSONObject;
import java.util.ArrayList;
import java.util.List;
import java.sql.SQLException;
import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.Statement;
import org.locationtech.jts.geom.Coordinate;

public class TerrainHelperTest {
    @Test
    public void testGetTerrain() throws SQLException {
        OntologyURIHelper ontologyURIHelper = new OntologyURIHelper("CEAAgentConfig");

        TerrainHelper terrainHelper = new TerrainHelper("", "", "");

        byte[] testBytes = {1, 2, 3, 4, 5};
        Connection mockConnection = mock(Connection.class);
        Statement mockStatement = mock(Statement.class);
        ResultSet mockResultSet = mock(ResultSet.class);

        doReturn(mockStatement).when(mockConnection).createStatement();
        doReturn(mockResultSet).when(mockStatement).executeQuery(anyString());
        when(mockResultSet.next()).thenReturn(true).thenReturn(false);
        doReturn(testBytes).when(mockResultSet).getBytes(anyString());

        List<Coordinate> testSurroundings = new ArrayList<>();

        testSurroundings.add(new Coordinate(1.0, 1.0));
        testSurroundings.add(new Coordinate(2.0, 1.0));
        testSurroundings.add(new Coordinate(2.0, 2.0));
        testSurroundings.add(new Coordinate(1.0, 2.0));
        testSurroundings.add(new Coordinate(1.0, 1.0));

        try (MockedConstruction<RemoteRDBStoreClient> remoteRDBStoreClientMock = mockConstruction(RemoteRDBStoreClient.class,
                (mock, context) -> {
                    doReturn(new JSONArray().put(new JSONObject().put("srid", 4326))).when(mock).executeQuery(anyString());
                    doReturn(mockConnection).when(mock).getConnection();
                })) {

            byte[] result = terrainHelper.getTerrain("", "", "32633", testSurroundings, "", ontologyURIHelper);

            assertEquals(testBytes.length, result.length);

            for (int i = 0; i < testBytes.length; i++) {
                assertEquals(testBytes[i], result[i]);
            }
        }
    }
}
