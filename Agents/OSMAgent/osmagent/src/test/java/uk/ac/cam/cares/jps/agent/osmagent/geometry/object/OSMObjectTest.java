package uk.ac.cam.cares.jps.agent.osmagent.geometry.object;

import static org.mockito.Mockito.*;
import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.Test;
import org.mockito.MockedConstruction;

import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;

import org.json.JSONArray;
import org.json.JSONObject;
import java.util.Map;

public class OSMObjectTest {
    @Test
    public void testGetOSMObject() {
        JSONArray testArray = new JSONArray();
        testArray.put(new JSONObject().put("ogc_fid", 0).put("geometry", "testGeo").put("srid", 0));

        try (MockedConstruction<RemoteRDBStoreClient> remoteRDBStoreClientMock = mockConstruction(RemoteRDBStoreClient.class,
                (mock, context) -> {
                    doReturn(testArray).when(mock).executeQuery(anyString());
                })) {
            Map<Integer, OSMObject> result = OSMObject.getOSMObjects("", "", "", "", "");

            assertEquals(1, result.size());
            assertTrue(result.containsKey(0));
            assertEquals(0, result.get(0).getOgcfid());
            assertEquals("testGeo", result.get(0).getGeometry());
            assertEquals(0, result.get(0).getSrid());

            verify(remoteRDBStoreClientMock.constructed().get(0), times(1)).executeQuery(anyString());
        }
    }
}
