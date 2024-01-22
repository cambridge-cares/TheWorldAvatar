package uk.ac.cam.cares.jps.agent.osmagent.geometry.object;

import static org.mockito.Mockito.*;
import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.Test;
import org.mockito.MockedConstruction;

import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;

import java.util.Map;
import org.json.JSONArray;
import org.json.JSONObject;

public class GeoObjectTest {
    @Test
    public void testGetGeoObjects() {
        JSONArray testArray = new JSONArray();
        testArray.put(new JSONObject().put("urival", "testIRI").put("geostring", "testGeo").put("srid", 0));

        try (MockedConstruction<RemoteRDBStoreClient> remoteRDBStoreClientMock = mockConstruction(RemoteRDBStoreClient.class,
                (mock, context) -> {
                    doReturn(testArray).when(mock).executeQuery(anyString());
                })) {
            Map<String, GeoObject> result = GeoObject.getGeoObjects("", "", "");

            assertEquals(1, result.size());
            assertTrue(result.containsKey("testIRI"));
            assertEquals("testIRI", result.get("testIRI").getUrival());
            assertEquals("testGeo", result.get("testIRI").getGeometry());
            assertEquals(0, result.get("testIRI").getSrid());

            verify(remoteRDBStoreClientMock.constructed().get(0), times(1)).executeQuery(anyString());
        }
    }
}
