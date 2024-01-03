package uk.ac.cam.cares.jps.agent.osmagent.geometry;

import static org.mockito.Mockito.*;

import org.junit.jupiter.api.Test;
import org.mockito.MockedConstruction;
import org.mockito.MockedStatic;

import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;
import uk.ac.cam.cares.jps.agent.osmagent.geometry.object.GeoObject;
import uk.ac.cam.cares.jps.agent.osmagent.geometry.object.OSMObject;

import org.locationtech.jts.io.ParseException;
import org.json.JSONArray;
import org.json.JSONObject;
import java.util.HashMap;
import java.util.Map;

public class GeometryMatcherTest {
    @Test
    public void testMatchGeometry() throws ParseException {
        Map<Integer, OSMObject> testOSMObjectMap = new HashMap<>();
        Map<String, GeoObject> testGeoObjectmap = new HashMap<>();

        OSMObject testOSMObject = new OSMObject();
        GeoObject testGeoObject = new GeoObject();
        String testPoint = "POINT (0 0)";
        String testIri = "testIri";

        testOSMObject.setOgcfid(0);
        testOSMObject.setGeometry(testPoint);
        testOSMObject.setSrid(0);

        testGeoObject.setUrival(testIri);
        testGeoObject.setGeometry(testPoint);
        testGeoObject.setSrid(0);

        testOSMObjectMap.put(0, testOSMObject);
        testGeoObjectmap.put(testPoint, testGeoObject);

        JSONArray testArray = new JSONArray().put(new JSONObject().put("building_iri", testPoint).put("geostring", "POINT"));

        try (MockedStatic<OSMObject> osmObjectMock = mockStatic(OSMObject.class)) {
            osmObjectMock.when(() -> OSMObject.getOSMObjects(anyString(), anyString(), anyString(), anyString(), anyString()))
                    .thenReturn(testOSMObjectMap);
            try (MockedStatic<GeoObject> geoObjectMock = mockStatic(GeoObject.class)) {
                geoObjectMock.when(() -> GeoObject.getGeoObjects(anyString(), anyString(), anyString()))
                        .thenReturn(testGeoObjectmap);
                try (MockedConstruction<RemoteRDBStoreClient> remoteRDBStoreClientMock = mockConstruction(RemoteRDBStoreClient.class,
                (mock, context) -> {
                    doReturn(testArray).when(mock).executeQuery(anyString());
                })) {
                    GeometryMatcher geometryMatcher = new GeometryMatcher("", "" ,"");
                    geometryMatcher.matchGeometry("points");
                    osmObjectMock.verify(
                            times(1), () -> OSMObject.getOSMObjects(anyString(), anyString(), anyString(), anyString(), anyString())
                    );
                    geoObjectMock.verify(
                            times(1), () -> GeoObject.getGeoObjects(anyString(), anyString(), anyString())
                    );
                    verify(remoteRDBStoreClientMock.constructed().get(0), times(2)).executeQuery(anyString());
                    verify(remoteRDBStoreClientMock.constructed().get(0), times(1)).executeUpdate(anyString());
                }
            }
        }
    }
}
