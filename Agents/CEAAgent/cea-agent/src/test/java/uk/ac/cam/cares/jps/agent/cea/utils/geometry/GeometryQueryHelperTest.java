package uk.ac.cam.cares.jps.agent.cea.utils.geometry;

import static org.mockito.Mockito.*;
import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.Test;
import org.mockito.MockedConstruction;
import org.mockito.MockedStatic;

import uk.ac.cam.cares.jps.agent.cea.data.CEAGeometryData;
import uk.ac.cam.cares.jps.agent.cea.utils.uri.OntologyURIHelper;

import org.json.JSONArray;
import org.json.JSONObject;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

import java.util.ArrayList;

public class GeometryQueryHelperTest {
    @Test
    public void testGetBuildingGeometry() {
        OntologyURIHelper uriHelperMock = mock(OntologyURIHelper.class);

        GeometryQueryHelper geometryQueryHelper = spy(new GeometryQueryHelper(uriHelperMock));

        doReturn("10.0").when(geometryQueryHelper).getBuildingHeight(anyString(), anyString());
        doReturn(new CEAGeometryData()).when(geometryQueryHelper).getLod0Footprint(anyString(), anyString(), anyString(), anyBoolean());

        geometryQueryHelper.getBuildingGeometry("", "", true);

        verify(geometryQueryHelper, times(1)).getBuildingHeight(anyString(), anyString());
        verify(geometryQueryHelper, times(1)).getLod0Footprint(anyString(), anyString(), anyString(), anyBoolean());
    }

    @Test
    public void testGetBuildingHeight() {
        OntologyURIHelper uriHelperMock = mock(OntologyURIHelper.class);

        doReturn("test/").when(uriHelperMock).getOntologyUri(anyString());

        JSONArray heightArray = new JSONArray().put(new JSONObject().put("height", "11.0"));

        try (MockedConstruction<RemoteStoreClient> remoteStoreClientMock = mockConstruction(RemoteStoreClient.class,
                (mock, context) -> {
                    when(mock.executeQuery(anyString())).thenReturn(heightArray);
                })) {

            GeometryQueryHelper geometryQueryHelper = new GeometryQueryHelper(uriHelperMock);

            String result = geometryQueryHelper.getBuildingHeight("", "");

            assertTrue(result.equals("11.0"));
        }
    }

    @Test
    public void testGetLod0Footprint() {
        OntologyURIHelper uriHelperMock = mock(OntologyURIHelper.class);

        doReturn("http://www.opengis.net/def/crs/EPSG/0/").when(uriHelperMock).getOntologyUri(anyString());

        String geometry = "POLYGON((0 0, 0 4, 4 4, 4 0, 0 0))";
        String testCRS = "4326";
        String testEPSG = "<http://www.opengis.net/def/crs/EPSG/0/" + testCRS + ">";

        JSONArray testArray = new JSONArray().put(new JSONObject().put("wkt", testEPSG + " " + geometry).put("crs", testEPSG));

        try (MockedConstruction<RemoteStoreClient> remoteStoreClientMock = mockConstruction(RemoteStoreClient.class,
                (mock, context) ->{
                    when(mock.executeQuery(anyString())).thenReturn(testArray);
                })) {
            try (MockedStatic<GeometryHandler> geometryHandlerMock = mockStatic(GeometryHandler.class)) {
                geometryHandlerMock.when(() -> GeometryHandler.extractFootprint(any(), anyString(), anyDouble()))
                        .thenReturn(new ArrayList<>());

                GeometryQueryHelper geometryQueryHelper = new GeometryQueryHelper(uriHelperMock);

                CEAGeometryData result = geometryQueryHelper.getLod0Footprint("", "", "10.0", true);

                assertNotNull(result);
                assertTrue(result.getHeight().equals("10.0"));
                assertTrue(result.getFootprint().isEmpty());
                assertTrue(result.getCrs().equals(testCRS));
                geometryHandlerMock.verify(times(1), () -> GeometryHandler.extractFootprint(any(), anyString(), anyDouble()));
            }
        }
    }
}
