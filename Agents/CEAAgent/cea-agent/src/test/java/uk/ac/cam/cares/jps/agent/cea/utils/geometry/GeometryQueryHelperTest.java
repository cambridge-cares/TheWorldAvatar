package uk.ac.cam.cares.jps.agent.cea.utils.geometry;

import static org.mockito.Mockito.*;
import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.Test;
import org.mockito.MockedConstruction;
import org.mockito.MockedStatic;

import uk.ac.cam.cares.jps.agent.cea.data.CEAGeometryData;
import uk.ac.cam.cares.jps.agent.cea.utils.FileReader;

import org.json.JSONArray;
import org.json.JSONObject;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.util.ArrayList;

public class GeometryQueryHelperTest {
    @Test
    public void testGetBuildingHeight() {
        JSONArray heightArray = new JSONArray().put(new JSONObject().put("height", "11.0"));

        String content = "uri.ontology.ontocitygml=test/\nuri.ontology.om=test/\nuri.ontology.ontoubemmp=test/\nuri.ontology.rdf=test/\nuri.ontology.owl=test/\n" +
                "uri.ontology.bot=test/\nuri.ontology.ontobuiltenv=test/\nuri.ontology.ontobuiltstructure=test/\nuri.ontology.ontotimeseries=test/\nuri.ontology.ontoems=test/\n" +
                "uri.ontology.geo=test/\nuri.ontology.geofunction=test/\nuri.ontology.bldg=test/\nuri.ontology.grp=test/\nuri.ontology.gml=test/\n" +
                "uri.ontology.geoliteral=test/\nuri.ontology.geofunction=test/\nuri.opengis.epsg=test/\nuri.service.geo=test/";
        InputStream mockInputStream = new ByteArrayInputStream(content.getBytes());
        try (MockedStatic<uk.ac.cam.cares.jps.agent.cea.utils.FileReader> fileReaderMock = mockStatic(uk.ac.cam.cares.jps.agent.cea.utils.FileReader.class)) {
            fileReaderMock.when(() -> FileReader.getStream(anyString())).thenReturn(mockInputStream);
            try (MockedConstruction<RemoteStoreClient> remoteStoreClientMock = mockConstruction(RemoteStoreClient.class,
                    (mock, context) -> {
                        when(mock.executeQuery(anyString())).thenReturn(heightArray);
                    })) {
                String result = GeometryQueryHelper.getBuildingHeight("", "");

                assertTrue(result.equals("11.0"));
            }
        }
    }

    @Test
    public void testGetLod0Footprint() {
        String geometry = "POLYGON((0 0, 0 4, 4 4, 4 0, 0 0))";
        String testCRS = "4326";

        JSONArray testArray = new JSONArray().put(new JSONObject().put("wkt", geometry).put("crs", testCRS));

        String content = "uri.ontology.ontocitygml=test/\nuri.ontology.om=test/\nuri.ontology.ontoubemmp=test/\nuri.ontology.rdf=test/\nuri.ontology.owl=test/\n" +
                "uri.ontology.bot=test/\nuri.ontology.ontobuiltenv=test/\nuri.ontology.ontobuiltstructure=test/\nuri.ontology.ontotimeseries=test/\nuri.ontology.ontoems=test/\n" +
                "uri.ontology.geo=test/\nuri.ontology.geofunction=test/\nuri.ontology.bldg=test/\nuri.ontology.grp=test/\nuri.ontology.gml=test/\n" +
                "uri.ontology.geoliteral=test/\nuri.ontology.geofunction=test/\nuri.opengis.epsg=test/\nuri.service.geo=test/";
        InputStream mockInputStream = new ByteArrayInputStream(content.getBytes());
        try (MockedStatic<uk.ac.cam.cares.jps.agent.cea.utils.FileReader> fileReaderMock = mockStatic(uk.ac.cam.cares.jps.agent.cea.utils.FileReader.class)) {
            fileReaderMock.when(() -> FileReader.getStream(anyString())).thenReturn(mockInputStream);
            try (MockedConstruction<RemoteStoreClient> remoteStoreClientMock = mockConstruction(RemoteStoreClient.class,
                    (mock, context) -> {
                        when(mock.executeQuery(anyString())).thenReturn(testArray);
                    })) {
                try (MockedStatic<GeometryHandler> geometryHandlerMock = mockStatic(GeometryHandler.class)) {
                    geometryHandlerMock.when(() -> GeometryHandler.extractFootprint(any(), anyString(), anyDouble()))
                            .thenReturn(new ArrayList<>());

                    CEAGeometryData result = GeometryQueryHelper.getLod0Footprint("", "", "10.0", true);

                    assertNotNull(result);
                    assertTrue(result.getHeight().equals("10.0"));
                    assertTrue(result.getFootprint().isEmpty());
                    assertTrue(result.getCrs().equals(testCRS));
                    geometryHandlerMock.verify(times(1), () -> GeometryHandler.extractFootprint(any(), anyString(), anyDouble()));
                }
            }
        }
    }

    @Test
    public void testCheckCRS84() {
        JSONArray crsArray = new JSONArray().put(new JSONObject().put("crs", "CRS84"));

        String content = "uri.ontology.ontocitygml=test/\nuri.ontology.om=test/\nuri.ontology.ontoubemmp=test/\nuri.ontology.rdf=test/\nuri.ontology.owl=test/\n" +
                "uri.ontology.bot=test/\nuri.ontology.ontobuiltenv=test/\nuri.ontology.ontobuiltstructure=test/\nuri.ontology.ontotimeseries=test/\nuri.ontology.ontoems=test/\n" +
                "uri.ontology.geo=test/\nuri.ontology.geofunction=test/\nuri.ontology.bldg=test/\nuri.ontology.grp=test/\nuri.ontology.gml=test/\n" +
                "uri.ontology.geoliteral=test/\nuri.ontology.geofunction=test/\nuri.opengis.epsg=test/\nuri.service.geo=test/";
        InputStream mockInputStream = new ByteArrayInputStream(content.getBytes());
        try (MockedStatic<uk.ac.cam.cares.jps.agent.cea.utils.FileReader> fileReaderMock = mockStatic(uk.ac.cam.cares.jps.agent.cea.utils.FileReader.class)) {
            fileReaderMock.when(() -> FileReader.getStream(anyString())).thenReturn(mockInputStream);
            try (MockedConstruction<RemoteStoreClient> remoteStoreClientMock = mockConstruction(RemoteStoreClient.class,
                    (mock, context) -> {
                        when(mock.executeQuery(anyString())).thenReturn(crsArray);
                    })) {
                assertTrue(GeometryQueryHelper.checkCRS84(""));
            }
        }
    }
}
