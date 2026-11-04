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
import java.util.Collections;
import java.util.List;

public class GeometryQueryHelperTest {
    @Test
    public void testBulkGetBuildingGeometry() {
        String building = "http://example.com/building";
        String geometry = "POLYGON((0 0, 0 4, 4 4, 4 0, 0 0))";
        JSONArray geometryArray = new JSONArray().put(new JSONObject()
                .put("building", building)
                .put("height", "11.0")
                .put("wkt", geometry)
                .put("crs", "CRS84"));

        String content = "uri.ontology.ontocitygml=test/\nuri.ontology.om=test/\nuri.ontology.ontoubemmp=test/\nuri.ontology.rdf=test/\nuri.ontology.owl=test/\n" +
                "uri.ontology.bot=test/\nuri.ontology.ontobuiltenv=test/\nuri.ontology.ontobuiltstructure=test/\nuri.ontology.ontotimeseries=test/\nuri.ontology.ontoems=test/\n" +
                "uri.ontology.geo=test/\nuri.ontology.geofunction=test/\nuri.ontology.bldg=test/\nuri.ontology.grp=test/\nuri.ontology.gml=test/\n" +
                "uri.ontology.geoliteral=test/\nuri.ontology.geofunction=test/\nuri.opengis.epsg=test/\nuri.service.geo=test/";
        InputStream mockInputStream = new ByteArrayInputStream(content.getBytes());
        try (MockedStatic<uk.ac.cam.cares.jps.agent.cea.utils.FileReader> fileReaderMock = mockStatic(uk.ac.cam.cares.jps.agent.cea.utils.FileReader.class)) {
            fileReaderMock.when(() -> FileReader.getStream(anyString())).thenReturn(mockInputStream);
            try (MockedConstruction<RemoteStoreClient> remoteStoreClientMock = mockConstruction(RemoteStoreClient.class,
                    (mock, context) -> {
                        when(mock.executeQuery(anyString())).thenReturn(geometryArray);
                    })) {
                List<CEAGeometryData> result = GeometryQueryHelper.bulkGetBuildingGeometry(
                        Collections.singletonList(building), "");

                assertEquals(1, result.size());
                assertEquals("11.0", result.get(0).getHeight());
                assertEquals("4326", result.get(0).getCrs());
                assertEquals(1, result.get(0).getFootprint().size());
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
