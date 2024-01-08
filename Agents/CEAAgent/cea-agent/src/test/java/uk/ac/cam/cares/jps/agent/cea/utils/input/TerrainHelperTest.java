package uk.ac.cam.cares.jps.agent.cea.utils.input;

import static org.mockito.Mockito.*;
import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.Test;
import org.locationtech.jts.geom.GeometryFactory;
import org.locationtech.jts.geom.Polygon;
import org.mockito.MockedConstruction;

import org.mockito.MockedStatic;

import uk.ac.cam.cares.jps.agent.cea.data.CEABuildingData;
import uk.ac.cam.cares.jps.agent.cea.data.CEAGeometryData;
import uk.ac.cam.cares.jps.agent.cea.utils.FileReader;
import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;

import org.json.JSONArray;
import org.json.JSONObject;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.util.*;
import java.sql.SQLException;
import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.Statement;
import org.locationtech.jts.geom.Coordinate;

public class TerrainHelperTest {
    @Test
    public void testGetTerrain() throws SQLException {
        TerrainHelper terrainHelper = new TerrainHelper("", "", "");

        byte[] testBytes = {1, 2, 3, 4, 5};
        Connection mockConnection = mock(Connection.class);
        Statement mockStatement = mock(Statement.class);
        ResultSet mockResultSet = mock(ResultSet.class);

        doReturn(mockStatement).when(mockConnection).createStatement();
        doReturn(mockResultSet).when(mockStatement).executeQuery(anyString());
        when(mockResultSet.next()).thenReturn(true).thenReturn(false);
        doReturn(testBytes).when(mockResultSet).getBytes(anyString());

        String testCRS = "4326";

        Coordinate[] coordinates = new Coordinate[] {
                new Coordinate(0, 0),
                new Coordinate(0, 5),
                new Coordinate(5, 5),
                new Coordinate(5, 0),
                new Coordinate(0, 0)
        };
        Coordinate[] coordinates1 = new Coordinate[] {
                new Coordinate(0.1, 0.1),
                new Coordinate(0.1, 5.1),
                new Coordinate(5.1, 5.1),
                new Coordinate(5.1, 0.1),
                new Coordinate(0.1, 0.1)
        };

        GeometryFactory geometryFactory = new GeometryFactory();

        Polygon polygon = geometryFactory.createPolygon(coordinates);
        Polygon polygon1 = geometryFactory.createPolygon(coordinates1);

        CEAGeometryData testGeometry = new CEAGeometryData(Arrays.asList(polygon), testCRS, "10.0");
        CEAGeometryData testGeometry1 = new CEAGeometryData(Arrays.asList(polygon1), testCRS, "10.0");

        Map<String, Double> testUsage = new HashMap<>();
        testUsage.put("MULTI_RES", 1.0);

        CEABuildingData testBuilding = new CEABuildingData(testGeometry, testUsage);

        ArrayList<CEABuildingData> testBuildings = new ArrayList<>();
        testBuildings.add(testBuilding);

        List<CEAGeometryData> testSurroundings = Arrays.asList(testGeometry, testGeometry1);

        String content = "uri.ontology.ontocitygml=test/\nuri.ontology.om=test/\nuri.ontology.ontoubemmp=test/\nuri.ontology.rdf=test/\nuri.ontology.owl=test/\n" +
                "uri.ontology.bot=test/\nuri.ontology.ontobuiltenv=test/\nuri.ontology.ontobuiltstructure=test/\nuri.ontology.ontotimeseries=test/\nuri.ontology.ontoems=test/\n" +
                "uri.ontology.geo=test/\nuri.ontology.geofunction=test/\nuri.ontology.bldg=test/\nuri.ontology.grp=test/\nuri.ontology.gml=test/\n" +
                "uri.ontology.geoliteral=test/\nuri.ontology.geofunction=test/\nuri.opengis.epsg=test/\nuri.service.geo=test/";
        InputStream mockInputStream = new ByteArrayInputStream(content.getBytes());
        try (MockedStatic<FileReader> fileReaderMock = mockStatic(uk.ac.cam.cares.jps.agent.cea.utils.FileReader.class)) {
            fileReaderMock.when(() -> FileReader.getStream(anyString())).thenReturn(mockInputStream);
            try (MockedConstruction<RemoteRDBStoreClient> remoteRDBStoreClientMock = mockConstruction(RemoteRDBStoreClient.class,
                    (mock, context) -> {
                        doReturn(new JSONArray().put(new JSONObject().put("srid", 4326))).when(mock).executeQuery(anyString());
                        doReturn(mockConnection).when(mock).getConnection();
                    })) {

                byte[] result = terrainHelper.getTerrain(testBuildings, testSurroundings, "");

                assertEquals(testBytes.length, result.length);

                for (int i = 0; i < testBytes.length; i++) {
                    assertEquals(testBytes[i], result[i]);
                }
            }
        }
    }
}
