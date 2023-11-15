package uk.ac.cam.cares.jps.agent.cea.utils.input;

import static org.mockito.Mockito.*;
import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.Test;
import org.locationtech.jts.geom.Coordinate;
import org.locationtech.jts.geom.Geometry;
import org.locationtech.jts.geom.GeometryFactory;
import org.locationtech.jts.geom.Polygon;
import org.locationtech.jts.io.ParseException;
import org.locationtech.jts.io.WKTReader;
import org.mockito.MockedConstruction;
import org.mockito.MockedStatic;

import uk.ac.cam.cares.jps.agent.cea.data.CEAGeometryData;
import uk.ac.cam.cares.jps.agent.cea.utils.uri.OntologyURIHelper;
import uk.ac.cam.cares.jps.base.query.AccessAgentCaller;
import uk.ac.cam.cares.jps.agent.cea.data.CEABuildingData;

import org.json.JSONArray;
import org.json.JSONObject;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.ArrayList;

public class SurroundingsHelperTest {
    @Test
    public void testGetSurroundings() throws ParseException {
        OntologyURIHelper ontologyURIHelper = new OntologyURIHelper("CEAAgentConfig");

        SurroundingsHelper surroundingsHelper = new SurroundingsHelper(ontologyURIHelper);

        String uri = "http://localhost/kings-lynn-open-data/cityobject/UUID_583747b0-1655-4761-8050-4036436a1052/";

        String geometry = "POLYGON((0 0, 0 4, 4 4, 4 0, 0 0))";
        String testCRS = "4326";
        String testEPSG = "<http://www.opengis.net/def/crs/EPSG/0/" + testCRS + ">";

        JSONArray testArray = new JSONArray().put(new JSONObject().put("building", uri).put("wkt", testEPSG + " " + geometry).put("crs", testEPSG).put("height", "10.0"));


        Coordinate[] coordinates = new Coordinate[] {
                new Coordinate(0, 0),
                new Coordinate(0, 5),
                new Coordinate(5, 5),
                new Coordinate(5, 0),
                new Coordinate(0, 0)
        };

        GeometryFactory geometryFactory = new GeometryFactory();

        Polygon polygon = geometryFactory.createPolygon(coordinates);

        CEAGeometryData testGeometry = new CEAGeometryData(Arrays.asList(polygon), testCRS, "10.0");
        CEABuildingData testBuilding = new CEABuildingData(testGeometry, new HashMap<>());

        ArrayList<CEABuildingData> testBuildings = new ArrayList<>();
        testBuildings.add(testBuilding);

        WKTReader wktReader = new WKTReader();
        Geometry testPolygon = wktReader.read(geometry);

        try (MockedConstruction<RemoteStoreClient> remoteStoreClientMock = mockConstruction(RemoteStoreClient.class,
                (mock, context) ->{
                    when(mock.executeQuery(anyString())).thenReturn(testArray);
                })) {
            List<CEAGeometryData> result = surroundingsHelper.getSurroundings(testBuildings, new ArrayList<>(), "");

            assertEquals(1, result.size());
            assertTrue(result.get(0).getHeight().equals("10.0"));
            assertTrue(result.get(0).getCrs().equals(testCRS));
            assertEquals(1, result.get(0).getFootprint().size());
            assertTrue(result.get(0).getFootprint().get(0).equals(testPolygon));
        }
    }
}
