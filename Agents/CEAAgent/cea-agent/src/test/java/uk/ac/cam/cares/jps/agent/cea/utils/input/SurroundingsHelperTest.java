package uk.ac.cam.cares.jps.agent.cea.utils.input;

import static org.mockito.Mockito.*;
import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;

import uk.ac.cam.cares.jps.agent.cea.utils.uri.OntologyURIHelper;
import uk.ac.cam.cares.jps.base.query.AccessAgentCaller;
import uk.ac.cam.cares.jps.agent.cea.data.CEAInputData;

import org.json.JSONArray;
import org.json.JSONObject;
import java.util.List;
import java.util.ArrayList;

public class SurroundingsHelperTest {
    @Test
    public void testGetSurroundings() {
        OntologyURIHelper ontologyURIHelper = new OntologyURIHelper("CEAAgentConfig");

        SurroundingsHelper surroundingsHelper = new SurroundingsHelper(ontologyURIHelper);

        String uri = "http://localhost/kings-lynn-open-data/cityobject/UUID_583747b0-1655-4761-8050-4036436a1052/";

        String geometry = "555438.08#305587.27999#-0.6#555484.04#305587.27999#-0.6#555484.04#305614.87999#-0.6#555438.08#305614.87999#-0.6#555438.08#305587.27999#-0.6";
        List<String> unique = new ArrayList<>();
        unique.add(uri);

        JSONArray geometryArray = new JSONArray().put(new JSONObject().put("envelope", geometry).put("geometry", geometry).put("datatype", "http://localhost/blazegraph/literals/POLYGON-3-15"));
        JSONArray buildingsArray = new JSONArray().put(new JSONObject().put("cityObject", uri)).put(new JSONObject().put("cityObject", "http://localhost/kings-lynn-open-data/cityobject/UUID_447787a5-1678-4246-8658-4036436c1052/"));
        JSONArray heightArray = new JSONArray().put(new JSONObject().put("HeightMeasuredHeigh", 10.0));

        List testSurroundingCoordinate = new ArrayList<>();

        try (MockedStatic<AccessAgentCaller> accessAgentCallerMock = mockStatic(AccessAgentCaller.class)) {
            accessAgentCallerMock.when(() -> AccessAgentCaller.queryStore(anyString(), anyString()))
                    .thenReturn(geometryArray).thenReturn(buildingsArray).thenReturn(heightArray).thenReturn(geometryArray);

            ArrayList<CEAInputData> result = surroundingsHelper.getSurroundings(uri, "testRoute", unique, testSurroundingCoordinate);

            assertFalse(result.isEmpty());
            assertTrue(result.get(0).getHeight().equals("10.0"));
            assertNull(result.get(0).getUsage());
            assertNull(result.get(0).getSurrounding());
            assertFalse(testSurroundingCoordinate.isEmpty());
        }
    }
}
