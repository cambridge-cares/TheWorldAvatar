package uk.ac.cam.cares.jps.agent.cea.utils.geometry;

import static org.mockito.Mockito.*;
import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;

import uk.ac.cam.cares.jps.agent.cea.utils.uri.OntologyURIHelper;
import uk.ac.cam.cares.jps.base.query.AccessAgentCaller;

import org.json.JSONArray;
import org.json.JSONObject;

public class GeometryQueryHelperTest {
    @Test
    public void testGetBuildingGeometry() {
        OntologyURIHelper ontologyURIHelper = new OntologyURIHelper("CEAAgentConfig");

        GeometryQueryHelper geometryQueryHelper = new GeometryQueryHelper(ontologyURIHelper);

        String uriString = "http://127.0.0.1:9999/blazegraph/namespace/kings-lynn-open-data/sparql/cityobject/UUID_test/";
        String type = "height";
        String route = "test_route";
        String value = "HeightMeasuredHeigh";
        String result = "5.2";

        JSONArray expected = new JSONArray().put(new JSONObject().put(value, result));
        JSONArray expectedBlank = new JSONArray();

        try (MockedStatic<AccessAgentCaller> accessAgentCallerMock = mockStatic(AccessAgentCaller.class)) {
            // test with mocked AccessAgentCaller when there is no height information store on knowledge graph to return
            accessAgentCallerMock.when(() -> AccessAgentCaller.queryStore(anyString(), anyString()))
                    .thenReturn(expectedBlank);

            assertTrue(geometryQueryHelper.getBuildingGeometry(uriString, route, type).equals("10.0"));
            accessAgentCallerMock.verify(
                    times(3), () -> AccessAgentCaller.queryStore(anyString(), anyString())
            );

            accessAgentCallerMock.when(() -> AccessAgentCaller.queryStore(anyString(), anyString()))
                    .thenReturn((expected));
            assertTrue(geometryQueryHelper.getBuildingGeometry(uriString, route, type).equals(result));
            accessAgentCallerMock.verify(
                    times(4), () -> AccessAgentCaller.queryStore(anyString(), anyString())
            );
        }
    }
}
