package uk.ac.cam.cares.jps.agent.cea.utils.datahandler;

import static org.mockito.Mockito.*;
import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;

import uk.ac.cam.cares.jps.base.query.AccessAgentCaller;
import uk.ac.cam.cares.jps.agent.cea.utils.uri.OntologyURIHelper;

import org.json.JSONArray;
import org.json.JSONObject;

import java.util.ArrayList;

public class DataRetrieverTest {
    @Test
    public void testGetDataIRI() {
        OntologyURIHelper ontologyURIHelper = new OntologyURIHelper("CEAAgentConfig");

        DataRetriever dataRetriever = new DataRetriever(ontologyURIHelper);

        String test_value = "PVRoofSupply";
        String measure = "measure";
        String test_measure = "testUri";
        String unit = "unit";
        String test_unit = "kWh";
        String route = "test_route";
        String building = "test_building";

        JSONArray expected = new JSONArray().put(new JSONObject().put(measure, test_measure).put(unit, test_unit));
        JSONArray expectedBlank = new JSONArray();

        try (MockedStatic<AccessAgentCaller> accessAgentCallerMock = mockStatic(AccessAgentCaller.class)) {
            //test with mocked AccessAgentCaller when it returns data
            accessAgentCallerMock.when(() -> AccessAgentCaller.queryStore(anyString(), anyString()))
                    .thenReturn(expected);

            ArrayList<String> result = dataRetriever.getDataIRI(building, test_value, route);
            assertTrue(result.contains(test_measure));
            assertTrue(result.contains(test_unit));

            //test with mocked AccessAgentCaller when there is nothing returned
            accessAgentCallerMock.when(() -> AccessAgentCaller.queryStore(anyString(), anyString()))
                    .thenReturn((expectedBlank));

            result = dataRetriever.getDataIRI(building, test_value, route);
            assertTrue(result.isEmpty());
        }
    }

    @Test
    public void testGetNumericalValue() {
        OntologyURIHelper ontologyURIHelper = new OntologyURIHelper("CEAAgentConfig");

        DataRetriever dataRetriever = new DataRetriever(ontologyURIHelper);

        String test_measure = "testUri";
        String value = "value";
        String test_value = "35.2";
        String route = "test_route";

        JSONArray expected = new JSONArray().put(new JSONObject().put(value, test_value));
        JSONArray expectedBlank = new JSONArray();

        try (MockedStatic<AccessAgentCaller> accessAgentCallerMock = mockStatic(AccessAgentCaller.class)) {
            //test with mocked AccessAgentCaller when it returns data
            accessAgentCallerMock.when(() -> AccessAgentCaller.queryStore(anyString(), anyString()))
                    .thenReturn(expected);

            String result = dataRetriever.getNumericalValue(test_measure, route, "");
            assertTrue(result.contains(test_value));

            //test with mocked AccessAgentCaller when there is nothing returned
            accessAgentCallerMock.when(() -> AccessAgentCaller.queryStore(anyString(), anyString()))
                    .thenReturn((expectedBlank));

            result = dataRetriever.getNumericalValue(test_measure, route, "");
            assertTrue(result.isEmpty());
        }
    }
}
