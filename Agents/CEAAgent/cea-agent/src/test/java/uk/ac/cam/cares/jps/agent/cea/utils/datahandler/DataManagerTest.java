package uk.ac.cam.cares.jps.agent.cea.utils.datahandler;

import static org.mockito.Mockito.*;
import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.Test;
import org.mockito.MockedConstruction;
import org.mockito.MockedStatic;

import uk.ac.cam.cares.jps.agent.cea.data.CEAConstants;
import uk.ac.cam.cares.jps.base.query.AccessAgentCaller;
import uk.ac.cam.cares.jps.agent.cea.utils.uri.OntologyURIHelper;

import org.json.JSONArray;
import org.json.JSONObject;

import java.util.ArrayList;
import java.util.List;
import java.util.LinkedHashMap;

public class DataManagerTest {
    @Test
    public void testCheckBuildingInitialised() {
        OntologyURIHelper ontologyURIHelper = new OntologyURIHelper("CEAAgentConfig");

        DataManager dataManager = new DataManager(ontologyURIHelper);

        String uriString = "http://127.0.0.1:9999/blazegraph/namespace/kings-lynn-open-data/sparql/cityobject/UUID_test/";
        String measure = "building";
        String test_measure = "test_uri";
        String route = "test_route";

        JSONArray expected = new JSONArray().put(new JSONObject().put(measure, test_measure));
        JSONArray expectedBlank = new JSONArray();

        try (MockedStatic<AccessAgentCaller> accessAgentCallerMock = mockStatic(AccessAgentCaller.class)) {
            //test with mocked AccessAgentCaller when it returns a string.
            accessAgentCallerMock.when(() -> AccessAgentCaller.queryStore(anyString(), anyString()))
                    .thenReturn(expected);

            assertEquals(test_measure, dataManager.checkBuildingInitialised(uriString, route));

            //test with mocked AccessAgentCaller when there is no string to return.
            accessAgentCallerMock.when(() -> AccessAgentCaller.queryStore(anyString(), anyString()))
                    .thenReturn((expectedBlank));

            assertEquals("", dataManager.checkBuildingInitialised(uriString, route));
        }
    }

    @Test
    public void testInitialiseBuilding() {
        OntologyURIHelper ontologyURIHelper = new OntologyURIHelper("CEAAgentConfig");

        DataManager dataManager = new DataManager(ontologyURIHelper);

        String route = "test_route";

        String uriString = "http://127.0.0.1:9999/blazegraph/namespace/kings-lynn-open-data/sparql/cityobject/UUID_test/";
        String ontoBuiltEnvUri = ontologyURIHelper.getOntologyUri(OntologyURIHelper.ontobuiltenv);
        String expected = ontoBuiltEnvUri + "Building";

        try (MockedStatic<AccessAgentCaller> accessAgentCallerMock = mockStatic(AccessAgentCaller.class)) {
            String result = dataManager.initialiseBuilding(uriString, "", route, "");

            //test string contains correct IRI format and update store is called once
            assertTrue(result.contains(expected));
            accessAgentCallerMock.verify(
                    times(1), () -> AccessAgentCaller.updateStore(anyString(), anyString())
            );
        }
    }

    @Test
    public void testCheckDataInitialised() {
        OntologyURIHelper ontologyURIHelper = new OntologyURIHelper("CEAAgentConfig");

        DataManager dataManager = new DataManager(ontologyURIHelper);

        //Test time series data
        String testUnit = "testUnit";
        String testIri = "testIri";
        String testBuilding = "testBuilding";

        ArrayList<String> testList = new ArrayList<>();
        testList.add(testIri);
        testList.add(testUnit);

        LinkedHashMap<String, String> tsIris = new LinkedHashMap();
        LinkedHashMap<String, String> scalarIris = new LinkedHashMap();

        try(MockedConstruction<DataRetriever> dataRetrieverMock = mockConstruction(DataRetriever.class,
                (mock, context) -> {doReturn(testList).when(mock).getDataIRI(anyString(), anyString(), anyString());}
                )) {
            Boolean result = dataManager.checkDataInitialised(testBuilding, tsIris, scalarIris, "");
            assertTrue(result);

            for (String scalar : CEAConstants.SCALARS) {
                assertTrue(scalarIris.get(scalar).contains(testIri));
            }

            for (String ts : CEAConstants.TIME_SERIES) {
                assertTrue(tsIris.get(ts).contains(testIri));
            }
        }
    }

    @Test
    public void testInitialiseData() {
        OntologyURIHelper ontologyURIHelper = new OntologyURIHelper("CEAAgentConfig");

        DataManager dataManager = new DataManager(ontologyURIHelper);

        LinkedHashMap<String,String> testTsIris = mock(LinkedHashMap.class);
        doReturn("test").when(testTsIris).get(anyString());

        LinkedHashMap<String,String> testScalarIris = mock(LinkedHashMap.class);
        doReturn("test").when(testScalarIris).get(anyString());

        LinkedHashMap<String,List<String>> testScalars = mock(LinkedHashMap.class);
        List<String> test_scalars = new ArrayList<>();
        test_scalars.add("test");
        doReturn(test_scalars).when(testScalars).get(anyString());

        String route = "test_route";

        Integer testCounter = 0;
        String building = "http://127.0.0.1:9999/blazegraph/namespace/kings-lynn-open-data/sparql/energyprofile/Building_UUID_test/";

        try (MockedStatic<AccessAgentCaller> accessAgentCallerMock = mockStatic(AccessAgentCaller.class)) {
            dataManager.initialiseData(testCounter, testScalars, building, testTsIris, testScalarIris, route, "");

            //test update store is called once
            accessAgentCallerMock.verify(
                    times(1), () -> AccessAgentCaller.updateStore(anyString(), anyString())
            );
        }
    }

    @Test
    public void testUpdateScalars() {
        OntologyURIHelper ontologyURIHelper = new OntologyURIHelper("CEAAgentConfig");

        DataManager dataManager = new DataManager(ontologyURIHelper);

        LinkedHashMap<String,String> scalarIrisMock = mock(LinkedHashMap.class);
        when(scalarIrisMock.get(anyString())).thenReturn("test");

        LinkedHashMap<String,List<String>> scalarsMock = mock(LinkedHashMap.class);
        List<String> testScalars = new ArrayList<>();
        testScalars.add("test");
        when(scalarsMock.get(anyString())).thenReturn(testScalars);

        String route = "test";

        Integer testCounter = 0;
        try (MockedStatic<AccessAgentCaller> accessAgentCallerMock = mockStatic(AccessAgentCaller.class)) {
            dataManager.updateScalars(route,scalarIrisMock, scalarsMock, testCounter, "");

            Integer expected = CEAConstants.SCALARS.size() * 2;

            accessAgentCallerMock.verify(
                    times(expected), () -> AccessAgentCaller.updateStore(anyString(), anyString())
            );
        }
    }
}
