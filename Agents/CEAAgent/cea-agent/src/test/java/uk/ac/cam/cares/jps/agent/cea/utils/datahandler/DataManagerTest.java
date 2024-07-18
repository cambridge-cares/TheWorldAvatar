package uk.ac.cam.cares.jps.agent.cea.utils.datahandler;

import static org.mockito.Mockito.*;
import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.Test;
import org.mockito.MockedConstruction;
import org.mockito.MockedStatic;

import uk.ac.cam.cares.jps.agent.cea.data.CEAConstants;
import uk.ac.cam.cares.jps.agent.cea.utils.FileReader;
import uk.ac.cam.cares.jps.base.query.AccessAgentCaller;

import org.json.JSONArray;
import org.json.JSONObject;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;
import java.util.LinkedHashMap;

public class DataManagerTest {
    @Test
    public void testCheckBuildingInitialised() {
        String uriString = "http://127.0.0.1:9999/blazegraph/namespace/kings-lynn-open-data/sparql/cityobject/UUID_test/";
        String route = "test_route";

        JSONArray expected = new JSONArray().put(new JSONObject().put("ASK", true));

        String content = "uri.ontology.ontocitygml=test/\nuri.ontology.om=test/\nuri.ontology.ontoubemmp=test/\nuri.ontology.rdf=test/\nuri.ontology.owl=test/\n" +
                "uri.ontology.bot=test/\nuri.ontology.ontobuiltenv=test/\nuri.ontology.ontobuiltstructure=test/\nuri.ontology.ontotimeseries=test/\nuri.ontology.ontoems=test/\n" +
                "uri.ontology.geo=test/\nuri.ontology.geofunction=test/\nuri.ontology.bldg=test/\nuri.ontology.grp=test/\nuri.ontology.gml=test/\n" +
                "uri.ontology.geoliteral=test/\nuri.ontology.geofunction=test/\nuri.opengis.epsg=test/\nuri.service.geo=test/";
        InputStream mockInputStream = new ByteArrayInputStream(content.getBytes());
        try (MockedStatic<uk.ac.cam.cares.jps.agent.cea.utils.FileReader> fileReaderMock = mockStatic(uk.ac.cam.cares.jps.agent.cea.utils.FileReader.class)) {
            fileReaderMock.when(() -> FileReader.getStream(anyString())).thenReturn(mockInputStream);
            try (MockedStatic<AccessAgentCaller> accessAgentCallerMock = mockStatic(AccessAgentCaller.class)) {
                accessAgentCallerMock.when(() -> AccessAgentCaller.queryStore(anyString(), anyString()))
                        .thenReturn(expected);

                assertTrue(DataManager.checkBuildingInitialised(uriString, route));
            }
        }
    }

    @Test
    public void testInitialiseBuilding() {
        String route = "test_route";

        String uriString = "http://127.0.0.1:9999/blazegraph/namespace/kings-lynn-open-data/sparql/cityobject/UUID_test/";

        String content = "uri.ontology.ontocitygml=test/\nuri.ontology.om=test/\nuri.ontology.ontoubemmp=test/\nuri.ontology.rdf=test/\nuri.ontology.owl=test/\n" +
                "uri.ontology.bot=test/\nuri.ontology.ontobuiltenv=test/\nuri.ontology.ontobuiltstructure=test/\nuri.ontology.ontotimeseries=test/\nuri.ontology.ontoems=test/\n" +
                "uri.ontology.geo=test/\nuri.ontology.geofunction=test/\nuri.ontology.bldg=test/\nuri.ontology.grp=test/\nuri.ontology.gml=test/\n" +
                "uri.ontology.geoliteral=test/\nuri.ontology.geofunction=test/\nuri.opengis.epsg=test/\nuri.service.geo=test/";
        InputStream mockInputStream = new ByteArrayInputStream(content.getBytes());
        try (MockedStatic<uk.ac.cam.cares.jps.agent.cea.utils.FileReader> fileReaderMock = mockStatic(uk.ac.cam.cares.jps.agent.cea.utils.FileReader.class)) {
            fileReaderMock.when(() -> FileReader.getStream(anyString())).thenReturn(mockInputStream);
            try (MockedStatic<AccessAgentCaller> accessAgentCallerMock = mockStatic(AccessAgentCaller.class)) {
                DataManager.initialiseBuilding(uriString, route);

                // test update store is called once
                accessAgentCallerMock.verify(
                        times(1), () -> AccessAgentCaller.updateStore(anyString(), anyString())
                );
            }
        }
    }

    @Test
    public void testCheckDataInitialised() {
        //Test time series data
        String testUnit = "testUnit";
        String testIri = "testIri";
        String testBuilding = "testBuilding";

        ArrayList<String> testList = new ArrayList<>();
        testList.add(testIri);
        testList.add(testUnit);

        LinkedHashMap<String, String> tsIris = new LinkedHashMap();
        LinkedHashMap<String, String> scalarIris = new LinkedHashMap();

        String content = "uri.ontology.ontocitygml=test/\nuri.ontology.om=test/\nuri.ontology.ontoubemmp=test/\nuri.ontology.rdf=test/\nuri.ontology.owl=test/\n" +
                "uri.ontology.bot=test/\nuri.ontology.ontobuiltenv=test/\nuri.ontology.ontobuiltstructure=test/\nuri.ontology.ontotimeseries=test/\nuri.ontology.ontoems=test/\n" +
                "uri.ontology.geo=test/\nuri.ontology.geofunction=test/\nuri.ontology.bldg=test/\nuri.ontology.grp=test/\nuri.ontology.gml=test/\n" +
                "uri.ontology.geoliteral=test/\nuri.ontology.geofunction=test/\nuri.opengis.epsg=test/\nuri.service.geo=test/";
        InputStream mockInputStream = new ByteArrayInputStream(content.getBytes());
        try (MockedStatic<uk.ac.cam.cares.jps.agent.cea.utils.FileReader> fileReaderMock = mockStatic(uk.ac.cam.cares.jps.agent.cea.utils.FileReader.class)) {
            fileReaderMock.when(() -> FileReader.getStream(anyString())).thenReturn(mockInputStream);
            try (MockedStatic<DataRetriever> dataRetrieverMock = mockStatic(DataRetriever.class)) {
                dataRetrieverMock.when(() -> DataRetriever.getDataIRI(anyString(), anyString(), anyString())).thenReturn(testList);
                Boolean result = DataManager.checkDataInitialised(testBuilding, tsIris, scalarIris, "");
                assertTrue(result);

                for (String scalar : CEAConstants.SCALARS) {
                    assertTrue(scalarIris.get(scalar).contains(testIri));
                }

                for (String ts : CEAConstants.TIME_SERIES) {
                    assertTrue(tsIris.get(ts).contains(testIri));
                }
            }
        }
    }

    @Test
    public void testInitialiseData() {
        LinkedHashMap<String,String> testTsIris = mock(LinkedHashMap.class);
        doReturn("test").when(testTsIris).get(anyString());

        LinkedHashMap<String,String> testScalarIris = mock(LinkedHashMap.class);
        doReturn("test").when(testScalarIris).get(anyString());

        LinkedHashMap<String,List<Double>> testScalars = mock(LinkedHashMap.class);
        List<Double> test_scalars = new ArrayList<>();
        test_scalars.add(0.0);
        doReturn(test_scalars).when(testScalars).get(anyString());

        String route = "test_route";

        Integer testCounter = 0;
        String building = "http://127.0.0.1:9999/blazegraph/namespace/kings-lynn-open-data/sparql/energyprofile/Building_UUID_test/";

        String content = "uri.ontology.ontocitygml=test/\nuri.ontology.om=test/\nuri.ontology.ontoubemmp=test/\nuri.ontology.rdf=test/\nuri.ontology.owl=test/\n" +
                "uri.ontology.bot=test/\nuri.ontology.ontobuiltenv=test/\nuri.ontology.ontobuiltstructure=test/\nuri.ontology.ontotimeseries=test/\nuri.ontology.ontoems=test/\n" +
                "uri.ontology.geo=test/\nuri.ontology.geofunction=test/\nuri.ontology.bldg=test/\nuri.ontology.grp=test/\nuri.ontology.gml=test/\n" +
                "uri.ontology.geoliteral=test/\nuri.ontology.geofunction=test/\nuri.opengis.epsg=test/\nuri.service.geo=test/";
        InputStream mockInputStream = new ByteArrayInputStream(content.getBytes());
        try (MockedStatic<uk.ac.cam.cares.jps.agent.cea.utils.FileReader> fileReaderMock = mockStatic(uk.ac.cam.cares.jps.agent.cea.utils.FileReader.class)) {
            fileReaderMock.when(() -> FileReader.getStream(anyString())).thenReturn(mockInputStream);
            try (MockedStatic<AccessAgentCaller> accessAgentCallerMock = mockStatic(AccessAgentCaller.class)) {
                DataManager.initialiseData(testCounter, testScalars, building, testTsIris, testScalarIris, route);

                //test update store is called once
                accessAgentCallerMock.verify(
                        times(1), () -> AccessAgentCaller.updateStore(anyString(), anyString())
                );
            }
        }
    }

    @Test
    public void testUpdateScalars() {
        LinkedHashMap<String,String> scalarIrisMock = mock(LinkedHashMap.class);
        when(scalarIrisMock.get(anyString())).thenReturn("test");

        LinkedHashMap<String,List<Double>> scalarsMock = mock(LinkedHashMap.class);
        List<Double> testScalars = new ArrayList<>();
        testScalars.add(0.0);
        when(scalarsMock.get(anyString())).thenReturn(testScalars);

        String route = "test";

        Integer testCounter = 0;
        String content = "uri.ontology.ontocitygml=test/\nuri.ontology.om=test/\nuri.ontology.ontoubemmp=test/\nuri.ontology.rdf=test/\nuri.ontology.owl=test/\n" +
                "uri.ontology.bot=test/\nuri.ontology.ontobuiltenv=test/\nuri.ontology.ontobuiltstructure=test/\nuri.ontology.ontotimeseries=test/\nuri.ontology.ontoems=test/\n" +
                "uri.ontology.geo=test/\nuri.ontology.geofunction=test/\nuri.ontology.bldg=test/\nuri.ontology.grp=test/\nuri.ontology.gml=test/\n" +
                "uri.ontology.geoliteral=test/\nuri.ontology.geofunction=test/\nuri.opengis.epsg=test/\nuri.service.geo=test/";
        InputStream mockInputStream = new ByteArrayInputStream(content.getBytes());
        try (MockedStatic<uk.ac.cam.cares.jps.agent.cea.utils.FileReader> fileReaderMock = mockStatic(uk.ac.cam.cares.jps.agent.cea.utils.FileReader.class)) {
            fileReaderMock.when(() -> FileReader.getStream(anyString())).thenReturn(mockInputStream);
            try (MockedStatic<AccessAgentCaller> accessAgentCallerMock = mockStatic(AccessAgentCaller.class)) {
                DataManager.updateScalars(route, scalarIrisMock, scalarsMock, testCounter);

                Integer expected = CEAConstants.SCALARS.size() * 2;

                accessAgentCallerMock.verify(
                        times(expected), () -> AccessAgentCaller.updateStore(anyString(), anyString())
                );
            }
        }
    }
}
