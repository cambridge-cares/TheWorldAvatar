package uk.ac.cam.cares.jps.agent.cea.utils.datahandler;

import static org.mockito.Mockito.*;
import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;

import uk.ac.cam.cares.jps.agent.cea.utils.FileReader;
import uk.ac.cam.cares.jps.base.query.AccessAgentCaller;

import org.json.JSONArray;
import org.json.JSONObject;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.util.ArrayList;

public class DataRetrieverTest {
    @Test
    public void testGetDataIRI() {
        String test_value = "PVRoofSupply";
        String measure = "measure";
        String test_measure = "testUri";
        String unit = "unit";
        String test_unit = "kWh";
        String route = "test_route";
        String building = "test_building";

        JSONArray expected = new JSONArray().put(new JSONObject().put(measure, test_measure).put(unit, test_unit));
        JSONArray expectedBlank = new JSONArray();

        String content = "uri.ontology.ontocitygml=test/\nuri.ontology.om=test/\nuri.ontology.ontoubemmp=test/\nuri.ontology.rdf=test/\nuri.ontology.owl=test/\n" +
                "uri.ontology.bot=test/\nuri.ontology.ontobuiltenv=test/\nuri.ontology.ontobuiltstructure=test/\nuri.ontology.ontotimeseries=test/\nuri.ontology.ontoems=test/\n" +
                "uri.ontology.geo=test/\nuri.ontology.geofunction=test/\nuri.ontology.bldg=test/\nuri.ontology.grp=test/\nuri.ontology.gml=test/\n" +
                "uri.ontology.geoliteral=test/\nuri.ontology.geofunction=test/\nuri.opengis.epsg=test/\nuri.service.geo=test/";
        InputStream mockInputStream = new ByteArrayInputStream(content.getBytes());
        try (MockedStatic<uk.ac.cam.cares.jps.agent.cea.utils.FileReader> fileReaderMock = mockStatic(uk.ac.cam.cares.jps.agent.cea.utils.FileReader.class)) {
            fileReaderMock.when(() -> FileReader.getStream(anyString())).thenReturn(mockInputStream);
            try (MockedStatic<AccessAgentCaller> accessAgentCallerMock = mockStatic(AccessAgentCaller.class)) {
                //test with mocked AccessAgentCaller when it returns data
                accessAgentCallerMock.when(() -> AccessAgentCaller.queryStore(anyString(), anyString()))
                        .thenReturn(expected);

                ArrayList<String> result = DataRetriever.getDataIRI(building, test_value, route);
                assertTrue(result.contains(test_measure));
                assertTrue(result.contains(test_unit));

                //test with mocked AccessAgentCaller when there is nothing returned
                accessAgentCallerMock.when(() -> AccessAgentCaller.queryStore(anyString(), anyString()))
                        .thenReturn((expectedBlank));

                result = DataRetriever.getDataIRI(building, test_value, route);
                assertTrue(result.isEmpty());
            }
        }
    }

    @Test
    public void testGetNumericalValue() {
        String test_measure = "testUri";
        String value = "value";
        String test_value = "35.2";
        String route = "test_route";

        JSONArray expected = new JSONArray().put(new JSONObject().put(value, test_value));
        JSONArray expectedBlank = new JSONArray();

        String content = "uri.ontology.ontocitygml=test/\nuri.ontology.om=test/\nuri.ontology.ontoubemmp=test/\nuri.ontology.rdf=test/\nuri.ontology.owl=test/\n" +
                "uri.ontology.bot=test/\nuri.ontology.ontobuiltenv=test/\nuri.ontology.ontobuiltstructure=test/\nuri.ontology.ontotimeseries=test/\nuri.ontology.ontoems=test/\n" +
                "uri.ontology.geo=test/\nuri.ontology.geofunction=test/\nuri.ontology.bldg=test/\nuri.ontology.grp=test/\nuri.ontology.gml=test/\n" +
                "uri.ontology.geoliteral=test/\nuri.ontology.geofunction=test/\nuri.opengis.epsg=test/\nuri.service.geo=test/";
        InputStream mockInputStream = new ByteArrayInputStream(content.getBytes());
        try (MockedStatic<uk.ac.cam.cares.jps.agent.cea.utils.FileReader> fileReaderMock = mockStatic(uk.ac.cam.cares.jps.agent.cea.utils.FileReader.class)) {
            fileReaderMock.when(() -> FileReader.getStream(anyString())).thenReturn(mockInputStream);
            try (MockedStatic<AccessAgentCaller> accessAgentCallerMock = mockStatic(AccessAgentCaller.class)) {
                //test with mocked AccessAgentCaller when it returns data
                accessAgentCallerMock.when(() -> AccessAgentCaller.queryStore(anyString(), anyString()))
                        .thenReturn(expected);

                String result = DataRetriever.getNumericalValue(test_measure, route);
                assertTrue(result.contains(test_value));

                //test with mocked AccessAgentCaller when there is nothing returned
                accessAgentCallerMock.when(() -> AccessAgentCaller.queryStore(anyString(), anyString()))
                        .thenReturn((expectedBlank));

                result = DataRetriever.getNumericalValue(test_measure, route);
                assertTrue(result.isEmpty());
            }
        }
    }
}
