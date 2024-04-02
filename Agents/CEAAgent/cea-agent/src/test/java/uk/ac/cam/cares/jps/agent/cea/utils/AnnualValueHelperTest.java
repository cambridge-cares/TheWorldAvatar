package uk.ac.cam.cares.jps.agent.cea.utils;

import static org.mockito.Mockito.*;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.MockedStatic;

import org.mockito.invocation.InvocationOnMock;
import org.mockito.stubbing.Answer;

import uk.ac.cam.cares.jps.agent.cea.data.CEAConstants;
import uk.ac.cam.cares.jps.agent.cea.utils.uri.OntologyURIHelper;
import uk.ac.cam.cares.jps.base.query.AccessAgentCaller;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import org.json.JSONArray;
import org.json.JSONObject;

public class AnnualValueHelperTest {
    @Test
    public void testInstantiateAnnual() {
        JSONArray typeArray = new JSONArray();
        typeArray.put(new JSONObject().put("type", "test/test"));
        JSONArray sArray = new JSONArray();
        sArray.put(new JSONObject().put("s", "test"));

        List<List<?>> testValues = new ArrayList<>();
        LinkedHashMap<String, String> testIris = new LinkedHashMap<>();

        List<Double> v = new ArrayList<>();
        v.add(1.0);

        for (String ts : CEAConstants.TIME_SERIES) {
            testValues.add(v);
            testIris.put(ts, "test");
        }

        // test when there is no previously existing annual values

        String content = "uri.ontology.ontocitygml=test/\nuri.ontology.om=test/\nuri.ontology.ontoubemmp=test/\nuri.ontology.rdf=test/\nuri.ontology.owl=test/\n" +
                "uri.ontology.bot=test/\nuri.ontology.ontobuiltenv=test/\nuri.ontology.ontobuiltstructure=test/\nuri.ontology.ontotimeseries=test/\nuri.ontology.ontoems=test/\n" +
                "uri.ontology.geo=test/\nuri.ontology.geofunction=test/\nuri.ontology.bldg=test/\nuri.ontology.grp=test/\nuri.ontology.gml=test/\n" +
                "uri.ontology.geoliteral=test/\nuri.ontology.geofunction=test/\nuri.opengis.epsg=test/\nuri.service.geo=test/";
        InputStream mockInputStream = new ByteArrayInputStream(content.getBytes());
        try (MockedStatic<FileReader> fileReaderMock = mockStatic(FileReader.class)) {
            fileReaderMock.when(() -> FileReader.getStream(anyString())).thenReturn(mockInputStream);
            try (MockedStatic<AccessAgentCaller> accessAgentCallerMock = mockStatic(AccessAgentCaller.class)) {
                accessAgentCallerMock.when(() -> AccessAgentCaller.queryStore(anyString(), anyString()))
                        .thenAnswer(new Answer() {
                            private int count = 0;

                            public JSONArray answer(InvocationOnMock invocation) {
                                count++;

                                if (count % 3 == 1) {
                                    return typeArray;
                                }
                                else if (count % 3 == 2){
                                    return sArray;
                                }
                                else {
                                    return new JSONArray();
                                }

                            }
                        });

                AnnualValueHelper.instantiateAnnual(testValues, testIris, "");
                accessAgentCallerMock.verify(
                        times(1), () -> AccessAgentCaller.updateStore(anyString(), anyString()));
            }
        }

        // test when there are previously existing annual value
        try (MockedStatic<FileReader> fileReaderMock = mockStatic(FileReader.class)) {
            fileReaderMock.when(() -> FileReader.getStream(anyString())).thenReturn(mockInputStream);
            try (MockedStatic<AccessAgentCaller> accessAgentCallerMock = mockStatic(AccessAgentCaller.class)) {
                accessAgentCallerMock.when(() -> AccessAgentCaller.queryStore(anyString(), anyString()))
                        .thenAnswer(new Answer() {
                            private int count = 0;

                            public JSONArray answer(InvocationOnMock invocation) {
                                count++;

                                if (count % 3 == 1) {
                                    return typeArray;
                                } else if (count % 3 == 2) {
                                    return sArray;
                                } else {
                                    return new JSONArray().put(new JSONObject().put("measure", "test"));
                                }

                            }
                        });

                AnnualValueHelper.instantiateAnnual(testValues, testIris, "");

                accessAgentCallerMock.verify(
                        times(CEAConstants.TIME_SERIES.size() * 3), () -> AccessAgentCaller.queryStore(anyString(), anyString()));

                ArgumentCaptor<String> argumentCaptor = ArgumentCaptor.forClass(String.class);
                accessAgentCallerMock.verify(
                        times(5), () -> AccessAgentCaller.updateStore(anyString(), argumentCaptor.capture()));

                List<String> allCaptures = argumentCaptor.getAllValues();

                assertTrue(allCaptures.get(0).contains("DELETE"));
                assertTrue(allCaptures.get(4).contains("INSERT"));
            }
        }
    }
}
