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

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import org.json.JSONArray;
import org.json.JSONObject;

public class AnnualValueHelperTest {
    @Test
    public void testInstantiateAnnual() {
        AnnualValueHelper annualValueHelper = new AnnualValueHelper(new OntologyURIHelper("CEAAgentConfig"));

        JSONArray typeArray = new JSONArray();
        typeArray.put(new JSONObject().put("type", "test"));
        JSONArray sArray = new JSONArray();
        sArray.put(new JSONObject().put("s", "test"));

        List<List<?>> testValues = new ArrayList<>();
        LinkedHashMap<String, String> testIris = new LinkedHashMap<>();

        List<Double> v = new ArrayList<>();
        v.add(1.0);

        for (String ts: CEAConstants.TIME_SERIES) {
            testValues.add(v);
            testIris.put(ts, "test");
        }

        // test when there is no previously existing annual values
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

            annualValueHelper.instantiateAnnual(testValues, testIris, "");

            accessAgentCallerMock.verify(
                    times(CEAConstants.TIME_SERIES.size() * 3), () -> AccessAgentCaller.queryStore(anyString(), anyString()));

            accessAgentCallerMock.verify(
                    times(1), () -> AccessAgentCaller.updateStore(anyString(), anyString()));
        }

        // test when there are previously existing annual values
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
                                return new JSONArray().put(new JSONObject().put("measure", "test"));
                            }

                        }
                    });

            annualValueHelper.instantiateAnnual(testValues, testIris, "");

            accessAgentCallerMock.verify(
                    times(CEAConstants.TIME_SERIES.size() * 3), () -> AccessAgentCaller.queryStore(anyString(), anyString()));

            ArgumentCaptor<String> argumentCaptor = ArgumentCaptor.forClass(String.class);
            accessAgentCallerMock.verify(
                    times(2), () -> AccessAgentCaller.updateStore(anyString(), argumentCaptor.capture()));

            List<String> allCaptures = argumentCaptor.getAllValues();

            assertTrue(allCaptures.get(0).contains("DELETE"));
            assertTrue(allCaptures.get(1).contains("INSERT"));

        }
    }
}
