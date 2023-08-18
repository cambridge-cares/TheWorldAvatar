package uk.ac.cam.cares.jps.agent.cea.utils;

import static org.mockito.Mockito.*;

import org.junit.jupiter.api.Test;
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

        try (MockedStatic<AccessAgentCaller> accessAgentCallerMock = mockStatic(AccessAgentCaller.class)) {


            accessAgentCallerMock.when(() -> AccessAgentCaller.queryStore(anyString(), anyString()))
                    .thenAnswer(new Answer() {
                        private int count = 0;

                        public JSONArray answer(InvocationOnMock invocation) {
                            count++;

                            if (count % 2 == 1) {
                                return typeArray;
                            }
                            else {
                                return  sArray;
                            }
                        }
                    });

            annualValueHelper.instantiateAnnual(testValues, testIris, "");

            accessAgentCallerMock.verify(
                    times(CEAConstants.TIME_SERIES.size() * 2), () -> AccessAgentCaller.queryStore(anyString(), anyString()));

            accessAgentCallerMock.verify(
                    times(1), () -> AccessAgentCaller.updateStore(anyString(), anyString()));
        }
    }
}
