package uk.ac.cam.cares.jps.agent.cea.utils.endpoint;

import static org.mockito.Mockito.*;
import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;

import uk.ac.cam.cares.jps.base.config.JPSConstants;
import uk.ac.cam.cares.jps.base.query.AccessAgentCaller;

import org.json.JSONArray;
import org.json.JSONObject;
import java.util.List;

public class RouteHelperTest {
    @Test
    public void testRouteHelper() {
        String testQueryEndpoint = "testQueryEndpoint";
        String testUpdateEndpoint = "testUpdateEndpoint";
        JSONObject testJSON = new JSONObject().put(JPSConstants.QUERY_ENDPOINT, testQueryEndpoint).put(JPSConstants.UPDATE_ENDPOINT, testUpdateEndpoint);

        try (MockedStatic<AccessAgentCaller> accessAgentCallerMock = mockStatic(AccessAgentCaller.class)) {
            accessAgentCallerMock.when(() -> AccessAgentCaller.getEndpoints(anyString()))
                    .thenReturn(testJSON);

            List<String> result = RouteHelper.getRouteEndpoints("");

            assertTrue(result.get(0).equals(testQueryEndpoint));
            assertTrue(result.get(1).equals(testUpdateEndpoint));

            accessAgentCallerMock.verify(
                    times(1), () -> AccessAgentCaller.getEndpoints(anyString())
            );
        }
    }

    @Test
    public void testCheckQuadsEnabled() {
        try (MockedStatic<AccessAgentCaller> accessAgentCallerMock = mockStatic(AccessAgentCaller.class)) {
            accessAgentCallerMock.when(() -> AccessAgentCaller.queryStore(anyString(), anyString()))
                    .thenReturn(new JSONArray());

            RouteHelper.checkQuadsEnabled("");

            accessAgentCallerMock.verify(
                    times(1), () -> AccessAgentCaller.queryStore(anyString(), anyString())
            );
        }
    }

    @Test
    public void checkEndpoint() {
        try (MockedStatic<AccessAgentCaller> accessAgentCallerMock = mockStatic(AccessAgentCaller.class)) {
            accessAgentCallerMock.when(() -> AccessAgentCaller.queryStore(anyString(), anyString()))
                    .thenReturn(new JSONArray());

            RouteHelper.checkEndpoint("");

            accessAgentCallerMock.verify(
                    times(1), () -> AccessAgentCaller.queryStore(anyString(), anyString())
            );
        }
    }
}
