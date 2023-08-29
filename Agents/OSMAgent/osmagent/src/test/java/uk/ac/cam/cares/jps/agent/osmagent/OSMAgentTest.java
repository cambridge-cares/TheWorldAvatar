package uk.ac.cam.cares.jps.agent.osmagent;

import static org.mockito.Mockito.*;

import org.json.JSONObject;
import org.junit.jupiter.api.Test;
import org.locationtech.jts.io.ParseException;
import org.mockito.MockedConstruction;

import org.mockito.MockedStatic;
import uk.ac.cam.cares.jps.agent.osmagent.geometry.GeometryMatcher;
import uk.ac.cam.cares.jps.agent.osmagent.usage.UsageMatcher;
import uk.ac.cam.cares.jps.agent.osmagent.usage.UsageShareCalculator;

public class OSMAgentTest {
    @Test
    public void testProcessRequestParameters() throws ParseException {
        try (MockedConstruction<EndpointConfig> endpointConfigMock = mockConstruction(EndpointConfig.class,
                (mock, context) -> {
                    doReturn("test").when(mock).getDbUrl(anyString());
                    doReturn("test").when(mock).getDbUser();
                    doReturn("test").when(mock).getDbPassword();
                })) {
            try (MockedStatic<UsageMatcher> usageMatcherMock = mockStatic(UsageMatcher.class)) {
                try (MockedConstruction<GeometryMatcher> geometryMatcherMock = mockConstruction(GeometryMatcher.class)) {
                    try (MockedStatic<UsageShareCalculator> usageShareCalculatorMock = mockStatic(UsageShareCalculator.class)) {
                        OSMAgent agent = new OSMAgent();
                        agent.init();
                        agent.processRequestParameters(new JSONObject());

                        verify(endpointConfigMock.constructed().get(0), times(1)).getDbUrl(anyString());
                        verify(endpointConfigMock.constructed().get(0), times(1)).getDbUser();
                        verify(endpointConfigMock.constructed().get(0), times(1)).getDbPassword();
                        usageMatcherMock.verify(
                                times(1), () -> UsageMatcher.checkAndAddColumns(anyString(), anyString(), anyString(), anyString(), anyString())
                        );
                        usageMatcherMock.verify(
                                times(1), () -> UsageMatcher.updateOntoBuilt(anyString(), anyString(), anyString(), anyString(), anyString())
                        );
                        verify(geometryMatcherMock.constructed().get(0), times(2)).matchGeometry(anyString());
                        usageShareCalculatorMock.verify(
                                times(1), () -> UsageShareCalculator.updateUsageShare(anyString(), anyString(), anyString(), anyString(), anyString())
                        );
                        usageShareCalculatorMock.verify(
                                times(1), () -> UsageShareCalculator.updateLandUse(anyString(), anyString(), anyString(), anyString(), anyString(), anyString())
                        );
                    }
                }
            }
        }
    }
}
