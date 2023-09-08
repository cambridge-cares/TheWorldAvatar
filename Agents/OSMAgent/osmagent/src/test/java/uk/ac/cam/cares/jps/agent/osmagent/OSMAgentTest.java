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

import java.io.IOException;

public class OSMAgentTest {
    @Test
    public void testProcessRequestParameters() throws ParseException, IOException {
        try (MockedConstruction<EndpointConfig> endpointConfigMock = mockConstruction(EndpointConfig.class,
                (mock, context) -> {
                    doReturn("test").when(mock).getDbUrl(anyString());
                    doReturn("test").when(mock).getDbUser();
                    doReturn("test").when(mock).getDbPassword();
                })) {
            try (MockedConstruction<UsageMatcher> usageMatcherMock = mockConstruction(UsageMatcher.class)) {
                try (MockedConstruction<GeometryMatcher> geometryMatcherMock = mockConstruction(GeometryMatcher.class)) {
                    try (MockedConstruction<UsageShareCalculator> usageShareCalculatorMock = mockConstruction(UsageShareCalculator.class)) {
                        OSMAgent agent = new OSMAgent();
                        agent.init();
                        agent.processRequestParameters(new JSONObject());

                        verify(endpointConfigMock.constructed().get(0), times(1)).getDbUrl(anyString());
                        verify(endpointConfigMock.constructed().get(0), times(1)).getDbUser();
                        verify(endpointConfigMock.constructed().get(0), times(1)).getDbPassword();
                        verify(usageMatcherMock.constructed().get(0), times(1)).checkAndAddColumns(anyString(), anyString());
                        verify(usageMatcherMock.constructed().get(0), times(1)).updateOntoBuilt(anyString(), anyString());
                        verify(usageShareCalculatorMock.constructed().get(0), times(1)).updateUsageShare(anyString());
                        verify(usageShareCalculatorMock.constructed().get(0), times(1)).updateLandUse(anyString(), anyString());
                        verify(geometryMatcherMock.constructed().get(0), times(2)).matchGeometry(anyString());
                    }
                }
            }
        }
    }
}
