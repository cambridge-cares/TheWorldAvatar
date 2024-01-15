package uk.ac.cam.cares.jps.agent.osmagent;

import static org.mockito.Mockito.*;

import com.cmclinnovations.stack.clients.geoserver.GeoServerClient;
import com.cmclinnovations.stack.clients.geoserver.GeoServerVectorSettings;
import com.cmclinnovations.stack.clients.geoserver.UpdatedGSVirtualTableEncoder;
import com.cmclinnovations.stack.clients.ontop.OntopClient;
import org.junit.jupiter.api.Test;
import org.mockito.MockedConstruction;
import org.mockito.MockedStatic;

import uk.ac.cam.cares.jps.agent.osmagent.geometry.GeometryMatcher;
import uk.ac.cam.cares.jps.agent.osmagent.usage.UsageMatcher;
import uk.ac.cam.cares.jps.agent.osmagent.usage.UsageShareCalculator;

import org.json.JSONObject;
import org.locationtech.jts.io.ParseException;
import java.io.ByteArrayInputStream;
import java.io.InputStream;

public class OSMAgentTest {
    @Test
    public void testProcessRequestParameters() throws ParseException {
        String content = "db.name=test\nosm.schema=test\nlanduse.table=test\nlanduse.csv=test";

        InputStream mockInputStream = new ByteArrayInputStream(content.getBytes());

        try (MockedStatic<FileReader> fileReaderMock = mockStatic(FileReader.class)) {
            fileReaderMock.when(() -> FileReader.getStream(anyString())).thenReturn(mockInputStream);
            try (MockedConstruction<EndpointConfig> endpointConfigMock = mockConstruction(EndpointConfig.class,
                    (mock, context) -> {
                        doReturn("test").when(mock).getDbUrl(anyString());
                        doReturn("test").when(mock).getDbUser();
                        doReturn("test").when(mock).getDbPassword();
                    })) {
                try (MockedConstruction<UsageMatcher> usageMatcherMock = mockConstruction(UsageMatcher.class)) {
                    try (MockedConstruction<GeometryMatcher> geometryMatcherMock = mockConstruction(GeometryMatcher.class)) {
                        try (MockedConstruction<UsageShareCalculator> usageShareCalculatorMock = mockConstruction(UsageShareCalculator.class)) {
                            try (MockedConstruction<GeoServerClient> geoServerClientMocke = mockConstruction(GeoServerClient.class)) {
                                try (MockedConstruction<UpdatedGSVirtualTableEncoder> updatedGSVirtualTableEncoderMock = mockConstruction(UpdatedGSVirtualTableEncoder.class)) {
                                    try (MockedConstruction<GeoServerVectorSettings> GeoServerVectorSettingsMOck = mockConstruction(GeoServerVectorSettings.class)) {
                                        try (MockedConstruction<OntopClient> ontopClientMock = mockConstruction(OntopClient.class)) {
                                            OSMAgent agent = new OSMAgent();
                                            agent.init();
                                            agent.processRequestParameters(new JSONObject());

                                            verify(endpointConfigMock.constructed().get(0), times(1)).getDbUrl(anyString());
                                            verify(endpointConfigMock.constructed().get(0), times(1)).getDbUser();
                                            verify(endpointConfigMock.constructed().get(0), times(1)).getDbPassword();
                                            verify(usageMatcherMock.constructed().get(0), times(1)).checkAndAddColumns(anyString(), anyString());
                                            verify(usageMatcherMock.constructed().get(0), times(1)).updateOntoBuilt(anyString(), anyString());
                                            verify(geometryMatcherMock.constructed().get(0), times(2)).matchGeometry(anyString());
                                            verify(usageMatcherMock.constructed().get(0), times(1)).copyFromOSM(anyString(), anyString(), anyString());
                                            if (agent.landUseTable.isEmpty()) {
                                                verify(usageShareCalculatorMock.constructed().get(0), times(0)).updateLandUse(anyString(), anyString(), anyString());
                                            } else {
                                                verify(usageShareCalculatorMock.constructed().get(0), times(1)).updateLandUse(anyString(), anyString(), anyString());
                                            }
                                            verify(usageShareCalculatorMock.constructed().get(0), times(1)).updateUsageShare(anyString());
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}
