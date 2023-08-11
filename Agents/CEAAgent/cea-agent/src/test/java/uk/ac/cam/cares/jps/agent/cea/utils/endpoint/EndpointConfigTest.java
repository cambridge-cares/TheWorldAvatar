package uk.ac.cam.cares.jps.agent.cea.utils.endpoint;

import static org.mockito.Mockito.*;
import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.Test;
import org.mockito.MockedConstruction;

import com.cmclinnovations.stack.clients.docker.ContainerClient;
import com.cmclinnovations.stack.clients.postgis.PostGISEndpointConfig;

public class EndpointConfigTest {
    @Test
    public void testEndpointConfig() {
        String url = "testUrl";
        String url1 = "testUrl1";
        String user = "testUser";
        String password = "testPassword";

        PostGISEndpointConfig postGISEndpointConfigMock = mock(PostGISEndpointConfig.class);
        doReturn(url).when(postGISEndpointConfigMock).getJdbcURL("database");
        doReturn(url1).when(postGISEndpointConfigMock).getJdbcURL("database1");
        doReturn(user).when(postGISEndpointConfigMock).getUsername();
        doReturn(password).when(postGISEndpointConfigMock).getPassword();

        try (MockedConstruction<ContainerClient> containerClientMock = mockConstruction(ContainerClient.class,
                (mock, context) -> {
                    doReturn(postGISEndpointConfigMock).when(mock).readEndpointConfig(anyString(), any());
                })) {
            EndpointConfig endpointConfig = new EndpointConfig();

            assertTrue(endpointConfig.getDbUrl("database").equals(url));
            assertTrue(endpointConfig.getDbUrl("database1").equals(url1));
            assertTrue(endpointConfig.getDbUser().equals(user));
            assertTrue(endpointConfig.getDbPassword().equals(password));

            verify(postGISEndpointConfigMock, times(2)).getJdbcURL(anyString());
            verify(postGISEndpointConfigMock, times(1)).getUsername();
            verify(postGISEndpointConfigMock, times(1)).getPassword();
        }
    }
}
