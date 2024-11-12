package uk.ac.cam.cares.jps.agent.cea.utils.endpoint;

import static org.mockito.Mockito.*;
import static org.junit.jupiter.api.Assertions.*;

import com.cmclinnovations.stack.clients.ontop.OntopClient;
import com.cmclinnovations.stack.clients.ontop.OntopEndpointConfig;
import org.junit.jupiter.api.Test;
import org.mockito.MockedConstruction;

import com.cmclinnovations.stack.clients.docker.ContainerClient;
import com.cmclinnovations.stack.clients.postgis.PostGISEndpointConfig;
import org.mockito.MockedStatic;

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

        String url2 = "testUrl2";

        OntopEndpointConfig ontopEndpointConfigMock = mock(OntopEndpointConfig.class);
        doReturn(url2).when(ontopEndpointConfigMock).getUrl();

        OntopClient clientMock = mock(OntopClient.class);
        doReturn(ontopEndpointConfigMock).when(clientMock).getEndpoint();

        try (MockedConstruction<ContainerClient> containerClientMock = mockConstruction(ContainerClient.class,
                (mock, context) -> {
                    doReturn(postGISEndpointConfigMock).when(mock).readEndpointConfig(anyString(), any());
                })) {
            try (MockedStatic<OntopClient> ontopClientMock = mockStatic(OntopClient.class)) {
                ontopClientMock.when(() -> OntopClient.getInstance())
                    .thenReturn(clientMock);

                EndpointConfig endpointConfig = new EndpointConfig();

                assertTrue(endpointConfig.getDbUrl("database").equals(url));
                assertTrue(endpointConfig.getDbUrl("database1").equals(url1));
                assertTrue(endpointConfig.getDbUser().equals(user));
                assertTrue(endpointConfig.getDbPassword().equals(password));
                assertTrue(endpointConfig.getOntopUrl().equals(url2));

                verify(postGISEndpointConfigMock, times(2)).getJdbcURL(anyString());
                verify(postGISEndpointConfigMock, times(1)).getUsername();
                verify(postGISEndpointConfigMock, times(1)).getPassword();
                verify(ontopEndpointConfigMock, times(1)).getUrl();
            }
        }
    }
}
