package uk.ac.cam.cares.jsp.integration;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.testcontainers.containers.Network;
import org.testcontainers.containers.PostgreSQLContainer;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.utility.DockerImageName;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

class SpatialLinkTest {

    DockerImageName postgisImage = DockerImageName.parse("postgis/postgis:13-3.2").asCompatibleSubstituteFor("postgres");
    @Container
    private PostgreSQLContainer<?> postgres = new PostgreSQLContainer<>(postgisImage);

    @BeforeEach
    public void initialise() {
        Network network = Network.newNetwork();
        postgres.setStartupAttempts(2);
        postgres.withNetwork(network);
        postgres.withNetworkAliases("postgis");
        postgres.withDatabaseName("sg_ntu");
        postgres.withUsername("postgres");
        postgres.withPassword("postgis");
        postgres.start();
        System.out.println(postgres.getJdbcUrl() + postgres.getUsername() + postgres.getPassword());
    }
    @Test
    public void spatialLinkTest() throws ServletException, IOException {

        HttpServletRequest request = mock(HttpServletRequest.class);
        HttpServletResponse response = mock(HttpServletResponse.class);

        when(request.getParameter("db2d")).thenReturn("sg_2D");
        when(request.getParameter("db3d")).thenReturn("sg_ntu");

        SpatialLink spatialLink = new SpatialLink();
        spatialLink.doPut(request, response);
        
    }

    @AfterEach
    public void cleanup() {
        if (postgres.isRunning()) {
            postgres.stop();
        }
    }
}