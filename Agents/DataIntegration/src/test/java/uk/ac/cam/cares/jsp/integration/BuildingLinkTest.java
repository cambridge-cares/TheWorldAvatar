package uk.ac.cam.cares.jsp.integration;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.testcontainers.containers.Network;
import org.testcontainers.containers.PostgreSQLContainer;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.utility.DockerImageName;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.net.URISyntaxException;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

class BuildingLinkTest {
    DockerImageName postgisImage = DockerImageName.parse("postgis/postgis:13-3.2").asCompatibleSubstituteFor("postgres");
    @Container
    private PostgreSQLContainer<?> postgres = new PostgreSQLContainer<>(postgisImage);
    RemoteStoreClient kgClient;
    @BeforeEach
    public void initialise() throws URISyntaxException {
        Network network = Network.newNetwork();

        postgres.setStartupAttempts(2);
        postgres.withNetwork(network);
        postgres.withNetworkAliases("postgis");
        postgres.start();
    }

    @Test
    public void buildingLinkTest() throws ServletException, IOException {

        HttpServletRequest request = mock(HttpServletRequest.class);
        HttpServletResponse response = mock(HttpServletResponse.class);

        when(request.getParameter("db2d")).thenReturn("sg_2D");
        when(request.getParameter("db3d")).thenReturn("sg_ntu");

        SpatialLink spatialLink = new SpatialLink();
        spatialLink.doPut(request, response);

//        when(request.getParameter("username")).thenReturn("postgres");
//        when(request.getParameter("password")).thenReturn("123456");
    }


}