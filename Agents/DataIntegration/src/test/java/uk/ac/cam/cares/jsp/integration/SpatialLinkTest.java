package uk.ac.cam.cares.jsp.integration;

import org.apache.http.client.utils.URIBuilder;
import org.json.JSONObject;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.testcontainers.containers.Network;
import org.testcontainers.containers.PostgreSQLContainer;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.junit.jupiter.Testcontainers;
import org.junit.jupiter.api.Test;
import org.testcontainers.utility.DockerImageName;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.net.URISyntaxException;
import java.time.Instant;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

class SpatialLinkTest {
    DockerImageName postgisImage = DockerImageName.parse("postgis/postgis:13-3.2").asCompatibleSubstituteFor("postgres");
    @Container
    private PostgreSQLContainer<?> postgres = new PostgreSQLContainer<>(postgisImage);
    @BeforeEach
    public void initialise() throws URISyntaxException {
        Network network = Network.newNetwork();
        postgres.setStartupAttempts(2);
        postgres.withNetwork(network);
        postgres.withNetworkAliases("postgis");
        postgres.start();
    }
    @Test
    public void spatialLinkTest() throws ServletException, IOException {

        HttpServletRequest request = mock(HttpServletRequest.class);
        HttpServletResponse response = mock(HttpServletResponse.class);

        when(request.getParameter("db2d")).thenReturn("sg_2D");
        when(request.getParameter("db3d")).thenReturn("sg_ntu");

        SpatialLink spatialLink = new SpatialLink();
        spatialLink.doPut(request, response);

//        when(request.getParameter("username")).thenReturn("postgres");
//        when(request.getParameter("password")).thenReturn("123456");
    }

    @AfterEach
    public void cleanup() throws IOException {
        if (postgres.isRunning()) {
            postgres.stop();
        }
    }
}