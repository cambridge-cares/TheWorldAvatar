package uk.ac.cam.cares.jsp.integration;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.testcontainers.containers.Network;
import org.testcontainers.containers.PostgreSQLContainer;
import org.testcontainers.junit.jupiter.Container;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

class BuildingLinkTest {
    // Create Docker container with postgres 13.3 image from Docker Hub
    @Container
    private PostgreSQLContainer<?> postgres = new PostgreSQLContainer<>("postgres:13.3");
    RemoteStoreClient kgClient;
    private String queryEndpoint = "http://157.245.193.188:48083/blazegraph/namespace/ntuenergy/sparql";
    private String dburl3D = "jdbc:postgresql://localhost:5432/sg_ntu";
    private String dbuser = "postgres";
    private String dbpassword = "123456";
    PostgresClient conn3;
    @BeforeEach
    public void initialise() {
        Network network = Network.newNetwork();

        postgres.setStartupAttempts(2);
        postgres.withNetwork(network);
        postgres.withNetworkAliases("postgis");
        postgres.start();

        kgClient = new RemoteStoreClient(queryEndpoint, queryEndpoint);

        conn3 = new PostgresClient(dburl3D, dbuser, dbpassword);
    }

    @Test
    public void buildingLinkTest() throws ServletException, IOException {

        HttpServletRequest request = mock(HttpServletRequest.class);
        HttpServletResponse response = mock(HttpServletResponse.class);

        when(request.getParameter("db3d")).thenReturn("sg_ntu");
        when(request.getParameter("iri")).thenReturn("http://157.245.193.188:48083/blazegraph/namespace/ntuenergy/sparql");

        BuildingLink buildingLink = new BuildingLink();

        buildingLink.doPut(request, response);

    }
    @AfterEach
    public void cleanup() {
        if (postgres.isRunning()) {
            postgres.stop();
        }
    }

}