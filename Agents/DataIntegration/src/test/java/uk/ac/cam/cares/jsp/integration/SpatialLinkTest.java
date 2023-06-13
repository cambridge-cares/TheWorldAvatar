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
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

class SpatialLinkTest {

    DockerImageName postgisImage = DockerImageName.parse("postgis/postgis:14-3.2-alpine").asCompatibleSubstituteFor("postgres");
    @Container
    private PostgreSQLContainer<?> postgres = new PostgreSQLContainer<>(postgisImage);
    private String dburl2D = "jdbc:postgresql://localhost:5432/ntu_2D";
    private String dburl3D = "jdbc:postgresql://localhost:5432/sg_ntu";
    private String dbuser = "postgres";
    private String dbpassword = "123456";
    PostgresClient conn2d;
    PostgresClient conn3d;

    @BeforeEach
    public void initialise() throws SQLException {
        Network network = Network.newNetwork();
        postgres.setStartupAttempts(2);
        postgres.withNetwork(network);
        postgres.withNetworkAliases("postgis");
//        postgres.withDatabaseName("postgres");
        postgres.withUsername(dbpassword);
        postgres.withPassword(dbuser);
        postgres.start();
//        System.out.println(postgres.getJdbcUrl() + postgres.getUsername() + postgres.getPassword());
        conn2d = new PostgresClient(dburl2D, dbuser, dbpassword);
        conn3d = new PostgresClient(dburl3D, dbuser, dbpassword);
    }
    @Test
    public void spatialLinkTest() throws ServletException, IOException {

        HttpServletRequest request = mock(HttpServletRequest.class);
        HttpServletResponse response = mock(HttpServletResponse.class);

        when(request.getParameter("db2d")).thenReturn("ntu_2D");
        when(request.getParameter("db3d")).thenReturn("sg_ntu");
        when(request.getParameter("db2d_table")).thenReturn("ntu_05");

        SpatialLink spatialLink = new SpatialLink();
        spatialLink.setPostGISClient2d(conn2d);
        spatialLink.setPostGISClient3d(conn3d);
        spatialLink.doPut(request, response);
        
    }

    @AfterEach
    public void cleanup() {
        if (postgres.isRunning()) {
            postgres.stop();
        }
    }
}