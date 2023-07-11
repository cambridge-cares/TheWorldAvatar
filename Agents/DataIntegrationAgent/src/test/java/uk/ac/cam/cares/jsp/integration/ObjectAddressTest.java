package uk.ac.cam.cares.jsp.integration;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.testcontainers.containers.Network;
import org.testcontainers.containers.PostgreSQLContainer;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.utility.DockerImageName;

import java.sql.SQLException;

class ObjectAddressTest {
    DockerImageName postgisImage = DockerImageName.parse("postgis/postgis:14-3.2-alpine").asCompatibleSubstituteFor("postgres");
    @Container
    private PostgreSQLContainer<?> postgres = new PostgreSQLContainer<>(postgisImage);
    private String dburl2D = "jdbc:postgresql://localhost:5432/pirmasens_2d";
    private String dburl3D = "jdbc:postgresql://localhost:5432/pirmasens";
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
        postgres.withUsername(dbpassword);
        postgres.withPassword(dbuser);
        postgres.start();
//        conn2d = new PostgresClient(dburl2D, dbuser, dbpassword);
//        conn3d = new PostgresClient(dburl3D, dbuser, dbpassword);
    }
    @Test
    void insertAtoB() throws SQLException {
        String gmlid = "ID_441ea408-882d-4e59-bce3-a7d0b2dacd28";
        ObjectAddress address = new ObjectAddress();
        address.setPostGISClient(conn3d);
        // address.insertAtoB(gmlid);
    }

    @AfterEach
    public void cleanup() {
        if (postgres.isRunning()) {
            postgres.stop();
        }
    }
}