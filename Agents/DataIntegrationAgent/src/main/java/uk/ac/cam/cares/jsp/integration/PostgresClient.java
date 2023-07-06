package uk.ac.cam.cares.jsp.integration;

import com.cmclinnovations.stack.clients.docker.ContainerClient;
import com.cmclinnovations.stack.clients.postgis.PostGISEndpointConfig;
import org.apache.log4j.LogManager;
import org.apache.log4j.Logger;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;

public class PostgresClient extends ContainerClient {
    private static final Logger LOGGER = LogManager.getLogger(DataIntegrationAgent.class);
    private final String postGISContainerId;
    private static final String PASSWORD_OPTION = "PGPASSWORD='";
    private static final String USER_OPTION = "-U ";
    private static final String SERVER_OPTION = "-h ";
    private static final String PORT_OPTION = "-p ";
    private static final String WHITESPACE = " ";

    /**
     * Standard Constructor intialising the client based on the endpoint.
     *
     * @param isStack A boolean indicating whether the agent is running in a stack.
     */
    public PostgresClient(boolean isStack) {
        postGISContainerId = isStack ? getContainerId("postgis") : null;
    }

//    PostgresClient(String dbrul, String dbuser, String dbpassword) {
//        this.dbrul = dbrul;
//        this.dbuser = dbuser;
//        this.dbpassword = dbpassword;
//    }

//    Connection getConnection() throws SQLException {
//        try {
//            Class.forName("org.postgresql.Driver");
//        } catch (ClassNotFoundException e) {
//            LOGGER.error(e.getMessage());
//            LOGGER.error("Failed to find postgre driver");
//        }
//        return DriverManager.getConnection(this.dbrul, this.dbuser, this.dbpassword);
//    }


}
