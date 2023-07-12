package uk.ac.cam.cares.jsp.linking;

import org.apache.log4j.LogManager;
import org.apache.log4j.Logger;

import com.cmclinnovations.stack.clients.docker.ContainerClient;


public class PostgresClient extends ContainerClient {
    private static final Logger LOGGER = LogManager.getLogger(PostgresClient.class);
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
}
