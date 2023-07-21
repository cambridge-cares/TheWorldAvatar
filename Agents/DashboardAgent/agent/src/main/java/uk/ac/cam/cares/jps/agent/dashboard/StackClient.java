package uk.ac.cam.cares.jps.agent.dashboard;

import com.cmclinnovations.stack.clients.blazegraph.BlazegraphEndpointConfig;
import com.cmclinnovations.stack.clients.docker.ContainerClient;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

/**
 * A client that checks if the agent is running on a stack and retrieve the required endpoints.
 *
 * @author qhouyee
 */
public class StackClient {
    private static final Logger LOGGER = LogManager.getLogger(DashboardAgent.class);
    private final String STACK_ENDPOINT;

    /**
     * Standard Constructor.
     */
    public StackClient() {
        LOGGER.debug("Attempting to retrieve services from the stack...");
        ContainerClient client = new ContainerClient();
        BlazegraphEndpointConfig blazeConfig = client.readEndpointConfig("blazegraph", BlazegraphEndpointConfig.class);
        this.STACK_ENDPOINT = "http://" + blazeConfig.getHostName() + ":" + blazeConfig.getPort();
    }
}
