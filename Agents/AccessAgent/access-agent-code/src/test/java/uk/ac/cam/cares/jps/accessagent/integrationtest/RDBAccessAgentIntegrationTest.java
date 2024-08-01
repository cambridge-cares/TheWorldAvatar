package uk.ac.cam.cares.jps.accessagent.integrationtest;

import org.junit.jupiter.api.*;
import org.testcontainers.containers.GenericContainer;
import org.testcontainers.containers.Network;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.junit.jupiter.Testcontainers;
import org.testcontainers.utility.DockerImageName;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.*;

import static org.junit.Assert.assertEquals;

/**
 * Integration tests for the RDB Access Agent.
 * This uses the RDBAccessAgentCaller methods in JPS_BASE_LIB
 * to call the RDBAccessAgent inside a Docker container.
 *
 * To test a new version of the RDBAccessAgent:
 * 1. Build the new AccessAgent Docker image
 * 	  if it doesn't already exist in the registry (see README)
 * 2. Update the ACCESS_AGENT_VERSION variable to the new version number
 * 3. Run the integration tests.
 *
 * @author Mehal Agarwal (ma988@cam.ac.uk)
 *
 */
@Disabled("Requires Testcontainers and Docker to run. The AccessAgent Docker image must be built.")
@Testcontainers
public class RDBAccessAgentIntegrationTest {


    //User defined variables
    //set the desired access agent version number here
    static final String ACCESS_AGENT_VERSION = "1.6.1";

    //////////////////////////////////////////////////

    static final String ACCESS_AGENT_IMAGE ="ghcr.io/cambridge-cares/access-agent:"+ACCESS_AGENT_VERSION;
    static final String BLAZEGRAPH_IMAGE = "ghcr.io/cambridge-cares/blazegraph_for_tests:1.0.0";
    static final int BLAZEGRAPH_INTERNAL_PORT = 9999;

    //Put all containers on the same network
    static final Network NETWORK = Network.newNetwork();
    static final String RDB_STORE_ROUTER_CONTAINER_ALIAS = "rdb-store-router-container";

    static final String RDB_STORE_ROUTER_ENDPOINT = "http://" + RDB_STORE_ROUTER_CONTAINER_ALIAS
            + ":"+Integer.toString(BLAZEGRAPH_INTERNAL_PORT)+"/blazegraph/namespace/kb/sparql";

    //Create only one store router triple store and Access Agent for the entire test
    @Container
    static final GenericContainer<?> RDB_STORE_ROUTER_CONTAINER = new GenericContainer<>(DockerImageName.parse(BLAZEGRAPH_IMAGE))
            .withExposedPorts(BLAZEGRAPH_INTERNAL_PORT)
            .withNetwork(NETWORK)
            .withNetworkAliases(RDB_STORE_ROUTER_CONTAINER_ALIAS);
    @Container
    static final GenericContainer<?> RDB_ACCESS_AGENT_CONTAINER = new GenericContainer<>(DockerImageName.parse(ACCESS_AGENT_IMAGE))
            .withExposedPorts(8080)
            .withEnv(RDBStoreRouter.RDB_STOREROUTER_ENDPOINT_NAME,RDB_STORE_ROUTER_ENDPOINT)
            .withNetwork(NETWORK)
            .dependsOn(RDB_STORE_ROUTER_CONTAINER);


    String Label;
    String targetResourceID;

    @BeforeAll
    static void setupAll() {
        try {
            RDB_STORE_ROUTER_CONTAINER.start();
            RDB_ACCESS_AGENT_CONTAINER.start();
        } catch (Exception e) {
            throw new JPSRuntimeException("AccessAgentIntegrationTest: Docker container startup failed. Please try running tests again");
        }
    }


    @AfterAll
    public static void stopAll(){
        if (RDB_STORE_ROUTER_CONTAINER.isRunning()) {
            RDB_STORE_ROUTER_CONTAINER.stop();
        }
        if (RDB_ACCESS_AGENT_CONTAINER.isRunning()) {
            RDB_ACCESS_AGENT_CONTAINER.stop();
        }
    }

    // Integration tests

    @Test
    void testPostSparqlQuery() {

        //Upload routing information
        String endpoint = "http://" + RDB_STORE_ROUTER_CONTAINER.getHost()
                + ":" + RDB_STORE_ROUTER_CONTAINER.getFirstMappedPort()+"/blazegraph/namespace/kb/sparql";
        RemoteStoreClient storeClient = new RemoteStoreClient(endpoint, endpoint);

        String update = "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> \n" +
                "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>\n"+"PREFIX owl: <http://www.w3.org/2002/07/owl#>\n"+"INSERT DATA{"+"<http://host.docker.internal:9999/blazegraph/ontordbrouter/test> <http://www.theworldavatar.com/kg/ontordbrouter/hasUrl> \"jdbc:postgresql://localhost:5432/test\"."+"\n"+
                "<http://host.docker.internal:9999/blazegraph/ontordbrouter/test> rdf:type <http://www.theworldavatar.com/kg/ontordbrouter/TargetRDBResource>."+"\n"+
                "<http://host.docker.internal:9999/blazegraph/ontordbrouter/test> rdf:type owl:NamedIndividual."+"\n"+
                "<http://host.docker.internal:9999/blazegraph/ontordbrouter/test> rdfs:label \"test\"}";

        storeClient.executeUpdate(update);

        Label = "test";

        targetResourceID = "http://" + RDB_ACCESS_AGENT_CONTAINER.getHost()
                + ":" + RDB_ACCESS_AGENT_CONTAINER.getFirstMappedPort()
                + "/" + Label;

        String url = RDBAccessAgentCaller.getRDBUrl(targetResourceID);

        assertEquals("jdbc:postgresql://localhost:5432/test",url);
    }

}
