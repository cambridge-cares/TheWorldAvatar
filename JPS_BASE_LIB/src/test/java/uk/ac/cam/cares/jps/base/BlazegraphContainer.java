package uk.ac.cam.cares.jps.base;

import org.testcontainers.containers.GenericContainer;
import org.testcontainers.containers.wait.strategy.Wait;
import org.testcontainers.images.builder.Transferable;
import org.testcontainers.utility.DockerImageName;

import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

public class BlazegraphContainer extends GenericContainer<BlazegraphContainer> {

    public static final String USERNAME = "bg_user";
    public static final String PASSWORD = "bg_password";
    public static final String BLAZEGRAPH_PASSWORD_FILE = "/run/secrets/blazegraph_password";

    public static final String BLAZEGRAPH_URL_PATH = "/blazegraph/namespace/kb/sparql";
    public static final String DELETE_ALL_QUERY = "DELETE {?s ?p ?o.} WHERE {?s ?p ?o.}";

    private boolean authenticated;

    public BlazegraphContainer() {
        super(DockerImageName.parse("ghcr.io/cambridge-cares/blazegraph:1.2.0"));
        withExposedPorts(8080);
        waitingFor(Wait.forHttp(BLAZEGRAPH_URL_PATH));
    }

    public BlazegraphContainer withAuthentication() {
        withEnv("BLAZEGRAPH_PASSWORD_FILE", BLAZEGRAPH_PASSWORD_FILE);
        withEnv("BLAZEGRAPH_USER", USERNAME);
        withCopyToContainer(Transferable.of(PASSWORD), BLAZEGRAPH_PASSWORD_FILE);
        waitingFor(Wait.forHttp(BLAZEGRAPH_URL_PATH).withBasicCredentials(USERNAME, PASSWORD));
        authenticated = true;
        return this;
    }

    public String getURL() {
        return "http://" + getHost() + ":" + getFirstMappedPort() + BLAZEGRAPH_URL_PATH;
    }

    public RemoteStoreClient getRemoteStoreClient() {
        return getRemoteStoreClient(authenticated);
    }

    public RemoteStoreClient getRemoteStoreClient(boolean authenticated) {
        String endpoint = getURL();
        RemoteStoreClient remoteStoreClient = new RemoteStoreClient(endpoint, endpoint);
        if (authenticated) {
            remoteStoreClient.setUser(USERNAME);
            remoteStoreClient.setPassword(PASSWORD);
        }
        return remoteStoreClient;
    }
}
