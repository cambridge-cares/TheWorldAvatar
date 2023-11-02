package uk.ac.cam.cares.jps.base;

import org.testcontainers.containers.GenericContainer;
import org.testcontainers.containers.wait.strategy.Wait;
import org.testcontainers.utility.DockerImageName;

public class BlazegraphContainer extends GenericContainer<BlazegraphContainer> {

    public static final String BLAZEGRAPH_URL_PATH = "/blazegraph/namespace/kb/sparql";
    public static final String DELETE_ALL_QUERY = "DELETE {?s ?p ?o.} WHERE {?s ?p ?o.}";

    public BlazegraphContainer() {
        super(DockerImageName.parse("ghcr.io/cambridge-cares/blazegraph:1.1.0"));
        withExposedPorts(8080);
        waitingFor(Wait.forHttp(BLAZEGRAPH_URL_PATH));

    }
}
