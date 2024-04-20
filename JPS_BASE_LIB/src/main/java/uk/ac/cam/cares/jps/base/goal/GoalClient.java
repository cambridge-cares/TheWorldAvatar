package uk.ac.cam.cares.jps.base.goal;

import java.time.Instant;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.Stream;

import org.apache.http.client.methods.CloseableHttpResponse;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.entity.ContentType;
import org.apache.http.entity.StringEntity;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClients;
import org.apache.http.util.EntityUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.TriplePattern;
import org.jgrapht.graph.DefaultEdge;
import org.jgrapht.graph.DirectedAcyclicGraph;
import org.json.JSONArray;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.derivation.DerivationClient;
import uk.ac.cam.cares.jps.base.derivation.DerivationSparql;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.interfaces.StoreClientInterface;

public class GoalClient {

    /**
     * Logger for error output.
     */
    private static final Logger LOGGER = LogManager.getLogger(GoalClient.class);

    StoreClientInterface kbClient;
    GoalSparql sparqlClient;

    /**
     * This constructor should be used to enable customised goal instance baseURL.
     *
     * @param kbClient
     * @param goalInstanceBaseURL
     */
    public GoalClient(StoreClientInterface kbClient, String goalInstanceBaseURL) {
        this.kbClient = kbClient;
        this.sparqlClient = new GoalSparql(kbClient, goalInstanceBaseURL);
    }
}
