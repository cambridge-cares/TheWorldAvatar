package uk.ac.cam.cares.goal.framework;

import java.util.Arrays;
import java.util.List;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import uk.ac.cam.cares.jps.base.interfaces.StoreClientInterface;



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

    /**
     * adds a timestamp to your input following the w3c standard for unix timestamp
     * https://www.w3.org/TR/owl-time/
     * <entity> <hasTime> <time>, <time> <numericPosition> 123
     *
     * @param entity
     */
    public void addTimeInstance(String entity) {
        // calls the method that adds timestamp in bulk
        addTimeInstance(Arrays.asList(entity));
        LOGGER.info("Added timestamp to <" + entity + "> if it doesn't have a timestamp already");
    }

    /**
     * same method as above but in bulk
     *
     * @param entities
     */
    public void addTimeInstance(List<String> entities) {
        this.sparqlClient.addTimeInstance(entities);
        LOGGER.info("Added timestamps to <" + entities + "> if they don't have a timestamp already");
    }

    /**
     *     Create take in agent_iri, range_iri, realstate_iri, create goal instance
     */

    public String createGoalForNewInfo(String agent_iri, String agent_url, String range_iri, String realstate_iri){

        String goalIRI = this.sparqlClient.createGoalIRI();
        this.sparqlClient.createNewGoal(goalIRI,agent_iri,range_iri,realstate_iri);
        LOGGER.info("Instantiated goal with time series <" + goalIRI + ">");
        return goalIRI;
    }


    /**
     * Update goal
     * Take in goal_iri
     */
    public void updateGoal (String goal_iri){
        //Query for Goal_IRI "ex:isAchievedUsing"




        //Send Request to TruckAgent with (RealState)

    }


}
