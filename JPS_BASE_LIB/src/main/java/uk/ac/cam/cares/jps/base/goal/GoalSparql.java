package uk.ac.cam.cares.jps.base.goal;

import uk.ac.cam.cares.jps.base.interfaces.StoreClientInterface;

import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class GoalSparql {

    private StoreClientInterface storeClient;
    private String goalInstanceBaseURL; // an example of this can be
    // "https://www.example.com/triplestore/repository/"


    /**
     * This constructor should be used to enable customised derivation instance base
     * URL.
     *
     * @param storeClient
     * @param derivationInstanceBaseURL
     */
    public GoalSparql(StoreClientInterface storeClient, String derivationInstanceBaseURL) {
        this.storeClient = storeClient;
        this.goalInstanceBaseURL = derivationInstanceBaseURL;
    }

    /**
     * add a time stamp instance to the input or goal if it does not exist the goal uses the unix timestamp
     *
     * @param kbClient
     * @param input
     */
    void addTimeInstance(String entity) {
        addTimeInstance(Arrays.asList(entity));
    }

    /**
     * This method adds timestamp to the given entities in bulk. It skips entities
     * who already have a timestamp or is a derived data.
     *
     * @param entities
     */
    void addTimeInstance(List<String> entities) {
        Map<String, Long> entitiesTimestamp = new HashMap<>();
        entities.stream().forEach(en -> {
            if (!entitiesTimestamp.containsKey(en)) {
                entitiesTimestamp.put(en, (long) 0);
            }
        });
        addTimeInstance((List<String>) entitiesTimestamp);
    }

}
