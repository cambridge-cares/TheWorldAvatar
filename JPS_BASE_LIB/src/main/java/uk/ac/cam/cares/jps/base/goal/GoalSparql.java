package uk.ac.cam.cares.jps.base.goal;

import uk.ac.cam.cares.jps.base.interfaces.StoreClientInterface;

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

}
