package uk.ac.cam.cares.jps.agent.isochroneagent;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONArray;
import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

public class QueryClient {
    private static final Logger LOGGER = LogManager.getLogger(QueryClient.class);

    private RemoteStoreClient storeClient;
    private RemoteStoreClient ontopStoreClient;
    private RemoteRDBStoreClient rdbStoreClient;


    QueryClient(RemoteStoreClient storeClient, RemoteStoreClient ontopStoreClient, RemoteRDBStoreClient rdbStoreClient) {
        this.storeClient = storeClient;
        this.ontopStoreClient = ontopStoreClient;
        this.rdbStoreClient = rdbStoreClient;
    }

    public JSONArray getPOIwkt (String sparqlQuery ) {

        JSONArray queryResult=storeClient.executeQuery(sparqlQuery);
        return queryResult;
    }

}