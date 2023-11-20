package com.cmclinnovations.filteragent;

import org.apache.logging.log4j.Level;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONArray;

import com.google.gson.Gson;

import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

public class QueryClient {

    public Gson gson;
    private static final Logger LOGGER = LogManager.getLogger(QueryClient.class);

    public QueryClient(Gson gson) {
        this.gson = gson;
    }

    public static JSONArray executeQuery(RemoteStoreClient remoteStoreClient, String queryString) {
        long startTime = System.nanoTime();
        JSONArray result;
        Level logLevel = Level.ERROR;
        try {
            result = remoteStoreClient.executeQuery(queryString);
            logLevel = Level.INFO;
        } finally {
            long endTime = System.nanoTime();
            long duration = (endTime - startTime) / 1000_000_000;
            LOGGER.log(logLevel, "On '{}' it took {}s to run the query {}", remoteStoreClient.getQueryEndpoint(),
                    duration, queryString.replaceAll("\r?\n", "\\\\n"));
        }
        return result;
    }

}
