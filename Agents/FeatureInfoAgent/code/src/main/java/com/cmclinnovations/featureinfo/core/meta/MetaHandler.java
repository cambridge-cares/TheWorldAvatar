package com.cmclinnovations.featureinfo.core.meta;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import javax.servlet.http.HttpServletResponse;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONArray;
import org.json.JSONObject;

import com.cmclinnovations.featureinfo.config.ConfigEntry;
import com.cmclinnovations.featureinfo.config.ConfigStore;
import com.cmclinnovations.featureinfo.config.StackEndpointType;
import com.cmclinnovations.featureinfo.utils.Utils;

import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

/**
 * This class handles querying the Knowledge Graph to get meta data.
 */
public class MetaHandler {

    /**
     * Logger for reporting info/errors.
     */
    private static final Logger LOGGER = LogManager.getLogger(MetaHandler.class);

    /**
     * IRI of the asset.
     */
    private final String iri;

    /**
     * Optional enforced Blazegraph URL.
     */
    private final Optional<String> enforcedEndpoint;
    
    /**
     * Configuration store.
     */
    private final ConfigStore configStore;

    /**
     * Connection to KG.
     */
    private RemoteStoreClient kgClient;

    /**
     * Initialise a new MetaHandler instance.
     * 
     * @param iri IRI of the asset.
     * @param enforcedEndpoint optional enforced Blazegraph URL.
     * @param configStore Store of class mappings and stack endpoints.
     */
    public MetaHandler(String iri, Optional<String> enforcedEndpoint, ConfigStore configStore) {
        this.iri = iri;
        this.enforcedEndpoint = enforcedEndpoint;
        this.configStore = configStore;
    }

    /**
     * Sets the remote store client used to connect to the KG.
     * 
     * @param kgClient KG connection client.
     */
    public void setClient(RemoteStoreClient kgClient) {
        this.kgClient = kgClient;
    }

    /**
     * Queries the KG to determine the classes representing the current IRI, then finds
     * the first linked SPARQL query before executing it and returning the result.
     * 
     * @param classMatches configuration entries that contain class matches.
     * @param response HTTP response to write to.
     * 
     * @return JSONArray of query result.
     */
    public JSONObject getData(List<ConfigEntry> classMatches, HttpServletResponse response) {
        List<JSONArray> rawResults = new ArrayList<>();
        
        // Iterate through each matching query
        classMatches.forEach(classMatch -> {
            try {
                JSONArray rawResult = runQuery(classMatch);
                rawResults.add(rawResult);
            } catch(Exception exception) {
                LOGGER.error("Execution for meta data query has failed!", exception);
            }
        });

        // Format and combine meta data
        return MetaParser.formatData(rawResults);  
    }

    /**
     * Performs a meta data query for the input class match.
     * 
     * @param classMatch configuration entry containing query details.
     * 
     * @return Resulting JSONArray of meta data.
     * 
     * @throws Exception if SPARQL execution fails.
     */
    private JSONArray runQuery(ConfigEntry classMatch) throws Exception {
        String templateQuery = classMatch.getMetaQueryContent();
        String query = Utils.queryInject(
            templateQuery,
            this.iri,
            configStore.getStackEndpoints(StackEndpointType.ONTOP),
            Utils.getBlazegraphEndpoints(configStore, enforcedEndpoint)
        );

        // Run query
        List<String> endpoints = Utils.getBlazegraphURLs(configStore, enforcedEndpoint);

        if(endpoints.size() == 1) {
            LOGGER.debug("Running non-federated meta data query.");
            kgClient.setQueryEndpoint(endpoints.get(0));
            return kgClient.executeQuery(query);

        } else {
            LOGGER.debug("Running federated meta data query.");
            return kgClient.executeFederatedQuery(endpoints, query);
        }
    }
    
}
// End of class.