package com.cmclinnovations.featureinfo.queries;

import org.json.JSONArray;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

public class WrittenQuery extends AbstractQuery {

    /**
     * Pre-written SPARQL query.
     */
    private final String query;

    /**
     * 
     * @param iri
     * @param endpoint
     * @param query
     */
    public WrittenQuery(String iri, String endpoint, RemoteStoreClient kgClient, String query) {
        super(iri, endpoint, kgClient);
        this.query = query;
    }

    /**
     * 
     */
    @Override
    public JSONArray getMetadata() {
        return super.kgClient.executeQuery(this.query);
    }
    
}
