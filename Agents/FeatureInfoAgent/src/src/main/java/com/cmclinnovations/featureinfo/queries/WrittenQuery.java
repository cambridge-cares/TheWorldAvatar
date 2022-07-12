package com.cmclinnovations.featureinfo.queries;

import org.json.JSONArray;

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
    public WrittenQuery(String iri, String endpoint, String query) {
        super(iri, endpoint);
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
