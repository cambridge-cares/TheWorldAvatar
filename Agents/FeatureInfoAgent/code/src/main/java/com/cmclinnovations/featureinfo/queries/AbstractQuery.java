package com.cmclinnovations.featureinfo.queries;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONArray;

import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

/**
 * 
 */
public abstract class AbstractQuery {
    
    /**
     * Logger for reporting info/errors.
     */
    private static final Logger LOGGER = LogManager.getLogger(AbstractQuery.class);

    /**
     * Feature IRI
     */
    protected final String iri;

    /**
     * KG endpoint
     */
    protected final String endpoint;

    /**
     * KG connection
     */
    protected final RemoteStoreClient kgClient;

    /**
     * Initialise a new query.
     * 
     * @param iri Feature IRI
     * @param endpoint  KG endpoint
     */
    public AbstractQuery(String iri, String endpoint) {
        this.iri = iri;
        this.endpoint = endpoint;
        this.kgClient = new RemoteStoreClient(endpoint);
    }

    /**
     * Abstract method to query and return metadata.
     * 
     * @return metadata
     */
    public abstract JSONArray getMetadata();
}
