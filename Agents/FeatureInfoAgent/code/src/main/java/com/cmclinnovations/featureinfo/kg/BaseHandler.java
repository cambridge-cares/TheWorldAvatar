package com.cmclinnovations.featureinfo.kg;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.regex.Pattern;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONArray;

import com.cmclinnovations.featureinfo.FeatureInfoAgent;
import com.cmclinnovations.featureinfo.config.ConfigEndpoint;

import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

public abstract class BaseHandler {
    /**
     * Logger for reporting info/errors.
     */
    private static final Logger LOGGER = LogManager.getLogger(BaseHandler.class);

    /**
     * IRI of the asset.
     */
    private final String iri;

    /**
     * Endpoint(s) for the KG.
     */
    private final List<ConfigEndpoint> endpoints = new ArrayList<>();

    /**
     * Connection to KG.
     */
    private RemoteStoreClient rsClient;

    protected BaseHandler(final String iri, final List<ConfigEndpoint> endpoints) {
        String fixedIRI = iri;
        if (!fixedIRI.startsWith("<"))
            fixedIRI = "<" + fixedIRI;
        if (!fixedIRI.endsWith(">"))
            fixedIRI = fixedIRI + ">";

        this.iri = fixedIRI;
        this.endpoints.addAll(endpoints);
    }

    /**
     * Sets the remote store client used to connect to the KG.
     * 
     * @param rsClient KG connection client.
     */
    public final void setClient(final RemoteStoreClient rsClient) {
        this.rsClient = rsClient;
    }

    protected final boolean hasEndpoints() {
        return !endpoints.isEmpty();
    }

    protected final JSONArray runQuery(final String queryTemplate, final String description) throws Exception {

        // Inject parameters into query
        String query = filterIri(queryTemplate);

        query = filterOntopEndpoints(query);

        JSONArray rawResult;

        final List<String> endpointURLs = this.getEndpointURLs();
        if (endpointURLs.size() == 1) {
            LOGGER.debug("Running non-federated {} query.", description);
            rsClient.setQueryEndpoint(endpointURLs.get(0));
            rawResult = rsClient.executeQuery(query);

        } else {
            LOGGER.debug("Running federated {} query.", description);
            rawResult = rsClient.executeFederatedQuery(endpointURLs, query);
        }

        LOGGER.debug("Result of query was:\n{}", () -> (rawResult != null) ? rawResult.toString(2) : "null");

        return formatJSON(rawResult);
    }

    /**
     * Formats the raw JSON result into something the visualisation can use.
     * 
     * @param rawResult
     * @return
     */
    protected JSONArray formatJSON(final JSONArray rawResult) {
        return rawResult;
    }

    private final String filterIri(final String queryTemplate) {
        return queryTemplate.replaceAll(Pattern.quote("[IRI]"), iri);
    }

    /**
     * Get a list of the URLs from the input endpoints.
     * 
     * @return list of endpoint urls.
     */
    private final List<String> getEndpointURLs() {
        return endpoints.stream().map(ConfigEndpoint::url).toList();
    }

    /**
     * Returns the URL for the ONTOP endpoint.
     *
     * @return ONTOP url.
     */
    private final String getOntopURL() {
        final Optional<ConfigEndpoint> result = FeatureInfoAgent.CONFIG.getOntopEndpoint();
        if (result.isPresent()) {
            return result.get().url();
        }
        return null;
    }

    private final String filterOntopEndpoints(final String query) {
        // Inject ontop endpoint
        if (query.contains("[ONTOP]")) {
            final String ontopEndpoint = this.getOntopURL();
            if (ontopEndpoint == null) {
                throw new IllegalStateException("Could not determine Ontop endpoint!");
            }
            return query.replaceAll(Pattern.quote("[ONTOP]"), "<" + ontopEndpoint + ">");
        }
        return query;
    }
}
