package com.cmclinnovations.featureinfo.kg;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.regex.Matcher;
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

    protected final String filterOntopEndpoints(final String query) {
        Optional<ConfigEndpoint> ontopEndpoint = FeatureInfoAgent.CONFIG.getOntopEndpoint();
        String ontopURL;
        // Inject ontop endpoint
        if (ontopEndpoint.isEmpty()) {
            ontopURL = null;
        } else {
            ontopURL = ontopEndpoint.get().url();
        }
        if (ontopURL == null) {
            throw new IllegalStateException("Could not determine URL for Ontop endpoint!");
        }
        Pattern pattern = Pattern.compile("\\[ONTOP(-?[^\\]]*)\\]",
                Pattern.CASE_INSENSITIVE | Pattern.UNICODE_CASE);
        Matcher matcher = pattern.matcher(query);
        StringBuilder sb = new StringBuilder();
        int lastIndex = 0;
        while (matcher.find()) {
            for (int i = 0; i < matcher.groupCount(); ++i) {
                String replacementUrlComponent = "/ontop" + matcher.group(2 * i + 1) + "/";
                String url = ontopURL.replaceFirst("/ontop/", replacementUrlComponent);
                sb.append(query, lastIndex, matcher.start())
                        .append("<" + url + ">");
                lastIndex = matcher.end();
            }
        }
        sb.append(query, lastIndex, query.length());
        return sb.toString();
    }
}
