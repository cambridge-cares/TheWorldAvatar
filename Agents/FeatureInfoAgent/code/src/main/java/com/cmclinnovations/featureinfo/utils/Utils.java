package com.cmclinnovations.featureinfo.utils;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import com.cmclinnovations.featureinfo.config.ConfigStore;
import com.cmclinnovations.featureinfo.config.StackEndpoint;
import com.cmclinnovations.featureinfo.config.StackEndpointType;

/**
 * Misc utilities for the FeatureInfoAgent.
 */
public final class Utils {
    
    /**
     * Logger for reporting info/errors.
     */
    private static final Logger LOGGER = LogManager.getLogger(Utils.class);

    /**
     * Constructor.
     */
    private Utils() {
        // No
    }

	/**
	 * Injects the input IRI and Ontop endpoint into the input SPARQL query.
	 * 
	 * @param query SPARQL query to inject into.
	 * @param iri feature IRI to inject.
	 * @param ontops List of ontop endpoints for injection.
	 * @param blazegraphs List of blazegraph endpoints for injection.
	 * 
	 * @return Updated SPARQL query string.
	 */
	public static String queryInject(String query, String iri, List<StackEndpoint> ontops, List<StackEndpoint> blazegraphs) {
		String updatedQuery = query;

		if(!iri.startsWith("<")) iri = "<" + iri;
        if(!iri.endsWith(">")) iri += ">";
		updatedQuery = updatedQuery.replaceAll(Pattern.quote("[IRI]"), iri);

		// Build SPARQL values strings
		StringBuilder bothBuilder = new StringBuilder();
		StringBuilder blazegraphBuilder = new StringBuilder();

		ontops.forEach(endpoint -> {
			bothBuilder.append("<");
			bothBuilder.append(endpoint.url());
			bothBuilder.append("> ");
		});
		blazegraphs.forEach(endpoint -> {
			bothBuilder.append("<");
			bothBuilder.append(endpoint.url());
			bothBuilder.append("> ");
			blazegraphBuilder.append("<");
			blazegraphBuilder.append(endpoint.url());
			blazegraphBuilder.append("> ");
		});

		updatedQuery = updatedQuery.replaceAll(Pattern.quote("[ENDPOINTS-ALL]"), bothBuilder.toString());
		updatedQuery = updatedQuery.replaceAll(Pattern.quote("[ENDPOINTS-BLAZEGRAPH]"), blazegraphBuilder.toString());

		// If a single Ontop endpoint is present, inject that
		if(ontops.size() == 1) {
			String ontopURL = "<" + ontops.get(0).url() + ">";
			updatedQuery = updatedQuery.replaceAll(Pattern.quote("[ONTOP]"), ontopURL);
		}

		return updatedQuery;
	}

	/**
	 * Returns the URL of the Ontop endpoint from the input configuration store.
	 * 
	 * @param configStore configuration store.
	 * 
	 * @return Ontop URL.
	 */
	public static String getOntopURL(ConfigStore configStore) {
		StackEndpoint ontopEndpoint = configStore.getStackEndpoints(StackEndpointType.ONTOP)
			.stream()
			.findFirst()
			.orElse(null);
		return (ontopEndpoint == null) ? null : ontopEndpoint.url();
	}

	/**
     * Return a list of stack endpoints to use during KG queries.
     * 
	 * @param configStore configuration store.
	 * @param enforcedEndpoint optional enforced Blazegraph URL.
	 * 
     * @return StackEndpoint instance
     */
    public static List<StackEndpoint> getBlazegraphEndpoints(ConfigStore configStore, Optional<String> enforcedEndpoint) {
        // If an endpoint is enforced, try to find it's match from the auto-discovered ones
        if(enforcedEndpoint.isPresent()) {
            StackEndpoint match = configStore.getStackEndpoints(StackEndpointType.BLAZEGRAPH)
                .stream()
                .filter(endpoint -> endpoint.url().equalsIgnoreCase(enforcedEndpoint.get()))
                .findFirst()
                .orElse(null);

            if(match == null) {
                // No match, create a new temporary one.
                match = new StackEndpoint(enforcedEndpoint.get(), null, null, StackEndpointType.BLAZEGRAPH);
            }
            return new ArrayList<>(Arrays.asList(match));
        }

        return configStore.getStackEndpoints(StackEndpointType.BLAZEGRAPH);
    }

	/**
     * Return a list of stack URLs to use during KG queries.
     * 
	 * @param configStore configuration store.
	 * @param enforcedEndpoint optional enforced Blazegraph URL.
	 * 
     * @return KG URLs.
     */
	public static List<String> getBlazegraphURLs(ConfigStore configStore, Optional<String> enforcedEndpoint) {
		List<StackEndpoint> endpoints = getBlazegraphEndpoints(configStore, enforcedEndpoint);
		return endpoints.stream()
			.map(endpoint -> endpoint.url())
			.collect(Collectors.toList());
	}

}
// End of class.