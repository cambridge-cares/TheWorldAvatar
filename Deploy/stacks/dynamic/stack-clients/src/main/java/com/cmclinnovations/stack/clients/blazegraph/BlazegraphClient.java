package com.cmclinnovations.stack.clients.blazegraph;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.cmclinnovations.stack.clients.docker.ContainerClient;
import com.cmclinnovations.stack.clients.ontop.OntopEndpointConfig;

public class BlazegraphClient extends ContainerClient {

    private static final Pattern SERVICE_IRI_PATTERN = Pattern.compile("SERVICE\\s*<([a-z]+)>",
            Pattern.CASE_INSENSITIVE);

    private static BlazegraphClient instance = null;

    public static BlazegraphClient getInstance() {
        if (null == instance) {
            instance = new BlazegraphClient();
        }
        return instance;
    }

    private BlazegraphClient() {
    }

    /**
     * Method for replacing placeholders with real values
     * This is currently restricted to replacing endpoint names with their URL in
     * SPARQL SERVICE patterns, and specifically just for Ontop endpoints.
     * 
     * @param query query to be filtered
     * @return the query after the appropriate substitutions have been made
     */
    public String filterQuery(String query) {
        Matcher matcher = SERVICE_IRI_PATTERN.matcher(query);
        if (matcher.find()) {
            String serviceName = matcher.group(1).toLowerCase();
            return matcher.replaceAll("SERVICE <" +
                    readEndpointConfig(serviceName, OntopEndpointConfig.class).getUrl()
                    + ">");
        } else {
            return query;
        }
    }

}
