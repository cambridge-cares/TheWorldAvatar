package com.cmclinnovations.stack.clients.superset;

import java.util.regex.Matcher;
import java.util.regex.Pattern;
import com.cmclinnovations.stack.clients.core.ClientWithEndpoint;
import com.cmclinnovations.stack.clients.core.EndpointNames;
import com.cmclinnovations.stack.clients.docker.ContainerClient;
import com.cmclinnovations.stack.clients.ontop.OntopEndpointConfig;

import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

public class SupersetClient extends ContainerClient implements ClientWithEndpoint {

    private static final Pattern SERVICE_IRI_PATTERN = Pattern.compile("SERVICE\\s*<([a-z]+)>",
            Pattern.CASE_INSENSITIVE);

    private static SupersetClient instance = null;

    public static SupersetClient getInstance() {
        if (null == instance) {
            instance = new SupersetClient();
        }
        return instance;
    }

    private final SupersetEndpointConfig supersetEndpoint;

    private SupersetClient() {
        supersetEndpoint = readEndpointConfig(EndpointNames.SUPERSET, SupersetEndpointConfig.class);
    }

    @Override
    public SupersetEndpointConfig getEndpoint() {
        return supersetEndpoint;
    }

    public RemoteStoreClient getRemoteStoreClient() {
        String url = supersetEndpoint.getUrl();
        return new RemoteStoreClient(url, url,
        supersetEndpoint.getUsername(),
        supersetEndpoint.getPassword());
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
