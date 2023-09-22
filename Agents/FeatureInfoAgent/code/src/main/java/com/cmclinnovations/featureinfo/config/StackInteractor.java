package com.cmclinnovations.featureinfo.config;

import java.util.ArrayList;
import java.util.List;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import com.cmclinnovations.stack.clients.blazegraph.BlazegraphEndpointConfig;
import com.cmclinnovations.stack.clients.docker.ContainerClient;
import com.cmclinnovations.stack.clients.ontop.OntopEndpointConfig;
import com.cmclinnovations.stack.clients.postgis.PostGISEndpointConfig;

/**
 * This class handles interactions with the TWA Stack through the
 * stack client library.
 */
public class StackInteractor extends ContainerClient {
    
    /**
     * Logger for reporting info/errors.
     */
    private static final Logger LOGGER = LogManager.getLogger(StackInteractor.class);

    /**
     * Pool of parsed endpoint entries.
     */
    private final List<StackEndpoint> endpoints;

    /**
     * Initialise a new StackInteractor instance.
     * 
     * @param endpoints Pool of parsed endpoints to add to.
     */
    public StackInteractor(List<StackEndpoint> endpoints) {
        this.endpoints = endpoints;
    }

    /**
     * Uses the TWA Stack client library to determine all Ontop, PostgreSQL, and Blazegraph
     * endpoints within the current stack instance.
     */
    public void discoverEndpoints() {
        this.endpoints.addAll(discoverOntop());
        this.endpoints.addAll(discoverPostgres());
        this.endpoints.addAll(discoverBlazegraph());
    }

    /**
     * Determines available Ontop endpoints.
     * 
     * @returns List of available Ontop endpoints.
     */
    private List<StackEndpoint> discoverOntop() {
        List<StackEndpoint> ontopEndpoints = new ArrayList<>();

        OntopEndpointConfig ontopConfig = readEndpointConfig("ontop", OntopEndpointConfig.class);
        ontopEndpoints.add(new StackEndpoint(
            ontopConfig.getUrl(),
            ontopConfig.getUsername(),
            ontopConfig.getPassword(),
            StackEndpointType.ONTOP
        ));

        LOGGER.info("Have discovered a local Ontop endpoint: {}", ontopConfig.getUrl());
        return ontopEndpoints;
    }

    /**
     * Determines available PostgreSQL endpoints.
     * 
     * @returns List of available PostgreSQL endpoints.
     */
    private List<StackEndpoint> discoverPostgres() {
        List<StackEndpoint> postgresEndpoints = new ArrayList<>();

        PostGISEndpointConfig postgresConfig = readEndpointConfig("postgis", PostGISEndpointConfig.class);
        postgresEndpoints.add(new StackEndpoint(
            postgresConfig.getJdbcDriverURL(),
            postgresConfig.getUsername(),
            postgresConfig.getPassword(),
            StackEndpointType.ONTOP
        ));

        LOGGER.info("Have discovered a local Ontop endpoint: {}", postgresConfig.getJdbcDriverURL());
        return postgresEndpoints;
    }

    /**
     * Determines available Blazegraph endpoints.
     * 
     * @returns List of available Blazegraph endpoints.
     */
    private List<StackEndpoint> discoverBlazegraph() {
        // Use the client library to get the root URL of Blazezgraph
        BlazegraphEndpointConfig blazeConfig = readEndpointConfig("blazegraph", BlazegraphEndpointConfig.class);

        // Rather than asking the stack client library, ask Blazegraph itself to
        // tell use the URL endpoint for each of its namespaces.
        NamespaceGetter getter = new NamespaceGetter(
            blazeConfig.getServiceUrl(),
            blazeConfig.getUsername(),
            blazeConfig.getPassword()
        );

        // Run logic to query for namespaces
        try {
            List<StackEndpoint> blazegraphEndpoints = getter.discoverEndpoints();
            return blazegraphEndpoints;
        } catch(Exception exception) {
            LOGGER.error("Could not contact Blazegraph to determine namespace URLs!", exception);
            return new ArrayList<>();
        }
    }

}
// End of class.