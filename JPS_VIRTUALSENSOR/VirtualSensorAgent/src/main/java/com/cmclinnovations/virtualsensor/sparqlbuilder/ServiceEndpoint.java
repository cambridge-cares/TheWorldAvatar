package com.cmclinnovations.virtualsensor.sparqlbuilder;

import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.eclipse.rdf4j.sparqlbuilder.graphpattern.GraphPattern;

public class ServiceEndpoint {
    private final String endpoint;

    public ServiceEndpoint(String endpoint) {
        this.endpoint = endpoint;
    }

    public GraphPattern service(GraphPattern... gps) {
        return () -> "SERVICE <" + endpoint + "> {"
                + Stream.of(gps).map(GraphPattern::getQueryString).collect(Collectors.joining()) + "}";
    }
}