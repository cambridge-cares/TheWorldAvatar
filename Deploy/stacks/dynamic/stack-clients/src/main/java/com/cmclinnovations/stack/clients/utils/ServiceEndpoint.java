package com.cmclinnovations.stack.clients.utils;

import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf;

import org.eclipse.rdf4j.sparqlbuilder.core.QueryElement;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.GraphPattern;

public class ServiceEndpoint implements GraphPattern {
    private GraphPattern[] gps;
    private QueryElement service;

    public ServiceEndpoint(String serviceUrl, GraphPattern... gps) {
        this(Rdf.iri(serviceUrl), gps);
    }

    public ServiceEndpoint(QueryElement service, GraphPattern... gps) {
        this.service = service;
        this.gps = gps;
    }

    @Override
    public String getQueryString() {
        return String.format("SERVICE %s {%s}", service.getQueryString(),
                Stream.of(gps).map(GraphPattern::getQueryString).collect(Collectors.joining()));
    }

    /**
     * required for union to work
     */
    @Override
    public boolean isEmpty() {
        return (this.gps == null || gps.length == 0);
    }
}