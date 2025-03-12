package com.cmclinnovations.agent.model.type;

import java.util.stream.Collectors;
import java.util.stream.Stream;

public enum SparqlEndpointType {
    MIXED(null) {
        public String getIri() {
            return Stream.of(values())
                    .filter(type -> type != SparqlEndpointType.MIXED)
                    .map(SparqlEndpointType::getIri)
                    .collect(Collectors.joining(", "));
        }
    },
    BLAZEGRAPH("ser:Blazegraph"),
    ONTOP("ser:Ontop");

    private final String iri;

    private SparqlEndpointType(String iri) {
        this.iri = iri;
    }

    public String getIri() {
        return iri;
    }

}