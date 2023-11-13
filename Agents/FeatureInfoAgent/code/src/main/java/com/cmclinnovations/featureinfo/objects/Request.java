package com.cmclinnovations.featureinfo.objects;

import java.util.Optional;

import com.fasterxml.jackson.annotation.JsonAlias;
import com.fasterxml.jackson.annotation.JsonProperty;

public record Request(@JsonProperty("iri") @JsonAlias("IRI") String iri,
        @JsonProperty("endpoint") @JsonAlias("ENDPOINT") Optional<String> endpoint) {
}
