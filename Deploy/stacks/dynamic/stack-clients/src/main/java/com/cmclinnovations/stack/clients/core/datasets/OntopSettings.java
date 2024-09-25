package com.cmclinnovations.stack.clients.core.datasets;

import java.util.Collections;
import java.util.List;
import java.util.Optional;

import com.fasterxml.jackson.annotation.JsonProperty;

public class OntopSettings {

    @JsonProperty
    private final Optional<List<String>> rules = Optional.empty();

    public List<String> getRules() {
        return rules.orElse(Collections.emptyList());
    }
}
