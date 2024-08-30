package com.cmclinnovations.stack.clients.core.datasets;

import java.util.Collections;
import java.util.List;
import java.util.Optional;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

public class OntopSettings {

    @JsonProperty
    private final Optional<List<String>> rules;

    @JsonCreator
    OntopSettings() {
        this.rules = Optional.empty();
    }

    OntopSettings(Optional<List<String>> rules) {
        this.rules = rules;
    }

    public List<String> getRules() {
        return rules.orElse(Collections.emptyList());
    }
}
