package com.cmclinnovations.stack.clients.core.datasets;

import java.util.Optional;

import com.fasterxml.jackson.annotation.JsonProperty;

public abstract class AbstractDataObject {

    public abstract String getName();

    @JsonProperty(defaultValue = "false")
    private final boolean skip = false;

    @JsonProperty
    private final Optional<String> description = Optional.empty();

    public boolean isSkip() {
        return skip;
    }

    public String getDescription() {
        return description.orElse(getName());
    }
}
