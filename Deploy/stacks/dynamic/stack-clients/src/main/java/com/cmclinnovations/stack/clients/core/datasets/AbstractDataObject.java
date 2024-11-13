package com.cmclinnovations.stack.clients.core.datasets;

import java.util.Optional;

import com.cmclinnovations.stack.clients.utils.JsonHelper;
import com.fasterxml.jackson.annotation.JsonProperty;

public abstract class AbstractDataObject {

    @JsonProperty(defaultValue = "false")
    private final boolean skip;
    @JsonProperty
    private final Optional<String> description;

    protected AbstractDataObject() {
        this.description = Optional.empty();
        this.skip = false;
    }

    protected AbstractDataObject(Optional<String> description, boolean skip) {
        this.description = description;
        this.skip = skip;
    }

    public abstract String getName();

    public boolean isSkip() {
        return skip;
    }

    public String getDescription() {
        return description.map(JsonHelper::handleFileValues).orElse(getName());
    }
}
