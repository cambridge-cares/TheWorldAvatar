package com.cmclinnovations.stack.clients.core.visualisationsources;

import com.cmclinnovations.stack.clients.core.datasets.Dataset;
import com.fasterxml.jackson.annotation.JsonSubTypes;
import com.fasterxml.jackson.annotation.JsonSubTypes.Type;
import com.fasterxml.jackson.annotation.JsonTypeInfo;

@JsonTypeInfo(use = JsonTypeInfo.Id.NAME, include = JsonTypeInfo.As.PROPERTY, property = "type")
public abstract class VisSource {
    private String name;
    private boolean skip;

    public String getName() {
        return name;
    }

    public boolean isSkip() {
        return skip;
    }

    public void load(Dataset dataset) {
        if (!skip) {
            loadInternal(dataset);
        }
    }

    abstract void loadInternal(Dataset dataset);
}
