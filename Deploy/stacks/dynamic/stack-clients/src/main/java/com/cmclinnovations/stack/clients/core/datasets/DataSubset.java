package com.cmclinnovations.stack.clients.core.datasets;

import java.nio.file.Path;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonSubTypes;
import com.fasterxml.jackson.annotation.JsonSubTypes.Type;
import com.fasterxml.jackson.annotation.JsonTypeInfo;

@JsonTypeInfo(use = JsonTypeInfo.Id.NAME, include = JsonTypeInfo.As.PROPERTY, property = "type")
@JsonSubTypes({ @Type(Tabular.class), @Type(Vector.class), @Type(Raster.class), @Type(Triples.class) })
public abstract class DataSubset {

    private String name;
    @JsonProperty
    private String subdirectory;

    @JsonProperty
    private boolean skip;

    public String getName() {
        return name;
    }

    public Path getDirectory(Path parentDirectory) {
        return null != subdirectory ? parentDirectory.resolve(subdirectory) : parentDirectory;
    }

    public void load(Dataset dataset) {
        if (!skip) {
            loadInternal(dataset);
        }
    }

    abstract void loadInternal(Dataset dataset);
}
