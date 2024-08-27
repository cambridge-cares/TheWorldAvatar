package com.cmclinnovations.stack.clients.geoserver;

import java.nio.file.Path;

import javax.annotation.Nonnull;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

public class GeoServerStyle {

    private final String name;
    private final Path file;

    @JsonCreator
    private GeoServerStyle(
            @JsonProperty(value = "name", required = true) @Nonnull String name,
            @JsonProperty(value = "file", required = true) @Nonnull Path file) {
        this.name = name;
        this.file = file;
    }

    public String getName() {
        return name;
    }

    public Path getFile() {
        return file;
    }

}
