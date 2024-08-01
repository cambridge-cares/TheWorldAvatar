package com.cmclinnovations.stack.clients.core.datasets;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Optional;

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
        return description.map(AbstractDataObject::handleFileValues).orElse(getName());
    }

    public static final String handleFileValues(String value) {
        if (null != value && value.startsWith("@")) {
            String file = value.substring(1);
            try {
                value = Files.readString(Path.of(file));
            } catch (IOException ex) {
                throw new RuntimeException(
                        "Failed to read file '" + Path.of(file).toAbsolutePath().toString() + "'.", ex);
            }
        }
        return value;
    }
}
