package com.cmclinnovations.stack.clients.core.datasets;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
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
        return description.map(this::handleFileValues).orElse(getName());
    }

    public final String handleFileValues(String value) {
        if (null != value && value.startsWith("@")) {
            String file = value.substring(1);
            try {
                value = Files.readString(Path.of(file));
            } catch (IOException ex) {
                throw new RuntimeException(
                        "Failed to read SQL file '" + Path.of(file).toAbsolutePath().toString()
                                + "' for '" + getName() + "'.",
                        ex);
            }
        }
        return value;
    }
}
