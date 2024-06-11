package com.cmclinnovations.stack.clients.gdal;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.fasterxml.jackson.annotation.JsonProperty;

public abstract class GDALOptions<T extends GDALOptions<T>> extends CommonOptions<T> {

    @JsonProperty
    protected final Map<String, String> creationOptions = new HashMap<>();

    protected GDALOptions(String command) {
        super(command);
    }

    public T addCreationOption(String name, String value) {
        creationOptions.put(name, value);
        return (T) this;
    }

    String[] appendToArgs(String... args) {
        List<String> allArgs = appendCommonToArgs(args);

        creationOptions.forEach((name, value) -> addKeyValuePair(allArgs, "-co", name, value));

        return allArgs.toArray(args);
    }

}
