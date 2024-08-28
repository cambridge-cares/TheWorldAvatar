package com.cmclinnovations.stack.clients.core;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.fasterxml.jackson.annotation.JsonProperty;

public class Options {
    @JsonProperty("options")
    private final Map<String, Option> argOptions = new HashMap<>();
    private final List<String> flags = new ArrayList<>();

    public Options() {
    }

    public List<String> getOptionsList() {
        List<String> optionsList = new ArrayList<>();
        optionsList.addAll(flags);
        argOptions.forEach((key, value) -> {
            optionsList.add(key);
            optionsList.addAll(value.getOptionList());
        });
        return optionsList;
    }

    public Map<String, Option> getArgOptions() {
        return argOptions;
    }

    public List<String> getFlags() {
        return flags;
    }
}
