package com.cmclinnovations.stack.clients.core;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class Options {
    private final Map<String, Option> optionsMap = new HashMap<>();
    private final List<String> flags = new ArrayList<>();

    public Options() {
    }

    public List<String> getOptionsList() {
        List<String> optionsList = new ArrayList<>();
        optionsList.addAll(flags);
        optionsMap.forEach((key, value) -> {
            optionsList.add(key);
            optionsList.addAll(value.getOptionList());
        });
        return optionsList;
    }
}
