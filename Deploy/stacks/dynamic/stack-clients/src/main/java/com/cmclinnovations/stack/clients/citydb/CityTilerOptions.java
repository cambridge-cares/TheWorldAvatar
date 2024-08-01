package com.cmclinnovations.stack.clients.citydb;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.JsonNode;

public class CityTilerOptions {

    @JsonProperty
    private final Map<String, List<String>> options = new HashMap<>();

    @JsonProperty
    private final CityTilerColours colours = new CityTilerColours();

    public JsonNode getColours() {
        return colours.getJson();
    }

    String[] appendOtherArgs(String[] specArgs, String... baseArgs) {
        List<String> allArgs = new ArrayList<>(List.of(baseArgs));

        options.forEach((option, values) -> {
            allArgs.add(option);
            values.stream().collect(Collectors.toCollection(() -> allArgs));
        });
        allArgs.addAll(List.of(specArgs));
        return allArgs.toArray(baseArgs);
    }

}
