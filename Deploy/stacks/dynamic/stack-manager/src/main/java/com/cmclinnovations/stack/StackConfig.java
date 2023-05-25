package com.cmclinnovations.stack;

import java.util.Collections;
import java.util.EnumMap;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonProperty;

public class StackConfig {

    private enum Selector {
        @JsonProperty("includes")
        INCLUDES,
        @JsonProperty("excludes")
        EXCLUDES;
    }

    @JsonProperty("services")
    private final EnumMap<Selector, List<String>> serviceSelectors = new EnumMap<>(Selector.class);

    List<String> getIncludedServices() {
        return serviceSelectors.getOrDefault(Selector.INCLUDES, Collections.emptyList());
    }

    List<String> getExcludedServices() {
        return serviceSelectors.getOrDefault(Selector.EXCLUDES, Collections.emptyList());
    }
}
