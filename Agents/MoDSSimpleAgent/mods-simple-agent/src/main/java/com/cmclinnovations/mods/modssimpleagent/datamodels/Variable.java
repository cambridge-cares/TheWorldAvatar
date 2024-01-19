package com.cmclinnovations.mods.modssimpleagent.datamodels;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;

public record Variable(String name, String type, @JsonInclude(Include.NON_NULL) String objective,
        @JsonInclude(Include.NON_NULL) Double minimum, @JsonInclude(Include.NON_NULL) Double maximum,
        @JsonInclude(Include.NON_NULL) Double weight) {

    public static final String SUBTYPE_PREFIX = "subtype_";

    @JsonIgnore
    public String getSubtype() {
        return SUBTYPE_PREFIX + name;
    }
}
