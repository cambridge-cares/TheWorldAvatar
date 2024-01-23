package com.cmclinnovations.mods.modssimpleagent.datamodels;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonPropertyOrder;

@JsonPropertyOrder({ "variable_name", "minimum", "maximum", "mean", "scaling" })
public record InputMetaDataRow(@JsonProperty("variable_name") String varName, Double minimum, Double maximum,
        Double mean, String scaling) {
}