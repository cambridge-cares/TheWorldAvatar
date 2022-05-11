package com.cmclinnovations.mods.modssimpleagent.datamodels;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;

public class Variable {

    public static final String SUBTYPE_PREFIX = "subtype_";

    private String name;
    private String type;
    @JsonInclude(Include.NON_NULL)
    private String objective;
    @JsonInclude(Include.NON_NULL)
    private Double minimum;
    @JsonInclude(Include.NON_NULL)
    private Double maximum;
    @JsonInclude(Include.NON_NULL)
    private Double weight;

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    @JsonIgnore
    public String getSubtype() {
        return SUBTYPE_PREFIX + name;
    }

    public String getType() {
        return type;
    }

    public void setType(String type) {
        this.type = type;
    }

    public String getObjective() {
        return objective;
    }

    public void setObjective(String objective) {
        this.objective = objective;
    }

    public Double getMinimum() {
        return minimum;
    }

    public void setMinimum(Double minimum) {
        this.minimum = minimum;
    }

    public Double getMaximum() {
        return maximum;
    }

    public void setMaximum(Double maximum) {
        this.maximum = maximum;
    }

    public Double getWeight() {
        return weight;
    }

    public void setWeight(Double weight) {
        this.weight = weight;
    }

}
