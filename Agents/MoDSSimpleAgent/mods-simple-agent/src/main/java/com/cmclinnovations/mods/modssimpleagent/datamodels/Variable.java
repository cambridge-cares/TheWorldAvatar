package com.cmclinnovations.mods.modssimpleagent.datamodels;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;

public class Variable {

    public static final String SUBTYPE_PREFIX = "subtype_";

    private final String name;
    private final String type;
    @JsonInclude(Include.NON_NULL)
    private final String objective;
    @JsonInclude(Include.NON_NULL)
    private final Double minimum;
    @JsonInclude(Include.NON_NULL)
    private final Double maximum;
    @JsonInclude(Include.NON_NULL)
    private final Double weight;

    public Variable() {
        this.name = null;
        this.type = null;
        this.objective = null;
        this.minimum = null;
        this.maximum = null;
        this.weight = null;
    }

    public String getName() {
        return name;
    }

    @JsonIgnore
    public String getSubtype() {
        return SUBTYPE_PREFIX + name;
    }

    public String getType() {
        return type;
    }

    public String getObjective() {
        return objective;
    }

    public Double getMinimum() {
        return minimum;
    }

    public Double getMaximum() {
        return maximum;
    }

    public Double getWeight() {
        return weight;
    }

}
