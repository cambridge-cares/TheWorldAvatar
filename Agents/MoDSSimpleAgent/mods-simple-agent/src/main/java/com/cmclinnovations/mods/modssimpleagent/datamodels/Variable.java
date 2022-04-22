package com.cmclinnovations.mods.modssimpleagent.datamodels;

import com.fasterxml.jackson.annotation.JsonIgnore;
public class Variable {

    public static final String SUBTYPE_PREFIX = "subtype_";

    private String name;
    private String type;
    private String objective;

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


}
