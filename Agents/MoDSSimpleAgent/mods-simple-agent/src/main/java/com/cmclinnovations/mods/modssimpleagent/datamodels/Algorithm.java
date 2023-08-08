package com.cmclinnovations.mods.modssimpleagent.datamodels;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;

public class Algorithm {

    private final String name;
    private final String type;
    @JsonInclude(Include.NON_NULL)
    private final List<Variable> variables;
    @JsonInclude(Include.NON_NULL)
    private final int maxNumberOfResults;
    @JsonInclude(Include.NON_NULL)
    private final String surrogateToLoad;
    @JsonInclude(Include.NON_NULL)
    private final Boolean saveSurrogate;

    private Algorithm() {
        this.name = null;
        this.type = null;
        this.variables = null;
        this.surrogateToLoad = null;
        this.saveSurrogate = null;
        this.maxNumberOfResults = Integer.MAX_VALUE;
    }

    public String getName() {
        return name;
    }

    public String getType() {
        return type;
    }

    public List<Variable> getVariables() {
        return variables;
    }

    public int getMaxNumberOfResults() {
        return maxNumberOfResults;
    }

    public String getSurrogateToLoad() {
        return surrogateToLoad;
    }

    public Boolean getSaveSurrogate() {
        return saveSurrogate;
    }

}
