package com.cmclinnovations.mods.modssimpleagent.datamodels;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonSetter;
import com.fasterxml.jackson.annotation.Nulls;

public class Algorithm {

    private final String name;
    private final String type;
    private final List<Variable> variables;
    @JsonSetter(nulls = Nulls.SKIP)
    private int maxNumberOfResults = Integer.MAX_VALUE;
    private final String surrogateToLoad;
    private final Boolean saveSurrogate;

    private Algorithm() {
        this.name = null;
        this.type = null;
        this.variables = null;
        this.surrogateToLoad = null;
        this.saveSurrogate = null;
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
