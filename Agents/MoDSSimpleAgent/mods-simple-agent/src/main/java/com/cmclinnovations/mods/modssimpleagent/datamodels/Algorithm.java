package com.cmclinnovations.mods.modssimpleagent.datamodels;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;

public class Algorithm {

    private String name;
    private String type;
    @JsonInclude(Include.NON_NULL)
    private List<Variable> variables;
    @JsonInclude(Include.NON_NULL)
    private int maxNumberOfResults = Integer.MAX_VALUE;
    @JsonInclude(Include.NON_NULL)
    private String surrogateToLoad;
    @JsonInclude(Include.NON_NULL)
    private Boolean saveSurrogate;

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getType() {
        return type;
    }

    public void setType(String type) {
        this.type = type;
    }

    public List<Variable> getVariables() {
        return variables;
    }

    public void setVariables(List<Variable> variables) {
        this.variables = variables;
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
