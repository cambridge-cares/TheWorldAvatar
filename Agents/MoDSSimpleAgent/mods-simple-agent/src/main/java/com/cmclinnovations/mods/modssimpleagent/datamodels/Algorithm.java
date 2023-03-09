package com.cmclinnovations.mods.modssimpleagent.datamodels;

import java.util.List;

public class Algorithm {

    private String name;
    private String type;
    private List<Variable> variables;
    private int maxNumberOfResults = Integer.MAX_VALUE;
    private Boolean saveSurrogate;
    private String surrogateToLoad;

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

    public Boolean getSaveSurrogate() {
        return saveSurrogate;
    }

    public void setSaveSurrogate(Boolean saveSurrogate) {
        this.saveSurrogate = saveSurrogate;
    }

    public String getSurrogateToLoad() {
        return surrogateToLoad;
    }

    public void setSurrogateToLoad(String surrogateToLoad) {
        this.surrogateToLoad = surrogateToLoad;
    }

}
