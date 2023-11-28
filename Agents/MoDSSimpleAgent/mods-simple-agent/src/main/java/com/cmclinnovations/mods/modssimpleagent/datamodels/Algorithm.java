package com.cmclinnovations.mods.modssimpleagent.datamodels;

import java.util.List;

<<<<<<< HEAD
public record Algorithm(String name, String type, List<Variable> variables, Integer maxNumberOfResults,
        String surrogateToLoad, Boolean saveSurrogate) {

    public Algorithm(String name, String type, List<Variable> variables, Integer maxNumberOfResults,
            String surrogateToLoad, Boolean saveSurrogate) {
        this.name = name;
        this.type = type;
        this.variables = variables;
        this.maxNumberOfResults = maxNumberOfResults == null ? Integer.MAX_VALUE : maxNumberOfResults;
        this.surrogateToLoad = surrogateToLoad;
        this.saveSurrogate = saveSurrogate;
    }
}
=======
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
>>>>>>> parent of 4fae184ea3 (Merge branch 'main' of https://github.com/cambridge-cares/TheWorldAvatar into main)
