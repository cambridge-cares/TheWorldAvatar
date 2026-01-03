package com.cmclinnovations.mods.modssimpleagent.datamodels;

import java.util.List;

public record Algorithm(String name, String type, List<Variable> variables, Integer maxNumberOfResults,
        String surrogateToLoad, Boolean saveSurrogate, String modelToLoad) {

    public Algorithm(String name, String type, List<Variable> variables, Integer maxNumberOfResults,
            String surrogateToLoad, Boolean saveSurrogate, String modelToLoad) {
        this.name = name;
        this.type = type;
        this.variables = variables;
        this.maxNumberOfResults = maxNumberOfResults == null ? Integer.MAX_VALUE : maxNumberOfResults;
        this.surrogateToLoad = surrogateToLoad;
        this.saveSurrogate = saveSurrogate;
        this.modelToLoad = modelToLoad;
    }
}