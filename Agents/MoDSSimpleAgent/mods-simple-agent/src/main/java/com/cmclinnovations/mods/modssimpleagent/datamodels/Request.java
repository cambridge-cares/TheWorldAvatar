package com.cmclinnovations.mods.modssimpleagent.datamodels;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonProperty;

public class Request {

    String jobID;
    @JsonProperty("SimulationType")
    private String simulationType;
    @JsonProperty("Algorithms")
    private List<Algorithm> algorithms;
    @JsonProperty("Inputs")
    private Data inputs;

    public String getJobID() {
        return jobID;
    }

    public void setJobID(String jobID) {
        this.jobID = jobID;
    }

    public String getSimulationType() {
        return simulationType;
    }

    public void setSimulationType(String simulationType) {
        this.simulationType = simulationType;
    }

    public List<Algorithm> getAlgorithms() {
        return algorithms;
    }

    public void setAlgorithms(List<Algorithm> algorithms) {
        this.algorithms = algorithms;
    }

    public Data getInputs() {
        return inputs;
    }

    public void setInputs(Data inputs) {
        this.inputs = inputs;
    }

}
