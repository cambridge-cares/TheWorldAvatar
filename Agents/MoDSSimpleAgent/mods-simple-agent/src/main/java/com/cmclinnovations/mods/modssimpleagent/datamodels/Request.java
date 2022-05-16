package com.cmclinnovations.mods.modssimpleagent.datamodels;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;
import com.fasterxml.jackson.annotation.JsonProperty;

public class Request {

    @JsonInclude(Include.NON_NULL)
    String jobID;
    @JsonProperty("SimulationType")
    private String simulationType;
    @JsonProperty("Algorithms")
    @JsonInclude(Include.NON_NULL)
    private List<Algorithm> algorithms;
    @JsonProperty("Inputs")
    @JsonInclude(Include.NON_NULL)
    private Data inputs;
    @JsonProperty("Outputs")
    @JsonInclude(Include.NON_NULL)
    private Data outputs;

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

    public Data getOutputs() {
        return outputs;
    }

    public void setOutputs(Data outputs) {
        this.outputs = outputs;
    }
}
