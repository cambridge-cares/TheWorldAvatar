package com.cmclinnovations.mods.modssimpleagent.datamodels;

import java.util.List;
import java.util.Objects;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonInclude.Include;

import lombok.Builder;

<<<<<<< HEAD
@JsonInclude(Include.NON_NULL)
@Builder(toBuilder = true)
public record Request(String jobID, @JsonProperty("SimulationType") String simulationType,
        @JsonProperty("Algorithms") List<Algorithm> algorithms, @JsonProperty("Inputs") Data inputs,
        @JsonProperty("Outputs") Data outputs, @JsonProperty("Sensitivities") List<SensitivityResult> sensitivities) {

    public Request(String jobID, String simulationType) {
        this(jobID, simulationType, null, null, null, null);
    }

    @JsonIgnore
    public Algorithm getAlgorithmOfType(String algType) {
        return algorithms.stream().filter(alg -> alg.type().equals(algType)).findFirst().orElseThrow();
    }

    @JsonIgnore
    public String getSurrogateToLoad() {
        return algorithms.stream().map(Algorithm::surrogateToLoad).filter(Objects::nonNull).findFirst().orElse(null);
=======
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
>>>>>>> parent of 4fae184ea3 (Merge branch 'main' of https://github.com/cambridge-cares/TheWorldAvatar into main)
    }
}
