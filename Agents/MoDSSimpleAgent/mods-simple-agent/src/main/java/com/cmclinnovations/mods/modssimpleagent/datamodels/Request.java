package com.cmclinnovations.mods.modssimpleagent.datamodels;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;
import com.fasterxml.jackson.annotation.JsonProperty;

public class Request {

    @JsonInclude(Include.NON_NULL)
    private final String jobID;
    @JsonProperty("SimulationType")
    private final String simulationType;
    @JsonProperty("Algorithms")
    @JsonInclude(Include.NON_NULL)
    private final List<Algorithm> algorithms;
    @JsonProperty("Inputs")
    @JsonInclude(Include.NON_NULL)
    private final Data inputs;
    @JsonProperty("Outputs")
    @JsonInclude(Include.NON_NULL)
    private Data outputs;
    @JsonProperty("Sensitivities")
    @JsonInclude(Include.NON_NULL)
    private List<SensitivityResult> sensitivities;

    private Request() {
        this(null, null);
    }

    public Request(String jobID, String simulationType) {
        this.jobID = jobID;
        this.simulationType = simulationType;
        this.algorithms = null;
        this.inputs = null;
        this.outputs = null;
        this.sensitivities = null;
    }

    public String getJobID() {
        return jobID;
    }

    public String getSimulationType() {
        return simulationType;
    }

    public List<Algorithm> getAlgorithms() {
        return algorithms;
    }

    public Data getInputs() {
        return inputs;
    }

    public Data getOutputs() {
        return outputs;
    }

    public void setOutputs(Data outputs) {
        this.outputs = outputs;
    }

    public List<SensitivityResult> getSensitivities() {
        return sensitivities;
    }

    public void setSensitivities(List<SensitivityResult> sensitivities) {
        this.sensitivities = sensitivities;
    }

    public Algorithm getAlgorithmOfType(String algType) {
        return getAlgorithms().stream().filter(alg -> alg.getType().equals(algType)).findFirst().orElseThrow();
    }

    public String getSurrogateToLoad() {
        return getAlgorithms().stream().map(Algorithm::getSurrogateToLoad).filter(surr -> surr != null).findFirst()
                .orElse(null);
    }
}
