package com.cmclinnovations.mods.modssimpleagent.datamodels;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonProperty;

public class Request {

    String jobID;
    @JsonProperty("SimulationType")
    private String simulationType;
    @JsonProperty("Settings")
    private List<Setting> settings;
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

    public List<Setting> getSettings() {
        return settings;
    }

    public void setSettings(List<Setting> settings) {
        this.settings = settings;
    }

    public Data getInputs() {
        return inputs;
    }

    public void setInputs(Data inputs) {
        this.inputs = inputs;
    }

}
