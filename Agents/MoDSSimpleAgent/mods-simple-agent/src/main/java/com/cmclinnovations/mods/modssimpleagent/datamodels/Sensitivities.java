package com.cmclinnovations.mods.modssimpleagent.datamodels;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonValue;

public class Sensitivities {

    @JsonValue
    private final List<Sensitivity> sensitivityList;

    public Sensitivities(List<Sensitivity> sensitivityList) {
        this.sensitivityList = sensitivityList;
    }

    public List<Sensitivity> getSensitivities() {
        return sensitivityList;
    }

}
