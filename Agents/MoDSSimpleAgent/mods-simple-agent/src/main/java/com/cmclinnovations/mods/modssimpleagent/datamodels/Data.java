package com.cmclinnovations.mods.modssimpleagent.datamodels;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonValue;

public class Data {

    @JsonValue
    private List<VarValues> values;

    @JsonCreator
    public Data(List<VarValues> values) {
        this.values = values;
    }

    public List<VarValues> getValues() {
        return values;
    }

    public void setValues(List<VarValues> values) {
        this.values = values;
    }
}
