package com.cmclinnovations.mods.modssimpleagent.datamodels;

public class InputMetaDataRow {
    private final String varName;
    private final Double minimum;
    private final Double maximum;
    private final Double mean;
    private final String scaling;
    
    public InputMetaDataRow(String varName, Double minimum, Double maximum, Double mean, String scaling) {
        this.varName = varName;
        this.minimum = minimum;
        this.maximum = maximum;
        this.mean = mean;
        this.scaling = scaling;
    }
    
    public String[] toWritableStrings() {
        return new String[] {varName, minimum.toString(), maximum.toString(), mean.toString(),scaling};
    }

    public String getVarName() {
        return varName;
    }
    public Double getMinimum() {
        return minimum;
    }
    public Double getMaximum() {
        return maximum;
    }
    public Double getMean() {
        return mean;
    }
    public String getScaling() {
        return scaling;
    }

    
}
