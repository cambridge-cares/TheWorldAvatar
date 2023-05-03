package com.cmclinnovations.mods.modssimpleagent.datamodels;

public record InputMetaDataRow(String varName, Double minimum, Double maximum, Double mean, String scaling) {
    public String[] toWritableStrings() {
        return new String[] { varName, minimum.toString(), maximum.toString(), mean.toString(), scaling };
    }
}
