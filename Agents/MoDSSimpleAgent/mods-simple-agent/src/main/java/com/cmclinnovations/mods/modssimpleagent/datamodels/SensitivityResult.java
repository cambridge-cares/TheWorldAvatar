package com.cmclinnovations.mods.modssimpleagent.datamodels;

import java.util.List;

public class SensitivityResult {
    private final String name;
    private final List<SensitivityLabels> labels;
    private final List<SensitivityValues> values;

    public SensitivityResult(String name, List<SensitivityLabels> labels, List<SensitivityValues> values) {
        this.labels = labels;
        this.values = values;
        this.name = name;
    }

    public List<SensitivityLabels> getLabels() {
        return labels;
    }

    public List<SensitivityValues> getValues() {
        return values;
    }

    public String getName() {
        return name;
    }

}
