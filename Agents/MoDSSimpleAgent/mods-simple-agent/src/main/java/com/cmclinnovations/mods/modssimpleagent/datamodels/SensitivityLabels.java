package com.cmclinnovations.mods.modssimpleagent.datamodels;

import java.util.List;

public class SensitivityLabels {
    private final int order;
    private final List<String> values;

    public SensitivityLabels(int order, List<String> values) {
        this.order = order;
        this.values = values;
    }

    public int getOrder() {
        return order;
    }

    public List<String> getValues() {
        return values;
    }

}
