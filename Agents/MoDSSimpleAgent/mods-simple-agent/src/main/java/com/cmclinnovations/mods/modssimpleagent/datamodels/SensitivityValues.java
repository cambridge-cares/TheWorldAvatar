package com.cmclinnovations.mods.modssimpleagent.datamodels;

import java.util.List;

public class SensitivityValues {
    private final int order;
    private final List<Double> values;

    public SensitivityValues(int order, List<Double> values) {
        this.order = order;
        this.values = values;
    }

    public int getOrder() {
        return order;
    }

    public List<Double> getValues() {
        return values;
    }

}
