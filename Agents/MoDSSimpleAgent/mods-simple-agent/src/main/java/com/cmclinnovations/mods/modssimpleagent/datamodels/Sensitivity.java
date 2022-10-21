package com.cmclinnovations.mods.modssimpleagent.datamodels;

import java.util.List;

public class Sensitivity {
    private final String name;
    private final List<SensitivityLables> lables;
    private final List<SensitivityValues> values;

    public Sensitivity(String name, List<SensitivityLables> lables, List<SensitivityValues> values) {
        this.lables = lables;
        this.values = values;
        this.name = name;
    }

    public List<SensitivityLables> getLables() {
        return lables;
    }

    public List<SensitivityValues> getValues() {
        return values;
    }

    public String getName() {
        return name;
    }
    
}
